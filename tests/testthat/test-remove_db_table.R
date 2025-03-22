# Test script for remove_db_table function
# Run with: R -f tests/test-remove-db-table.R

# Ensure the package is loaded correctly
tryCatch(
  {
    library(islet)
    # Check that the functions are available
    if (!exists("write_db_table") || !exists("remove_db_table")) {
      stop("Functions write_db_table or remove_db_table not found in the package")
    }
    cat("✓ Package loaded successfully\n")
  },
  error = function(e) {
    cat("✗ Error loading package:", e$message, "\n")
    quit(status = 1)
  }
)

# Load additional packages needed for testing
if (!requireNamespace("DBI", quietly = TRUE)) {
  cat("✗ Required package DBI not available\n")
  quit(status = 1)
}

# Set up test table name with unique identifier to avoid conflicts
test_table <- paste0("TEMP_TEST_", format(Sys.time(), "%Y%m%d%H%M%S"))
cat("\nTest table name:", test_table, "\n")

# Connection parameters
conn_params <- list(
  schema = "IDEPBS",
  conn_name = "idb",
  host = "idbdw-p.mskcc.org",
  port = "50000",
  database = "BLUDB"
)

# Reusable function to establish connection
get_connection <- function() {
  # --- Credential Retrieval ---
  get_credentials <- function(conn_name) {
    # Check environment variables first
    env_username <- Sys.getenv(paste0(conn_name, "_UX"))
    env_password <- Sys.getenv(paste0(conn_name, "_PW"))
    if (env_password != "") {
      return(list(username = env_username, password = env_password))
    }

    # Fallback to keyring if available
    if (requireNamespace("keyring", quietly = TRUE) && keyring::has_keyring_support()) {
      keyring_service <- paste0("mskr_", conn_name)
      username <- tryCatch(keyring::key_list(service = keyring_service)$username[[1]], error = function(e) NULL)
      password <- tryCatch(keyring::key_get(service = keyring_service, username = username), error = function(e) NULL)
      if (!is.null(password)) {
        return(list(username = username, password = password))
      }
    }

    # If in RStudio, prompt for login
    if (.Platform$GUI == "RStudio" && interactive()) {
      username <- readline(prompt = paste0("Enter username for ", conn_name, ": "))
      password <- readline(prompt = paste0("Enter password for ", conn_name, ": "))
      credentials <- list(username = username, password = password)
      # Store in env for this session
      env_vars <- list(username, password)
      names(env_vars) <- c(paste0(conn_name, "_UX"), paste0(conn_name, "_PW"))
      do.call(Sys.setenv, env_vars)
      return(credentials)
    }

    # Fail if no credentials found in non-interactive mode
    stop(
      "No credentials found. Set them with:\n",
      paste0('Sys.setenv("', conn_name, '_UX" = "YOUR_USERNAME")\n'),
      paste0('Sys.setenv("', conn_name, '_PW" = "YOUR_PASSWORD")'),
      call. = FALSE
    )
  }

  # Get credentials
  credentials <- get_credentials(conn_params$conn_name)

  # Find JAR file
  jar_path <- system.file("drv", "db2jcc4.jar", package = "islet")

  # Connect to DB2
  if (requireNamespace("RJDBC", quietly = TRUE) && requireNamespace("rJava", quietly = TRUE)) {
    message("Connecting with JDBC driver using JAR at: ", jar_path)
    driver <- "com.ibm.db2.jcc.DB2Driver"
    url <- paste0(
      "jdbc:db2://", conn_params$host, ":", conn_params$port, "/", conn_params$database,
      ":retrieveMessagesFromServerOnGetMessage=true;",
      "emulateParameterMetaDataForZCalls=1;",
      "extendedDiagnosticLevel=241;"
    )
    drv <- RJDBC::JDBC(driver, jar_path)
    conn <- RJDBC::dbConnect(drv, url, credentials$username, credentials$password)
    return(conn)
  } else {
    stop("RJDBC or rJava packages not available")
  }
}

# Helper function to verify if table exists with retry mechanism
verify_table_exists <- function(conn, schema, table, max_attempts = 3) {
  # Try multiple methods to verify table existence with retries
  for (attempt in 1:max_attempts) {
    if (attempt > 1) {
      cat("Retry attempt", attempt, "to verify table existence\n")
      Sys.sleep(1) # Wait between attempts
    }

    # Method 1: DBI table exists check
    table_id <- DBI::Id(schema = toupper(schema), table = toupper(table))
    if (DBI::dbExistsTable(conn, table_id)) {
      return(TRUE)
    }

    # Method 2: Direct SQL query to system catalog
    tryCatch(
      {
        query <- paste0(
          "SELECT 1 FROM SYSCAT.TABLES WHERE TABSCHEMA = '",
          toupper(schema),
          "' AND TABNAME = '",
          toupper(table),
          "'"
        )
        result <- DBI::dbGetQuery(conn, query)
        if (nrow(result) > 0) {
          return(TRUE)
        }
      },
      error = function(e) {
        cat("System catalog check failed:", e$message, "\n")
      }
    )
  }

  return(FALSE)
}

# --- Phase 1: Create a test table ---
cat("\nCreating test table:", test_table, "\n")

# Create a simple data frame for the test
test_data <- data.frame(
  ID = 1:3,
  NAME = c("Test1", "Test2", "Test3"),
  VALUE = c(10.5, 20.1, 30.8)
)

# Create the table first
tryCatch(
  {
    write_db_table(
      input_data = test_data,
      table = test_table,
      schema = conn_params$schema,
      overwrite = TRUE,
      conn_name = conn_params$conn_name,
      host = conn_params$host,
      port = conn_params$port,
      database = conn_params$database,
      verify = TRUE,
      strict_verify = FALSE # Don't fail if verification fails
    )
    cat("✓ Test table creation function completed\n")

    # Independently verify the table was created
    conn <- get_connection()

    # Use our helper function with retry logic
    if (verify_table_exists(conn, conn_params$schema, test_table)) {
      cat("✓ Verified table exists in database\n")
    } else {
      cat("✗ Table creation failed - table does not exist in database\n")
      DBI::dbDisconnect(conn)
      quit(status = 1)
    }
    DBI::dbDisconnect(conn)
  },
  error = function(e) {
    cat("✗ Error creating test table:", e$message, "\n")
    quit(status = 1)
  }
)

# --- Phase 2: Test removing a table that exists ---
cat("\nTesting remove_db_table on existing table\n")

tryCatch(
  {
    result <- remove_db_table(
      table = test_table,
      schema = conn_params$schema,
      conn_name = conn_params$conn_name,
      host = conn_params$host,
      port = conn_params$port,
      database = conn_params$database,
      verify = TRUE # Ensure verification is enabled
    )

    if (result) {
      cat("✓ Successfully removed table", test_table, "\n")

      # Verify table is really gone
      conn <- get_connection()

      # Wait a moment for any pending transactions to complete
      Sys.sleep(1)

      # Check table is gone - should NOT exist
      if (!verify_table_exists(conn, conn_params$schema, test_table)) {
        cat("✓ Verified table no longer exists in database\n")
      } else {
        cat("✗ Table still exists in database even though remove_db_table returned TRUE\n")
        DBI::dbDisconnect(conn)
        quit(status = 1)
      }
      DBI::dbDisconnect(conn)
    } else {
      cat("✗ Function returned FALSE but should have returned TRUE\n")
      quit(status = 1)
    }
  },
  error = function(e) {
    cat("✗ Error removing test table:", e$message, "\n")
    quit(status = 1)
  }
)

# --- Phase 3: Test removing a table that doesn't exist ---
cat("\nTesting remove_db_table on non-existent table\n")

tryCatch(
  {
    result <- remove_db_table(
      table = test_table, # Same table name that we just removed
      schema = conn_params$schema,
      conn_name = conn_params$conn_name,
      host = conn_params$host,
      port = conn_params$port,
      database = conn_params$database,
      verify = TRUE
    )

    if (!result) {
      cat("✓ Correctly handled non-existent table (returned FALSE)\n")
    } else {
      cat("✗ Function returned TRUE but should have returned FALSE\n")
      quit(status = 1)
    }
  },
  error = function(e) {
    cat("✗ Error when testing with non-existent table:", e$message, "\n")
    quit(status = 1)
  }
)

cat("\nAll tests passed! The remove_db_table function is working correctly.\n")
