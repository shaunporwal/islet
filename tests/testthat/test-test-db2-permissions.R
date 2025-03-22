# Test script to diagnose DB2 connection and permissions issues
# Run with: R -f tests/test-db2-permissions.R

library(islet)
cat("Testing DB2 connection and permissions\n")

# Connection parameters
conn_params <- list(
  schema = "IDEPBS",
  conn_name = "idb",
  host = "idbdw-p.mskcc.org",
  port = "50000",
  database = "BLUDB"
)

# Function to get db connection
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
    cat("Connecting with JDBC driver using JAR at:", jar_path, "\n")
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

# Get connection
tryCatch(
  {
    cat("\nEstablishing DB2 connection...\n")
    conn <- get_connection()
    cat("✓ Connection established successfully\n")

    # Print connection details
    cat("\nConnection details:\n")
    cat("- Username:", Sys.getenv(paste0(conn_params$conn_name, "_UX")), "\n")
    cat("- Database:", conn_params$database, "\n")
    cat("- Schema:", conn_params$schema, "\n")

    # Try to list tables in the schema
    cat("\nAttempting to list tables in schema:", conn_params$schema, "\n")
    tryCatch(
      {
        tables_query <- paste0(
          "SELECT TABNAME FROM SYSCAT.TABLES WHERE TABSCHEMA = '",
          toupper(conn_params$schema),
          "' AND OWNER = '",
          toupper(Sys.getenv(paste0(conn_params$conn_name, "_UX"))),
          "' FETCH FIRST 5 ROWS ONLY"
        )

        result <- DBI::dbGetQuery(conn, tables_query)
        if (nrow(result) > 0) {
          cat("✓ Found", nrow(result), "tables owned by you in this schema:\n")
          cat(paste(" -", result$TABNAME), sep = "\n")
        } else {
          cat("✗ No tables found. This could indicate permission issues.\n")
        }
      },
      error = function(e) {
        cat("✗ Error listing tables:", e$message, "\n")
      }
    )

    # Try to create a test table
    test_table <- paste0("TEMP_TEST_", format(Sys.time(), "%Y%m%d%H%M%S"))
    cat("\nAttempting to create a simple test table:", test_table, "\n")

    tryCatch(
      {
        test_data <- data.frame(ID = 1:3, NAME = c("A", "B", "C"))
        table_id <- DBI::Id(schema = conn_params$schema, table = toupper(test_table))

        # Try to write the table directly with DBI
        DBI::dbWriteTable(conn, table_id, test_data, overwrite = TRUE)
        cat("✓ Test table created successfully using DBI::dbWriteTable\n")

        # Try to check if the table exists
        if (DBI::dbExistsTable(conn, table_id)) {
          cat("✓ Table existence verified with DBI::dbExistsTable\n")

          # Try to remove the table
          DBI::dbRemoveTable(conn, table_id)
          cat("✓ Test table removed successfully\n")

          # Verify removal
          if (!DBI::dbExistsTable(conn, table_id)) {
            cat("✓ Table removal verified\n")
          } else {
            cat("✗ Table still exists after removal attempt\n")
          }
        } else {
          cat("✗ Table does not exist immediately after creation\n")
        }
      },
      error = function(e) {
        cat("✗ Error creating/removing test table:", e$message, "\n")
      }
    )

    # Close connection
    DBI::dbDisconnect(conn)
    cat("\nConnection closed.\n")
  },
  error = function(e) {
    cat("✗ Error establishing connection:", e$message, "\n")
  }
)

cat("\nDiagnosis complete.\n")
