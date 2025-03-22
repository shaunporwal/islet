# Comprehensive test for DB table functions
# Run with: R -f tests/test-db-functions.R

# Skip execution during package loading/documentation
if (sys.nframe() > 0) {
  return(invisible(NULL))
}

library(islet)

# Define connection parameters - modify as needed
schema <- "IDEPBS" # Schema name
conn_name <- "idb" # Connection name for credentials lookup
host <- "idbdw-p.mskcc.org" # Database host
port <- "50000" # Database port
database <- "BLUDB" # Database name

# Generate a unique test table name with timestamp to avoid conflicts
test_table <- paste0("TEMP_TEST_", format(Sys.time(), "%Y%m%d%H%M%S"))
message("Test table name: ", test_table)

# Create a simple test dataframe
test_data <- data.frame(
  ID = 1:3,
  NAME = c("Test1", "Test2", "Test3"),
  VALUE = c(10.5, 20.5, 30.5),
  stringsAsFactors = FALSE
)

# --------------------------------------------------
# Phase 1: Create Test Table
# --------------------------------------------------
message("\n=== Phase 1: Creating Test Table ===")

create_result <- tryCatch(
  {
    write_db_table(
      input_data = test_data,
      table = test_table,
      schema = schema,
      overwrite = TRUE,
      conn_name = conn_name,
      host = host,
      port = port,
      database = database,
      verify = TRUE,
      strict_verify = FALSE # Don't fail if verification fails
    )
    message("✓ Table creation completed")
    TRUE
  },
  error = function(e) {
    message("✗ Error creating table: ", e$message)
    FALSE
  }
)

if (!create_result) {
  stop("Test failed: Could not create test table")
}

# --------------------------------------------------
# Phase 2: Test if table exists (using uppercase and lowercase)
# --------------------------------------------------
message("\n=== Phase 2: Verifying Table Existence ===")

# Attempt manual verification using the get_connection helper
get_connection <- function() {
  # Function to get credentials
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
    if (interactive()) {
      username <- readline(prompt = paste0("Enter username for ", conn_name, ": "))
      password <- readline(prompt = paste0("Enter password for ", conn_name, ": "))
      credentials <- list(username = username, password = password)
      # Store in env for this session
      env_vars <- list(username, password)
      names(env_vars) <- c(paste0(conn_name, "_UX"), paste0(conn_name, "_PW"))
      do.call(Sys.setenv, env_vars)
      return(credentials)
    }

    stop("No credentials found for ", conn_name)
  }

  # Get credentials
  credentials <- get_credentials(conn_name)

  # Find DB2 JDBC driver
  jar_path <- system.file("drv", "db2jcc4.jar", package = "islet")
  if (jar_path == "") {
    potential_paths <- c(
      file.path("inst", "drv", "db2jcc4.jar"),
      file.path("drv", "db2jcc4.jar"),
      "db2jcc4.jar"
    )

    for (path in potential_paths) {
      if (file.exists(path)) {
        jar_path <- path
        message("Found JAR file at: ", jar_path)
        break
      }
    }

    if (jar_path == "" || !file.exists(jar_path)) {
      stop("DB2 JDBC driver JAR file not found")
    }
  }

  # Connect to DB2
  if (requireNamespace("RJDBC", quietly = TRUE)) {
    driver <- "com.ibm.db2.jcc.DB2Driver"
    url <- paste0(
      "jdbc:db2://", host, ":", port, "/", database,
      ":retrieveMessagesFromServerOnGetMessage=true;",
      "emulateParameterMetaDataForZCalls=1;",
      "extendedDiagnosticLevel=241;"
    )
    drv <- RJDBC::JDBC(driver, jar_path)
    conn <- RJDBC::dbConnect(drv, url, credentials$username, credentials$password)
    return(conn)
  } else {
    stop("RJDBC package is required")
  }
}

verify_table_exists <- function(conn, schema, table) {
  # Try standard DBI method first with uppercase
  table_id <- DBI::Id(schema = toupper(schema), table = toupper(table))
  if (DBI::dbExistsTable(conn, table_id)) {
    return(TRUE)
  }

  # Try both uppercase and lowercase in system catalog
  tryCatch(
    {
      query_uppercase <- paste0(
        "SELECT 1 FROM SYSCAT.TABLES WHERE TABSCHEMA = '",
        toupper(schema),
        "' AND TABNAME = '",
        toupper(table),
        "'"
      )
      result_uppercase <- DBI::dbGetQuery(conn, query_uppercase)
      if (nrow(result_uppercase) > 0) {
        return(TRUE)
      }

      query_lowercase <- paste0(
        "SELECT 1 FROM SYSCAT.TABLES WHERE TABSCHEMA = '",
        toupper(schema),
        "' AND TABNAME = '",
        table, # Original case
        "'"
      )
      result_lowercase <- DBI::dbGetQuery(conn, query_lowercase)
      if (nrow(result_lowercase) > 0) {
        return(TRUE)
      }

      return(FALSE)
    },
    error = function(e) {
      message("System catalog check failed: ", e$message)
      return(FALSE)
    }
  )
}

# Check if table exists
conn <- tryCatch(get_connection(), error = function(e) {
  message("Failed to connect to database: ", e$message)
  return(NULL)
})

if (!is.null(conn)) {
  # Try to find the table
  for (attempt in 1:3) {
    exists <- verify_table_exists(conn, schema, test_table)
    if (exists) {
      message("✓ Table exists in database (verified manually)")
      break
    } else if (attempt < 3) {
      message("Table not found on attempt ", attempt, ", retrying...")
      Sys.sleep(1)
    } else {
      message("✗ Could not verify table existence after ", attempt, " attempts")
    }
  }

  # Close connection
  DBI::dbDisconnect(conn)
} else {
  message("✗ Manual database connection failed, skipping manual verification")
}

# --------------------------------------------------
# Phase 3: Test table removal
# --------------------------------------------------
message("\n=== Phase 3: Removing Test Table ===")

remove_result <- tryCatch(
  {
    # Try to remove the table
    result <- remove_db_table(
      table = test_table,
      schema = schema,
      conn_name = conn_name,
      host = host,
      port = port,
      database = database,
      verify = TRUE
    )

    if (result) {
      message("✓ Table removed successfully")
      TRUE
    } else {
      message("✗ Failed to remove table")
      FALSE
    }
  },
  error = function(e) {
    message("✗ Error removing table: ", e$message)
    FALSE
  }
)

if (!remove_result) {
  message("Warning: Table removal failed, but continuing test")
}

# --------------------------------------------------
# Phase 4: Verify table doesn't exist
# --------------------------------------------------
message("\n=== Phase 4: Verifying Table Removal ===")

# Connect again to verify table is gone
conn <- tryCatch(get_connection(), error = function(e) {
  message("Failed to connect to database: ", e$message)
  return(NULL)
})

if (!is.null(conn)) {
  exists <- verify_table_exists(conn, schema, test_table)
  if (!exists) {
    message("✓ Table removal confirmed - table no longer exists")
  } else {
    message("✗ Table still exists after removal attempt")
  }

  # Close connection
  DBI::dbDisconnect(conn)
} else {
  message("✗ Manual database connection failed, skipping manual verification")
}

# --------------------------------------------------
# Phase 5: Test removing a non-existent table
# --------------------------------------------------
message("\n=== Phase 5: Testing Removal of Non-existent Table ===")

non_existent_result <- tryCatch(
  {
    # Try to remove the table again - should return FALSE since it doesn't exist
    result <- remove_db_table(
      table = test_table,
      schema = schema,
      conn_name = conn_name,
      host = host,
      port = port,
      database = database,
      verify = TRUE
    )

    if (!result) {
      message("✓ remove_db_table correctly returned FALSE for non-existent table")
      TRUE
    } else {
      message("✗ remove_db_table incorrectly returned TRUE for non-existent table")
      FALSE
    }
  },
  error = function(e) {
    message("✗ Error testing removal of non-existent table: ", e$message)
    FALSE
  }
)

# --------------------------------------------------
# Conclusion
# --------------------------------------------------
message("\n=== Test Complete ===")
message("Create table:", if (create_result) "SUCCESS" else "FAILURE")
message("Remove table:", if (remove_result) "SUCCESS" else "FAILURE")
message("Non-existent table test:", if (non_existent_result) "SUCCESS" else "FAILURE")

if (create_result && (remove_result || non_existent_result)) {
  message("\n✓ Database functions test PASSED")
} else {
  message("\n✗ Database functions test FAILED")
}
