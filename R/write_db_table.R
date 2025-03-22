#' Write data to a DB2 database table
#'
#' @param input_data Data frame to write to database
#' @param table Target table name
#' @param schema Database schema
#' @param overwrite Whether to overwrite existing table
#' @param conn_name Connection name for credentials lookup
#' @param host Database host
#' @param port Database port
#' @param database Database name
#' @param jar_path Path to DB2 JDBC driver JAR file (optional)
#' @param verify Whether to verify table was created successfully
#' @param strict_verify If TRUE, will fail if verification fails; if FALSE, will return success anyway
#' @return TRUE on success
#' @import DBI
#' @importFrom RJDBC JDBC dbConnect
#' @importFrom odbc odbc odbcListDrivers
#' @importFrom keyring key_list key_get has_keyring_support
#' @importFrom rJava .jcall
#' @export
write_db_table <- function(
    input_data,
    table,
    schema,
    overwrite = TRUE,
    conn_name,
    host,
    port,
    database,
    jar_path = NULL,
    verify = TRUE,
    strict_verify = FALSE) {
  # Check for missing required parameters
  missing_params <- character(0)
  if (missing(input_data)) missing_params <- c(missing_params, "input_data")
  if (missing(table)) missing_params <- c(missing_params, "table")
  if (missing(schema)) missing_params <- c(missing_params, "schema")
  if (missing(conn_name)) missing_params <- c(missing_params, "conn_name")
  if (missing(host)) missing_params <- c(missing_params, "host")
  if (missing(port)) missing_params <- c(missing_params, "port")
  if (missing(database)) missing_params <- c(missing_params, "database")

  if (length(missing_params) > 0) {
    stop("The following required parameters are missing: ", paste(missing_params, collapse = ", "))
  }

  # Input validation
  if (!is.data.frame(input_data)) stop("input_data must be a data.frame")
  if (!is.character(table)) stop("table must be a character")
  if (!is.logical(overwrite)) stop("overwrite must be a logical")

  # Check or set jar_path
  if (is.null(jar_path)) {
    # Try to find the JAR file in the package
    jar_path <- system.file("drv", "db2jcc4.jar", package = "islet")

    # If not found in the package, look in the working directory
    if (jar_path == "") {
      potential_paths <- c(
        file.path("inst", "drv", "db2jcc4.jar"), # Source package structure
        file.path("drv", "db2jcc4.jar"), # Installed package structure
        "db2jcc4.jar" # Current directory
      )

      for (path in potential_paths) {
        if (file.exists(path)) {
          jar_path <- path
          message("Found JAR file at: ", jar_path)
          break
        }
      }
    }

    if (jar_path == "" || !file.exists(jar_path)) {
      stop("DB2 JDBC driver JAR file not found. Please specify the jar_path parameter with the correct path to db2jcc4.jar")
    }
  } else {
    # Verify the provided jar_path exists
    if (!file.exists(jar_path)) {
      stop("The specified JAR file does not exist at: ", jar_path)
    }
  }

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

    # If in RStudio, prompt for login (simplified; no external get_login())
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
  credentials <- get_credentials(conn_name)

  # --- DB2 Connection Setup ---
  connect_db2 <- function(conn_name, host, port, database, credentials, jar_path) {
    # Check for ODBC driver availability
    drivers <- if (requireNamespace("odbc", quietly = TRUE)) odbc::odbcListDrivers()$name else character(0)
    if (any(c("Db2", "IDB") %in% drivers)) {
      message("Using ODBC driver for connection")
      # Use ODBC with the first available driver
      driver_name <- intersect(c("Db2", "IDB"), drivers)[1]
      conn_string <- paste0(
        "DRIVER={", driver_name, "};",
        "Database=", database, ";",
        "Hostname=", host, ";",
        "Port=", port, ";",
        "PROTOCOL=TCPIP;",
        "UID=", credentials$username, ";",
        "PWD=", credentials$password, ";",
        "CURRENTAPPENSCH=UNICODE;",
        "RETCATALOGASCURRSERVER=0;",
        "ENCODING=UTF-8"
      )
      conn <- DBI::dbConnect(odbc::odbc(), conn_string, encoding = "UTF-8")
    } else if (requireNamespace("RJDBC", quietly = TRUE) && requireNamespace("rJava", quietly = TRUE)) {
      message("Using JDBC driver for connection with JAR at: ", jar_path)
      # Fallback to RJDBC if ODBC isn't available
      driver <- "com.ibm.db2.jcc.DB2Driver"
      url <- paste0(
        "jdbc:db2://", host, ":", port, "/", database,
        ":retrieveMessagesFromServerOnGetMessage=true;",
        "emulateParameterMetaDataForZCalls=1;",
        "extendedDiagnosticLevel=241;"
      )
      drv <- RJDBC::JDBC(driver, jar_path) # Use user-specified JAR path
      conn <- RJDBC::dbConnect(drv, url, credentials$username, credentials$password)
    } else {
      stop(
        "DB2 connection requires either odbc (with Db2/IDB driver) or RJDBC/rJava packages.\n",
        "Install with: install.packages(c('odbc', 'RJDBC', 'rJava'))",
        call. = FALSE
      )
    }
    return(conn)
  }

  # Establish connection
  conn <- connect_db2(conn_name, host, port, database, credentials, jar_path)

  # Disable automatic disconnection so we can verify the table was created
  auto_disconnect <- TRUE
  if (verify) {
    auto_disconnect <- FALSE
  } else {
    on.exit(DBI::dbDisconnect(conn)) # Ensure connection closes
  }

  # --- Table Writing ---
  table_id <- DBI::Id(schema = toupper(schema), table = toupper(table))

  if (overwrite && DBI::dbExistsTable(conn, table_id)) {
    message("overwrite = TRUE: The existing table will be dropped and replaced with new data")
    DBI::dbRemoveTable(conn, table_id)
  }

  # Try to create the table
  tryCatch(
    {
      DBI::dbWriteTable(conn, table_id, input_data, overwrite = overwrite)

      # For JDBC connections, try to explicitly commit changes - using trySendQuery which is safer
      if (inherits(conn, "JDBCConnection")) {
        tryCatch(
          {
            # Try different methods to commit the transaction
            trySendQuery <- function(conn, sql) {
              result <- NULL
              for (method in c("dbSendQuery", "dbSendStatement", "dbExecute")) {
                tryCatch(
                  {
                    if (method == "dbSendQuery") {
                      result <- DBI::dbSendQuery(conn, sql)
                      DBI::dbClearResult(result)
                      return(TRUE)
                    } else if (method == "dbSendStatement") {
                      result <- DBI::dbSendStatement(conn, sql)
                      DBI::dbClearResult(result)
                      return(TRUE)
                    } else {
                      DBI::dbExecute(conn, sql)
                      return(TRUE)
                    }
                  },
                  error = function(e) {
                    message(paste("Method", method, "failed:", e$message))
                    return(FALSE)
                  }
                )
              }
              # Direct JDBC call as last resort
              tryCatch(
                {
                  conn@jc$commit()
                  return(TRUE)
                },
                error = function(e) {
                  message("Direct JDBC commit failed:", e$message)
                  return(FALSE)
                }
              )
              return(FALSE)
            }

            commit_result <- trySendQuery(conn, "COMMIT")
            if (commit_result) {
              message("Transaction committed successfully")
            } else {
              message("Warning: Could not explicitly commit transaction, but operation may have succeeded")
            }
          },
          error = function(e) {
            message("Note: Could not explicitly commit transaction: ", e$message)
            message("This is often normal with JDBC connections - the table may still be created")
          }
        )
      }

      # Verify the table was created if required
      if (verify) {
        # Check for table existence with retry mechanism
        table_found <- FALSE
        for (attempt in 1:3) {
          Sys.sleep(0.5 * attempt) # Increasing wait times

          # Try direct SQL query first
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
                message("Table existence verified via system catalog: ", schema, ".", toupper(table))
                table_found <- TRUE
                break
              }
            },
            error = function(e) {
              message("System catalog check failed on attempt ", attempt, ": ", e$message)
            }
          )

          # Then try DBI method
          if (DBI::dbExistsTable(conn, table_id)) {
            message("Table existence verified with DBI::dbExistsTable: ", schema, ".", toupper(table))
            table_found <- TRUE
            break
          }

          if (attempt < 3) {
            message("Table not found on attempt ", attempt, ", retrying...")
          }
        }

        # Handle verification failure
        if (!table_found) {
          if (strict_verify) {
            stop("Table write operation appeared to succeed, but the table could not be verified")
          } else {
            message("Warning: Table creation couldn't be verified, but may have succeeded")
          }
        }

        # Manually close connection if we disabled auto-disconnect
        if (!auto_disconnect) {
          DBI::dbDisconnect(conn)
        }
      }

      return(TRUE)
    },
    error = function(e) {
      # Manually close connection if we disabled auto-disconnect
      if (!auto_disconnect) {
        tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL)
      }
      stop("Failed to write table: ", e$message)
    }
  )
}
