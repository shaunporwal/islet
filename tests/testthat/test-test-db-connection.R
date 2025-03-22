# Simple test script to verify JAR file is found properly
# Run this with: R -f tests/test-db-connection.R

library(islet)

# Check if the JAR file can be found
jar_path <- system.file("drv", "db2jcc4.jar", package = "islet")
cat("JAR file path:", jar_path, "\n")

if (file.exists(jar_path)) {
  cat("SUCCESS: JAR file exists and is accessible\n")
} else {
  cat("ERROR: JAR file not found\n")
}

# Load RJDBC to test if the driver can be initialized
if (requireNamespace("RJDBC", quietly = TRUE) && requireNamespace("rJava", quietly = TRUE)) {
  cat("Attempting to load DB2 JDBC driver...\n")

  # Try to initialize the driver without connecting
  tryCatch(
    {
      driver <- "com.ibm.db2.jcc.DB2Driver"
      drv <- RJDBC::JDBC(driver, jar_path)
      cat("SUCCESS: DB2 JDBC driver successfully loaded\n")
    },
    error = function(e) {
      cat("ERROR loading JDBC driver:", e$message, "\n")
    }
  )
} else {
  cat("RJDBC or rJava package not available. Cannot test JDBC driver.\n")
  cat("Install them with: install.packages(c('RJDBC', 'rJava'))\n")
}
