# Helper functions

# Function to output message() with time stamp
log_message <- function(..., abort = FALSE) {
  timestamp <- toupper(format(Sys.time(), "%Y-%b-%d %H:%M:%S%p"))
  msg <- sprintf("[%s] %s", timestamp, paste(..., collapse = " "))

  if (abort) {
    stop(msg, call. = FALSE)
  } else {
    message(msg)
  }
}

# Function to check the status of the Sleeper API response
# Error codes are listed
status_sleeper_api <- function(response) {
  status <- httr::status_code(response)

  message_text <- switch(
    as.character(status),
    "400" = "Bad Request (400) – Your request is invalid.",
    "404" = "Not Found (404) – The requested resource could not be found.",
    "429" = "Too Many Requests (429) – Rate limit exceeded. Slow down!",
    "500" = "Internal Server Error (500) – Problem with Sleeper’s server. Try again later.",
    "503" = "Service Unavailable (503) – Sleeper is temporarily offline. Try again later.",
    NULL
  )

  if (!is.null(message_text)) {
    # stop() used with abort = TRUE
    # https://docs.sleeper.com/#errors
    ref <- "See https://docs.sleeper.com/#errors for more information on error codes."
    log_message(glue("{message_text} {ref}"), abort = TRUE)
  }

  # Also log success
  if (status >= 200 && status < 300) {
    invisible(TRUE)
    log_message(glue("OK (200) - The request was successful."))
  } else {
    warning(glue::glue("Unexpected HTTP status code: {status}"), call. = FALSE)
  }
}


# Database Functions -------------------------------------------------------------------
# List database tables, connect, read, write

# Function to list all tables stored in a database
db_list_tables <- function(con) {
  # List tables with connected database
  db_tbl_list <- dbListTables(con)
  log_message(glue("Stored database tables: "))
  for (table in db_tbl_list) {
    # Loop through table names and list with bullets
    message(glue("  \U23FA {table}"))
  }
}

# Connect to database and list saved tables
connect_db <- function(path_db, list_tbl = TRUE) {
  log_message("Loading SQLite database from: ", path_db)

  # Check if RSQLite is installed
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("Please install the 'RSQLite' package.")
  }

  # Connect to database
  con <- RSQLite::dbConnect(RSQLite::SQLite(), path_db)
  log_message("Successfully connected to database.")

  if (list_tbl == TRUE) {
    db_list_tables(con)
  }

  return(con)
}

# Load database table
read_tbl_db <- function(path_db, tbl_name) {
  # Connect to database
  con <- connect_db(path_db, list_tbl = FALSE)

  tbl <- dbReadTable(con, tbl_name)
  log_message(glue("Successfully loaded database table, '{tbl_name}'"))

  return(tbl)
}

# Load database table
write_tbl_db <- function(data, path_db, tbl_name, rm_current_tbl = TRUE) {
  # Check if RSQLite is installed
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("Please install the 'RSQLite' package.")
  }

  # Connect to database
  con <- connect_db(path_db, list_tbl = FALSE)

  RSQLite::dbWriteTable(con, tbl_name, data, overwrite = rm_current_tbl)
  log_message(glue("Successfully wrote table to database: '{tbl_name}'"))
  db_list_tables(con)
}


