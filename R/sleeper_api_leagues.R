# Sleeper API Endpoints for Leagues

# Load required packages
pacman::p_load(httr, jsonlite, tibble, tidyr, dplyr, glue, rlang)

# Helper functions
source("./R/helpers.R")

#' Validate transaction type parameter
#'
#' Checks if the provided transaction type is valid for Sleeper API
#'
#' @param tx_type Character string representing transaction type or NULL
#' @return The original tx_type if valid, or NULL if invalid
#' @examples
#' validate_tx_type("trade")    # Returns "trade"
#' validate_tx_type("invalid")  # Shows message, returns NULL
validate_tx_type <- function(tx_type) {
  # Valid transaction types
  valid_tx_types <- c("waiver", "free_agent", "trade")

  # Create a validator function
  is_valid_tx_type <- function(x) {
    rlang::is_null(x) ||
      (is.character(x) && length(x) == 1 && x %in% valid_tx_types)
  }

  # Check validation and return appropriate value
  if (is_valid_tx_type(tx_type)) {
    # Return the original value (either NULL or valid type)
    return(tx_type)
  } else {
    # Return NULL for invalid values, with informative message
    log_message(glue::glue("Invalid transaction type: '{tx_type}'.
                 Must be one of: {paste(valid_tx_types, collapse = ', ')}"))
    log_message("Returning NULL (for unfiltered transactions)")
    return(NULL)
  }
}

#' Get specific league information
#'
#' Fetches information about a specific league from the Sleeper API
#'
#' @param league_id Character string representing the league ID
#' @return A list containing league information
#' @examples
#' league_data <- get_specific_league("123456789")
get_specific_league <- function(league_id) {
  # Fetch specific league from Sleeper API
  url <- paste0("https://api.sleeper.app/v1/league/", league_id)
  log_message(paste0("Fetching specific league data from endpoint: ", url))
  response <- httr::GET(url)

  # If not successful, stop and return error message
  status_sleeper_api(response)

  # Extract/parse content
  log_message("Extracting content from request.")
  content <- httr::content(response, "text")

  # Return the response content
  log_message("Converting R objects to JSON.")
  return(jsonlite::fromJSON(content))
}

#' Get league users information
#'
#' Fetches information about all users in a specific league from the Sleeper API
#'
#' @param league_id Character string representing the league ID
#' @return A tibble containing users information
#' @examples
#' users_data <- get_league_users("123456789")
get_league_users <- function(league_id) {
  # Fetch specific league users from Sleeper API
  url <- paste0("https://api.sleeper.app/v1/league/", league_id, "/users")
  log_message(paste0("Fetching all league users from endpoint: ", url))
  response <- httr::GET(url)

  # If not successful, stop and return error message
  status_sleeper_api(response)

  # Extract/parse content
  log_message("Extracting content from request.")
  content <- httr::content(response, "text")

  # Convert to tibble and unnest
  log_message("Converting R objects to tibble.")
  user <- content |>
    jsonlite::fromJSON() |>
    tibble::tibble() |>
    tidyr::unnest_wider(1, names_sep = "_")

  return(user)
}

#' Get league rosters information
#'
#' Fetches information about all rosters in a specific league from the Sleeper API
#'
#' @param league_id Character string representing the league ID
#' @return A list containing rosters information
#' @examples
#' rosters_data <- get_league_rosters("123456789")
get_league_rosters <- function(league_id) {
  # Fetch specific league rosters from Sleeper API
  url <- paste0("https://api.sleeper.app/v1/league/", league_id, "/rosters")
  log_message(paste0("Fetching all league rosters from endpoint: ", url))
  response <- httr::GET(url)

  status_sleeper_api(response)

  # Extract/parse content
  log_message("Extracting content from request.")
  content <- httr::content(response, "text")

  # Return the response content
  log_message("Converting R objects to JSON.")
  return(jsonlite::fromJSON(content))
}

#' Get league matchups for a specific week
#'
#' Fetches matchup information for a specific week in a league from the Sleeper API
#'
#' @param league_id Character string representing the league ID
#' @param week Integer representing the week number
#' @return A list containing matchups information
#' @examples
#' matchups_data <- get_league_matchups("123456789", 5)
get_league_matchups <- function(league_id, week) {
  # Fetch specific league rosters from Sleeper API
  url <- paste0("https://api.sleeper.app/v1/league/", league_id, "/matchups/", week)
  log_message(paste0("Fetching all league matchups for week ", week,
                     " from endpoint: ", url))
  response <- httr::GET(url)

  status_sleeper_api(response)

  # Extract/parse content
  log_message("Extracting content from request.")
  content <- httr::content(response, "text")

  # Return the response content
  log_message("Converting R objects to JSON.")
  return(jsonlite::fromJSON(content))
}

#' Get league transactions for a specific week
#'
#' Fetches transaction information for a specific week in a league from the Sleeper API
#' with optional filtering by transaction type
#'
#' @param league_id Character string representing the league ID
#' @param week Integer representing the week number
#' @param tx_type Optional character string representing the transaction type
#'                to filter by. Must be one of: "waiver", "free_agent", "trade"
#' @return A tibble containing transactions information, or NULL if no transactions found
#' @examples
#' # Get all transactions for week 3
#' all_transactions <- get_league_transactions_week("123456789", 3)
#'
#' # Get only trade transactions for week 3
#' trade_transactions <- get_league_transactions_week("123456789", 3, "trade")
get_league_transactions_week <- function(league_id, week, tx_type = NULL) {
  # Fetch specific league rosters from Sleeper API
  url <- paste0("https://api.sleeper.app/v1/league/", league_id, "/transactions/", week)
  log_message(paste0("Fetching all league transactions for week ", week,
                     " from endpoint: ", url))
  response <- httr::GET(url)

  status_sleeper_api(response)

  # Extract/parse content
  log_message("Extracting content from request.")
  content <- httr::content(response)

  # Return the response content
  log_message("Converting R objects to tibble.")
  transactions <- content |>
    tibble::tibble() |>
    # Expand nested single col tibble
    tidyr::unnest_wider(1)

  if (length(transactions) == 0) {
    return(NULL)
  }

  # Filter by transaction type only if specified
  # Validate tx_type - returns original value if valid, NULL if invalid
  validated_tx_type <- validate_tx_type(tx_type)

  # Apply filtering if applicable (only when validated_tx_type is not NULL)
  if (!is.null(validated_tx_type)) {
    transactions <- transactions |>
      dplyr::filter(type == validated_tx_type)
  }

  # Add week info
  transactions$week <- week

  return(transactions)
}
