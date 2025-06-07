# Parse Sleeper Transactions with Sleeper API Functions

# Load required packages
pacman::p_load(dplyr, glue, httr, jsonlite, RSQLite, tidyr, purrr, rlang, tibble)

# Sleeper API Functions for Leagues
# source("./R/sleeper_api_players.R")
source("./R/sleeper_api_leagues.R")

parse_league_users <- function(league_id) {
  # Fetch specific league users from Sleeper API - returns tibble
  users <- get_league_users(league_id)

  # Unpack metadata column with nested list
  users_unpacked <- users |>
    tidyr::unnest_wider(metadata, names_sep = "_")

  # Select relevant columns - ignore mascots
  users_clean <- users_unpacked |>
    dplyr::select(
      user_id, display_name, commissioner = is_owner, metadata_allow_pn, metadata_mention_pn,
      metadata_allow_sms, metadata_player_like_pn, metadata_trade_block_pn,
      metadata_user_message_pn, metadata_league_report_pn, league_id, is_bot
    )

  return(users_clean)
}


