
#### Getting all necessary D1 college data for Elo Calculations


# Necessary Packages

library(tidyverse)
library(baseballr)

# Acquiring D1 Schedules

div_1_teams <- ncaa_teams(division = 1, year = 2024)

schedule_scraper_ncaa <- function(teamid) {
  
  safe_schedule <- safely(get_ncaa_schedule_info)
  
  result <- safe_schedule(team_id = teamid, year = 2024)
  
  Sys.sleep(1)  
  
  return(result$result)  # Return the actual result
}

# Use map to apply the function over the team IDs and then bind the results into a single data frame

schedules_list <- map(div_1_teams$team_id, schedule_scraper_ncaa)

# Bind to 1 df

schedules_df <- bind_rows(schedules_list)

schedules_df_unique <- schedules_df %>%
  filter(!is.na(game_info_url) & !duplicated(game_info_url))


# PBP

pbp_scraper_ncaa <- function(url) {
  
  safe_pbp <- safely(get_ncaa_baseball_pbp)
  
  result <- safe_pbp(game_info_url = url)
  
  Sys.sleep(2)  
  
  return(result$result)  
}



pbp_list <- map(schedules_df_unique$game_info_url, pbp_scraper_ncaa)


# Get Rosters

roster_scraper_ncaa <- function(teamid) {
  
  safe_roster <- safely(ncaa_roster)
  
  result <- safe_roster(team_id = teamid, year = 2024)
  
  Sys.sleep(1)  
  
  return(result$result)  
}


roster_list <- map(div_1_teams$team_id, roster_scraper_ncaa)

roster_df <- bind_rows(roster_list)


# Get Park Factors

park_factor_scraper_ncaa <- function(teamid) {
  
  safe_pf <- safely(ncaa_park_factor)
  
  result <- safe_pf(team_id = teamid, years = 2021:2023, type = 'division')
  
  Sys.sleep(1)  
  
  return(result$result)  
}


park_factor_list <- map(div_1_teams$team_id, park_factor_scraper_ncaa)

park_factor_df <- bind_rows(park_factor_list)


# Get Starting Lineups

lineup_scraper_ncaa <- function(url) {
  
  safe_lineup <- safely(ncaa_lineups)
  
  result <- safe_lineup(game_info_url = url)
  
  Sys.sleep(1)  
  
  return(result$result) 
}


lineup_list <- map(schedules_df_unique$game_info_url, lineup_scraper_ncaa)

lineup_df <- bind_rows(lineup_list)





