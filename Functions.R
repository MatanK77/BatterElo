# Some of the key functions used in Batter Elo, including parsing the identity of the batter and the result of the PA from the description,
as well as some basic Elo functions.


# Expected score calculation for Elo

expected_score <- function(rating_a, rating_b, pf_adjustment) {
  rating_diff <- rating_a - rating_b
  
  # 400 rating points correspond to 1 standard deviation in run values
  expected_run_diff <- (((rating_diff / 400) * 0.05)) + pf_adjustment # 400 points should equal 1 SD of performance (~0.05 rv/pa)
  
  return(expected_run_diff)
}



# Batter Elo update function (pitcher is almost identical with 80 K_max)

update_elo_batter <- function(current_elo, expected, actual, plate_appearances) {
  K_max <- 100
  K_min <- 20
  
  # Calculate the decaying K-value using an exponential decay function
  K <- K_min + (K_max - K_min) * exp(-plate_appearances / 20)


  # Update the Elo rating
  return(current_elo + K * (actual - expected))
}




# Finding the result of a given PA

categorize_pa_result <- function(description) {
  description <- tolower(description)
  
  if (grepl("singled", description)) {
    return("Single")
  } else if (grepl("doubled", description)) {
    return("Double")
  } else if (grepl("tripled", description)) {
    return("Triple")
  } else if (grepl("homered", description)) {
    return("Home Run")
  } else if (grepl("hit by pitch", description)) {
    return("Hit By Pitch")
  } else if (grepl("walked", description)) {
    return("Walk")
  } else if (grepl("out after review|picked off|caught stealing", description)) {
    return(NA)
  } else if (grepl("struck out|popped up|grounded out|flied out|fouled out|out at|fielding error|reached on a throwing error|fielder's choice|lined out|double play|triple play|reached on an error", description)) {
    return("Out")
  } else {
    return(NA)  # For non-covered events above
  }
}





# Function to get batter's name from description

extract_batter_name_new <- function(description, batting, roster_dataframe) {
  
  # Put all of the description in lower case
  description_norm <- tolower(description)
  
  # Split the description to extract potential names
  words <- strsplit(description_norm, " ")[[1]]
  potential_name <- words[1:min(length(words), 2)]
  
  # Define a list of words to exclude that are definitely not part of a name
  exclude_words <- c("grounded", "struck", "flied", "hit", "walked", "doubled", "singled", "tripled", "homered", 
                     "fouled", "lined", "reached", "out", "popped", "walked,", "walked.", "walked3a", 
                     "doubled,", "singled,", "intentionally", "at first", "homered,", "advanced", "overturned,", 
                     "overturned", "play", "walked;", "iii", "jr.", "jr", "singled.", "jr,", "jr.,", "jr.,r", "iv,",
                     "singled3a")
  
  # Filter out the exclude words from the potential name
  potential_name <- potential_name[!potential_name %in% exclude_words]
  
  last_name <- ""
  first_initial <- ""
  
  if (length(potential_name) == 1) {
    if (grepl(",", potential_name[1])) {
      # Format "Last,F" without a space
      name_parts <- strsplit(potential_name[1], ",")[[1]]
      last_name <- name_parts[1]  # Always set the last name to the first part
      if (length(name_parts) >= 2 && nchar(trimws(name_parts[2])) > 0) {
        # Set the first initial if there is something besides whitespace after the comma
        first_initial <- substr(trimws(name_parts[2]), 1, 1)
      } else {
        # If there is whitespace or nothing after the comma, set first_initial to empty
        first_initial <- ""
      }
    } else {
      # Format 1: Smith (Last name only) or "Smith "
      last_name <- trimws(potential_name[1])
      first_initial <- ""  # Ensure first_initial is an empty string
    }
  } else if (length(potential_name) == 2) {
    # Handle other conditions when there are two elements in potential_name
    if (grepl(",", potential_name[1])) {
      last_name <- gsub(",", "", potential_name[1])
      first_initial <- substr(potential_name[2], 1, 1)
      first_initial <- gsub("\\.", "", first_initial)  # Remove period from initial
    } else if (!grepl("\\.", potential_name[1])) {
      last_name <- potential_name[2]
      first_initial <- ""  # No initial provided, set to empty string
    } else {
      last_name <- potential_name[2]
      first_initial <- substr(potential_name[1], 1, 1)
    }
  }
  
  # Remove apostrophes from the last name
  last_name <- gsub("'", "", last_name)
  
  # Filter roster to include only players from the batting team
  team_roster <- roster_dataframe[tolower(roster_dataframe$team_name) == tolower(batting), ]
  
  # Attempt to find a match in the roster
  for (player in team_roster$player_name) {
    player_norm <- tolower(player)
    player_parts <- strsplit(player_norm, ", ")[[1]]
    player_last_name <- remove_suffix(player_parts[1])
    
    # Remove apostrophes from the player's last name
    player_last_name <- gsub("'", "", player_last_name)
    
    player_first_initial <- ifelse(length(player_parts) > 1, tolower(substring(player_parts[2], 1, 1)), "")
    
    if (tolower(last_name) == tolower(player_last_name) && (first_initial == "" || tolower(first_initial) == tolower(player_first_initial))) {
      return(player)
    }
  }
  
  return(NA)  # Return NA if no match is found
}
