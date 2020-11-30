library(data.table)
library(stringr)
library(dplyr)

last <- function(x) { return( x[length(x)] ) }

# preflop action
read_preflop_action <- function(hand, bb){
  
  pf_action <- ""
  row_num <- grep("HOLE CARDS", hand) + 2
  
  while(!grepl("FLOP|folds", hand[row_num])){
    
    if(grepl("raises", hand[row_num])){
      raise_amt <- strsplit(hand[row_num], split = " ")[[1]]
      raise_amt <- as.numeric(str_sub(last(raise_amt[grepl("$", raise_amt, fixed = TRUE)]),2))/bb
      pf_action <- paste0(pf_action, "r", raise_amt, " ")
    }
    
    if(grepl("calls", hand[row_num])){
      pf_action <- paste0(pf_action, "c", " ")
    }
    
    if(grepl("checks", hand[row_num])){
      pf_action <- paste0(pf_action, "k", " ")
    }
    
    row_num <- row_num + 1
  }
  
  if(grepl("folds", hand[row_num])){pf_action <- paste0(pf_action, "f")}
  
  pf_action <- trimws(pf_action, which = "right")
  
  return(pf_action)
  
}

# post flop action
read_action <- function(hand, bb, stage){
  
  if (stage == "FLOP"){
    end_marker <- "TURN|folds"
  } else if (stage == "TURN") {
    end_marker <- "RIVER|folds"
  } else if (stage == "RIVER") {
    end_marker <- "SHOW DOWN|folds"
  }
  
  action <- ""
  row_num <- grep(stage, hand) + 1
  
  if(!identical(row_num, numeric(0))){
    
    while(!grepl(end_marker, hand[row_num])){
      
      if(grepl("bets", hand[row_num])){
        raise_amt <- strsplit(hand[row_num], split = " ")[[1]]
        raise_amt <- as.numeric(str_sub(last(raise_amt[grepl("$", raise_amt, fixed = TRUE)]),2))/bb
        action <- paste0(action, "r", raise_amt, " ")
      }
      
      if(grepl("raises", hand[row_num])){
        raise_amt <- strsplit(hand[row_num], split = " ")[[1]]
        raise_amt <- as.numeric(str_sub(last(raise_amt[grepl("$", raise_amt, fixed = TRUE)]),2))/bb
        action <- paste0(action, "r", raise_amt, " ")
      }
      
      if(grepl("calls", hand[row_num])){
        action <- paste0(action, "c", " ")
      }
      
      if(grepl("checks", hand[row_num])){
        action <- paste0(action, "k", " ")
      }
      
      row_num <- row_num + 1
    }
    
    if(grepl("folds", hand[row_num])){action <- paste0(action, "f")}
    if(grepl("calls", hand[row_num])){action <- paste0(action, "c")}
    
    action <- trimws(action, which = "right")
    
  }
  
  return(action)
}

# read community cards
read_cards <- function(hand, stage){
  
  cards <- grep(stage, hand, value = TRUE)
  cards <- sub("\\].*", "", sub(".*\\[", "", cards))
  if(identical(cards, character(0))){cards <- ""}
  
  return(cards)
}

# read all hands in a HU directory
read_hu_hands <- function(directory){
  
  file_list <- list.files(directory)
  
  df <- data.frame(sb_chips = numeric(0), bb_chips = numeric(0),
                   sb_cards = character(0), bb_cards = character(0),
                   flop_cards = character(0), turn_cards = character(0), river_cards = character(0),
                   pf_action = character(0) , f_action = character(0),
                   t_action = character(0), r_action = character(0)
  )
  # read every hand in every file one by one
  for (file in file_list) {
    
    session <- readLines(paste0(directory, "/", file))
    empties <- !nchar(session)
    hands <- split(session[!empties], cumsum(empties)[!empties])
    
    for (hand in hands) {
      
      # reading player seats/chips
      bb <- as.numeric(gsub("[^0-9.-]", "", sub(".*\\/", "", sub("\\).*", "", sub(".*\\(", "", hand[1]))))) 
      
      hole_cards_row <- hand[grep("HOLE CARDS", hand) + 1]
      hole_cards <- sub("\\].*", "", sub(".*\\[", "", hole_cards_row))
      
      player_username <- strsplit(hole_cards_row, split = " ")[[1]][3]
      player_seat <- as.numeric(substr(strsplit(grep(player_username, hand[3:4], value = TRUE) , split = " ")[[1]][2],1,1))
      
      sb_seat <- strsplit(hand[2], split = " ")[[1]]
      sb_seat <- as.numeric(substr(sb_seat[length(sb_seat)-3],2,2))
      bb_seat <- setdiff(c(1,2), sb_seat)
      
      starting_chips <- c(str_sub(strsplit(sub("\\).*", "", sub(".*\\(", "", hand[3])), split = " ")[[1]][1],2),
                          str_sub(strsplit(sub("\\).*", "", sub(".*\\(", "", hand[4])), split = " ")[[1]][1],2))
      
      sb_chips <- as.numeric(starting_chips[as.numeric(sb_seat)])/bb
      bb_chips <- as.numeric(starting_chips[as.numeric(bb_seat)])/bb
      
      hand_summary <- hand[(grep("SUMMARY", hand)+1):length(hand)]
      
      if(player_seat == sb_seat) {
        sb_cards <- hole_cards
        bb_cards <- grep(paste0("Seat ", bb_seat), hand_summary, value = TRUE)
        if(grepl("[", bb_cards, fixed = TRUE)){
          bb_cards <- sub("\\].*", "", sub(".*\\[", "", bb_cards))
        } else {
          bb_cards <- ""
        }
      } else {
        bb_cards <- hole_cards
        sb_cards <- grep(paste0("Seat ", sb_seat), hand_summary, value = TRUE)
        if(grepl("[", sb_cards, fixed = TRUE)){
          sb_cards <- sub("\\].*", "", sub(".*\\[", "", sb_cards))
        } else {
          sb_cards <- ""
        }
      }
      
      # read community cards 
      flop_cards <- read_cards(hand, "FLOP")
      turn_cards <- read_cards(hand, "TURN")
      river_cards <- read_cards(hand, "RIVER")
      
      # action
      
      pf_action <- read_preflop_action(hand, bb)
      f_action <- read_action(hand, bb, "FLOP")
      t_action <- read_action(hand, bb, "TURN")
      r_action <- read_action(hand, bb, "RIVER")
      
      # append hand
      df2 <- data.frame(sb_chips = sb_chips, bb_chips = bb_chips,
                        sb_cards = sb_cards, bb_cards = bb_cards,
                        flop_cards = flop_cards, turn_cards = turn_cards, river_cards = river_cards,
                        pf_action = pf_action, f_action = f_action,
                        t_action = t_action, r_action = r_action
      )
      
      df <- rbind(df, df2)
    }
    
  }
  
  return(df)
}
