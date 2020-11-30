############################## basic functions ##############################

map_rank <- function(card){
  
  if(card == "A") {
    14
  } else if (card == "K") {
    13
  } else if (card == "Q"){
    12
  } else if (card == "J"){
    11
  } else if (card == "T"){
    10
  } else {as.numeric(card)}
}

############################## preflop hand score based on Chen formula ##############################
# https://en.wikipedia.org/wiki/Texas_hold_%27em_starting_hands#cite_note-krieger-2

map_value <- function(card){
  
  if(card == "A") {
    10
  } else if (card == "K") {
    8
  } else if (card == "Q"){
    7
  } else if (card == "J"){
    6
  } else if (card == "T"){
    10
  } else {as.numeric(card)/2}
}


gap_pts <- function(card_values, pts){
  
  gap <- abs(card_values[[2]] - card_values[[1]]) - 1
  
  if(gap == 1 | gap == 2) {
    pts <- pts - gap
  } else if (gap == 3){
    pts <- pts - 4
  } else if (gap >= 4){
    pts <- pts - 5
  }
  
  if( (gap == 0 | gap == 1) & max(card_values) < 7) {
    pts <- pts + 1
  }
  return(pts)
}

apply_chen_formula <- function(cards){
  
  if(cards == ""){
    return("")
  }else{
  
    pts <- 0
    
    card_vec <- str_split(cards, " ")[[1]]
    
    if (substr(card_vec[1],2,2) == substr(card_vec[2],2,2)){
      pts <- 2
    }
    
    card_vec <- c(substr(card_vec[1],1,1), substr(card_vec[2],1,1))
    
    card_values <- sapply(card_vec, map_value)
    card_ranks <- sapply(card_vec, map_rank)
    
    if (card_values[[1]] == card_values[[2]]){
      if (card_values[[1]] >= 3) {
        pts <- card_values[[1]]*2
      } else if (card_values[[1]] == 2.5) {
        pts <- 6
      } else  {
        pts <- 5
      }
    } else {
      pts <- pts + max(card_values) + gap_pts(card_ranks, pts)
    }
    return(pts)
  }
}

apply_chen_formula <- Vectorize(apply_chen_formula)

############################# detect made hands / draws ##################################

# straight_flush
check_straightflush <- function(suits, card_seq){
  return(check_flush(suits) * check_straight(card_seq))
}

# quads
check_quads <- function(values){
  if (max(table(values)) == 4){
    return(TRUE)
  } else {return(FALSE)}
}

# FH
check_fh <- function(values){
  if (max(table(values)) == 3 & sort(table(values), decreasing = TRUE)[2]>=2){
    return(TRUE)
  } else {return(FALSE)}
}

# flush
check_flush <- function(suits){
  if (max(table(suits)) >= 5){
    return(TRUE)
  } else {return(FALSE)}
}

# straight
check_straight <- function(card_seq){
  if (length(card_seq) >= 5){
    
    if (identical(card_seq[1:4], as.numeric(c(2:5))) & max(card_seq) == 14){ # wheel
      return(TRUE)
    }
    for (i in 5:length(card_seq)){ # non wheel
      if(identical(card_seq[(i-4):i], seq(card_seq[i-4], by = 1, length.out = 5))){ 
        return(TRUE)
      }
    }
    return(FALSE)
  } else{return(FALSE)}
}

# trips
check_trips <- function(values){
  if (max(table(values)) == 3){
    return(TRUE)
  } else {return(FALSE)}
}
  
# 2pair
check_2pair <- function(values){
  if (max(table(values)) == 2 & sort(table(values), decreasing = TRUE)[2]==2){
    return(TRUE)
  } else {return(FALSE)}
}

# top or second pair
check_top_second_pair <- function(values){
  if (max(table(values)) == 2 & 
      (table(values)[as.character(max(values))] == 2 | 
      table(values)[as.character(sort(unique(values), decreasing = TRUE)[2])] == 2)){
    return(TRUE)
  } else {return(FALSE)}
}

# flush draw
check_flushdraw <- function(suits){
  if (max(table(suits)) == 4){
    return(TRUE)
  } else {return(FALSE)}
}

# up/down straight draw

check_updown_straightdraw <- function(card_seq){
  if (length(card_seq) >= 4){
    
    if (identical(card_seq[1:4], as.numeric(c(2:5))) & max(card_seq) == 14){ # wheel
      return(TRUE)
    }
    for (i in 4:length(card_seq)){ # non wheel
      if(identical(card_seq[(i-3):i], seq(card_seq[i-3], by = 1, length.out = 4))){ 
        return(TRUE)
      }
    }
    return(FALSE)
  } else{return(FALSE)}
}

############# classify hands as good or bad ################


classify_hands <- function(card_str , expected_length){
  
  cards <- unlist(strsplit(card_str, " "))
  
  if (length(cards) == expected_length){
    
    suits <- sapply(cards, function(x) substr(x,2,2))
    values <- sapply(sapply(cards, function(x) substr(x,1,1)), map_rank)
    card_seq <- sort(unique(values))
    
    if(check_quads(values) | 
       check_flush(suits) |  
       check_straight(card_seq) | 
       check_trips(values) | 
       check_2pair(values) |
       check_top_second_pair(values) 
    ){
      return("G")
    } else if(length(cards) <= 6 & 
              (check_flushdraw(suits) |
               check_updown_straightdraw(card_seq)) 
    ){
      return("G")
      
    } else return("B")
    
  } else {
    return("")
  }
  
}

classify_hands <- Vectorize(classify_hands)
