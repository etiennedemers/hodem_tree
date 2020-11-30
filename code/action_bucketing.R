
last <- function(x) { return( x[length(x)] ) }

bucket_preflop_action <- function(action_sequence){

  actions <- unlist(strsplit(action_sequence, " "))
  
  if (last(actions) == "f") {
    if(length(actions) %% 2 != 0){
      return("nf") #1
    } else{
      return("npf") #2
    }
  } else {
    return("npp") #3
  }
}

bucket_preflop_action <- Vectorize(bucket_preflop_action)

bucket_postflop_action <- function(action_sequence){
  
  if(action_sequence == "") {return("")}
  actions <- unlist(strsplit(action_sequence, " "))
  
  if (last(actions) == "f") {
    if(length(actions) %% 2 == 0){
      return("rf") #1
    } else{
      return("fr") #2
    }
  } else if (action_sequence == "k k") {
    return("kk") #3
  } else{
    return("rr") #4
  }
}

bucket_postflop_action <- Vectorize(bucket_postflop_action)
