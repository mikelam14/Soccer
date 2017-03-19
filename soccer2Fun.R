## all utility functions for soccer2.R  

# 1. get home or away
getHorA <- function(info, team){
  if( team == info["ht"]) result = "H"
  else result = "A"
  return(result)
}

# 2. get win or lose
getWinLose <- function(info, team){
  if ((team == info["ht"] && info["fthg"] > info["ftag"]) || (team == info["at"] && info["fthg"] < info["ftag"])){
    result = "W"
  } else if (info["fthg"] == info["ftag"])  {
    result = "D"
  } else result = "L"
  return(result)
}



# trash



#levels(events$event_type) = c("Attempt","Hand Ball","Penalty Conceded","Corner","Foul","Yellow Card","Second yellow card","Red card",
#                              "Substitution","Free Kick won","Offside")
#levels(events$event_type2) = c("Key Pass","Failed through ball","Sending off","Own Goal")
#levels(events$side) = c("Home","Away")
#levels(events$shot_place) = c("Bit too High","Too high","Top centre of the goal","Top left corner","Top right corner","Blocked",
#                              "Bottom left corner","Bottom right corner","Centre of the goal","High and wide","Hits the bar",
#                              "Misses to the left","Misses to the right")
#levels(events$shot_outcome) = c("On target","Off target","Blocked","Hit the bar")
#levels(events$location) = c("Attacking half","Left side of the box","Left side of the six yard box")
