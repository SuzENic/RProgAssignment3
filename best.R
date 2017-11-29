best <- function(state_abv, outcome) {
     ## Read outcome data
     data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     ## Check that state and outcome are valid
     poss_outcomes <- c("heart attack", "heart failure", "pneumonia")
     if (outcome %in% poss_outcomes)  {
     	    if (outcome == poss_outcomes[1]) {
	       		compare = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
			if (state_abv %in% data$State)  {
     	    		   sub_data <- split(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,data$State)
     	    		   sub_name <- split(data$Hospital.Name,data$State)
			   mini <- lapply(sub_data, min)
			   
     			} else {
       	    		   message("invalid state")
     			}    
	    }    
     	    if (outcome == poss_outcomes[2]) {
	       		compare = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" 
     			if (state_abv %in% data$State)  {
     	    		   sub_data <- split(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,data$State)
	    		   wh <- (!is.na(sub_data$state_abv[,compare]))
			   mini <- sapply(sub_data, min)
         		   wh2 <- (data[wh,compare] == mini)
     			} else {
       	    		   message("invalid state")
     			}    
	    }    
     	    if (outcome == poss_outcomes[3]) {
	       		compare = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
     			if (state_abv %in% data$State)  {
     	    		   sub_data <- split(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,data$State)
	    		   wh <- (!is.na(sub_data$state_abv[,compare]))
			   mini <- sapply(sub_data, min)
        		   wh2 <- (data[wh,compare] == mini)
		 	} else {
       	    		   message("invalid state")
     			}    
	    }    
     } else {
     	    message("invalid outcome")     
     }
     ## Return hospital name in that state with lowest 30-day death
##     sub_data <- split(data$compare,data$State) 
#     sub_data[wh2]
     ## rate
     mini[state_abv]
     wh <- (sub_data$NM == mini$NM)
     sub_name$NM[wh]
#      wh
}