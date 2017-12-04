rankall <- function(outcome, num = "best") {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	## Check that state and outcome are valid
	 poss_outcomes <- c("heart attack", "heart failure", "pneumonia")
             if (outcome %in% poss_outcomes)  {
                if (outcome == poss_outcomes[1]) { #11
                   if (state_abv %in% data$State)  {
#                      d <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
#                      arr_data <- arrange(data, State, d, Hospital.Name)
#                      e <- as.numeric(arr_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
                   } else {
                      message("invalid state")
                   }
                }
                if (outcome == poss_outcomes[2]) { #17
                   if (state_abv %in% data$State)  {
#                      d <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
#                      arr_data <- arrange(data, State, d, Hospital.Name)
#                      e <- as.numeric(arr_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
                   } else {
                      message("invalid state")
                   }
                }
                if (outcome == poss_outcomes[3]) { #23
                   if (state_abv %in% data$State)  {
#                      d <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
#                      arr_data <- arrange(data, State, d, Hospital.Name)
#                      e <- as.numeric(arr_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
                   } else {
                      message("invalid state")
                   }
                }
             } else {
                message("invalid outcome")
             }
	## For each state, find the hospital of the given rank
	state_names <- unique(arrData[,7])
	hosp_ranks <- matrix(nrow = length(state_names), ncol = 2)
	
	for (i in 1:length(state_names)) {
	    hosp_ranks[i,2]<- state_names[i]
	    hosp_ranks[i,1]<- rankhospital(state_names[i], outcome, num)
	}

	## Return a data frame with the hospital names and the
	## (abbreviated) state name
	hosp_ranks
}