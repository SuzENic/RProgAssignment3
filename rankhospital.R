rankhospital <- function(state_abv, outcome, num = "best") {
	     ## Read outcome data
	     data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	     ## Check that state and outcome are valid
	     poss_outcomes <- c("heart attack", "heart failure", "pneumonia")
	     if (outcome %in% poss_outcomes)  {
             	if (outcome == poss_outcomes[1]) { #11
	     	   if (state_abv %in% data$State)  {
		      d <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
		      arr_data <- arrange(data, State, d, Hospital.Name)
		      e <- as.numeric(arr_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
		   } else {
                      message("invalid state")
                   }
		}
             	if (outcome == poss_outcomes[2]) { #17
	     	   if (state_abv %in% data$State)  {
		      d <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
		      arr_data <- arrange(data, State, d, Hospital.Name)
		      e <- as.numeric(arr_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
		   } else {
                      message("invalid state")
                   }
		}
             	if (outcome == poss_outcomes[3]) { #23
	     	   if (state_abv %in% data$State)  {
		      d <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
		      arr_data <- arrange(data, State, d, Hospital.Name)
		      e <- as.numeric(arr_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
		   } else {
                      message("invalid state")
                   }
		}
	     } else {
                message("invalid outcome")
     	     }
	     ## Return hospital name in that state with the given rank
	     
	     state_scripts <- ((arr_data$State == state_abv) & (!is.na(e)))
	     out <- c(2, 7, 11)
	     if (num == "best") {num <-1}
	     if (num == "Best") {num <-1}
	     if (num == "Worst") {num <-sum(state_scripts)}
	     if (num == "worst") {num <-sum(state_scripts)}
	     
	     state_data <- arr_data[state_scripts,2]
		 state_data[num]
	     ## 30-day death rate
}
