rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data

	## Check that state and outcome are valid

	## Return hospital name in that state with the given rank
	## 30-day death rate

	outcomes = c("heart attack", "heart failure", "pneumonia")
    	
	if(outcome %in% outcomes == FALSE) {
		stop("invalid outcome")
	}

	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	
	if(state %in% data[,7] == FALSE) {
		stop("invalid state")
	}

	if(outcome == outcomes[1]) {
		col_idx = 11
	} else if(outcome == outcomes[2]) {
		col_idx = 17		
	} else {
		col_idx = 23
	}

	data[,col_idx] <- as.numeric(data[,col_idx])
	data <- data[data$State==state & !is.na(data[,col_idx]),]

	data <- data[order(data[,col_idx], data[,"Hospital.Name"]),]

	if(num == "best") {
		num <- 1
	}
	else if(num == "worst") {
		num <- nrow(data)
	}

	data[num, "Hospital.Name"]

}
