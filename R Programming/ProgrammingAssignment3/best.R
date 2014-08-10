best <- function(state, outcome) {
	
	## Read outcome data

	## Check that state and outcome are valid
	
	## Return hospital name in that state with lowest 30-day death
	## rate

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

	rows <- which.min(data[,col_idx])

	data <- data[rows, "Hospital.Name"]

	data[min(order(data))]
	
}
