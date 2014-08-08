rankall <- function(outcome, num = "best") {
	## Read outcome data
	## Check that state and outcome are valid
	## For each state, find the hospital of the given rank
	## Return a data frame with the hospital names and the
	## (abbreviated) state name

	outcomes = c("heart attack", "heart failure", "pneumonia")
    	
	if(outcome %in% outcomes == FALSE) {
		stop("invalid outcome")
	}

	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	if(outcome == outcomes[1]) {
		col_idx = 11
	} else if(outcome == outcomes[2]) {
		col_idx = 17		
	} else {
		col_idx = 23
	}

	data[,col_idx] <- as.numeric(data[,col_idx])
	data <- data[!is.na(data[,col_idx]),]

	data <- data[order(data$State,data[,col_idx], data$Hospital.Name),]

      res <- data.frame("hospital"=character(), "state"=character())

	for(st in unique(data$State)) {

		data_by_state <- data[data$State==st,]

		if(num == "best") {
			row_idx <- 1
		}
		else if(num == "worst") {
			row_idx <- nrow(data_by_state)
		}
		else {
			row_idx <- num
		}

      	res <- rbind(res, data.frame(hospital=data_by_state[row_idx, "Hospital.Name"], state=st))
	
	}

	res


}
