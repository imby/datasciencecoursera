corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations

		res <- c()
		
		completeobs <- complete(directory)

		idthreshold <- completeobs[completeobs$nobs > threshold, "id"]

		if(length(idthreshold) > 0) {
			fname <- paste(formatC(idthreshold, width=3, flag="0"),".csv", sep="")
			data <- lapply(paste(directory, "/", fname, sep=""), read.csv, header=TRUE)

	 		calculate.cor = function(row) {
	    			cc = complete.cases(row)
				cor(row[cc, "sulfate"], row[cc, "nitrate"])

			}

			res <- sapply(data, calculate.cor)
		}

		res
}
