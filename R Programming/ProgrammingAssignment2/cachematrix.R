## The following two functions can be used to compute the inverse of 
## a matrix (considered always square and invertible) and to cache the
## result in order to retrieve it later without re-calculating it


## The following function takes in input a square and invertible matrix
## and creates a list object containing getters and setters methods to
## retrieve and store the input matrix and its inverse. The list is 
## returned as result

makeCacheMatrix <- function(x = matrix()) {
	
	## Initializing the inverse of the input matrix
	inv <- NULL

	## Getter method to retrieve the input matrix
	get <- function() {
		x
	}
	## Setter method to store the input matrix
	set <- function(mat) {
		x <<- mat
		inv <<- NULL
	}
	## Getter method to retrieve the inverse matrix
	getinv <- function() {
		inv
	}
	## Setter method to store the inverse matrix
	setinv <- function(inverse) {
		inv <<- inverse
	}
     
	## Returning a list containing the previous four methods
	list(	set = set, 
		get = get,
           	setinv = setinv,
           	getinv = getinv
	)
}


## The following function takes in input a list object created by the 
## makeCacheMatrix function and computes the inverse of the matrix contained
## in the list or gets it from the cache if the computation has been already 
## done. The returned object is the inverse matrix

cacheSolve <- function(x, ...) {
	
	## Getting cached inverse matrix from the imput list
	inv <- x$getinv()
	
	## If there isn't a cached inverse matrix in the input object,
	## I compute it and cache it, otherwise I get the cached one  
	if(is.null(inv)) {
		message("Computing inverse matrix and caching the result...")
	
		## Getting the matrix to invert
		data <- x$get()
	
		## Computing the inverse matrix
		inv <- solve(data)

		## Caching the inverse matrix
		x$setinv(inv)
	}
	else {
		message("Getting cached data...")
	}
	
	## Returning the result
	inv			
}
