## This file contains two functions makeCacheMatrix & CacheSolve. 
## makeCacheMatrix creates an object which can store a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
	invg <- NULL
	set <- function(y) {
		x <<- y
		invg <<- NUll
	}
	get <- function() x
	setinv <- function(inv) invg <<- inv
	getinv <- function() invg
	list(set = set, get = get, setinv = setinv, getinv = getinv)	
}


## cacheSolve retrieves the inverse of a matrix if it is present in the cache else calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invg <- x$getinv()
	if(!is.null(invg)) {
		print("Inverse retrieved from Cache")
		return(invg)
	}
	mat <- x$get()
	invg <- solve(mat)
	x$setinv(invg)
	invg	
}
