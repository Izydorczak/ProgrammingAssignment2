## These functions work together to return an inverse of a matrix. 
## They cut down on time by storing inverses that have previously been solved.

## This function caches matrices and their inverses

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set=set, get=get, setinv = setinv, getinv = getinv)
}


## This function retrieves (if available) or solves to return an inverse of a matrix

cacheSolve <- function(x = matrix(), ...) {
	inv <- x$getinv()
	if (!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
        ## Return a matrix that is the inverse of 'x'
}
