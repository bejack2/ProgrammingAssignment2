## These functions allow the user to create a matrix and cache the inverse of that matrix

## This function creates a special matrix, which is really a list of functions to manipulate the matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of the special matrix created with the above function

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
    	message("getting cached data")
    	return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}