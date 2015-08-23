
## makeCacheMatrix sets the matrix.
## cacheSolve calculates the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function is to cache the inverse of a matrix. 
## If the inverse of the matrix is already calculated, 
## it takes the result from the cache else it calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
		if (!is.null(m)) {
			message("getting cached data")
			return(m)
		}
		matrixdata <- x$get()
		m <- solve(matrixdata, ...)
		x$setinverse(m)
		m
}
