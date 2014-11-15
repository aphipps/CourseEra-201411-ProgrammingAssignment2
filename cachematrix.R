## Pair of functions allowing matrices to be inverted and the result cached.
## Use makeCacheMatrix to get the matrix supporting cached inversion operation.
## Then pass it to cacheSolve to get the inverse.  Repeated calls to cacheSolve
## will only result in the inverse being calculated the first time.

## Returns a list of functions to access the matrix parameter x supporting caching.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinv <- function(inv) m <<- inv
	getinv <- function() m
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Returns a matrix that is the inverse of 'x', representing a 'matrix' returned by makeCacheMatrix.  Caches the inverse.

cacheSolve <- function(x, ...) {
	m <- x$getinv()
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m
}

