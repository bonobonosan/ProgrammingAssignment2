# Caching the Inverse of a Matrix

# This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	# Store a matrix
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	# Return the stored matrix
	get <- function() {
		x
	}
	# Cache the given argument
	setInverse <- function(solve) {
		m <<- solve
	}
	# Return the cached value
	getInverse <- function() {
		m
	}
	# Return a list
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of x
	m <- x$getInverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	# Get the matrix and calculate the inverse
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	# Return the value
	m
}
