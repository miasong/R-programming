## Programming Assignment2: Writing a pair of functions that cache the inverse of a matrix 

## The first function, makeCacheMatrix, creates a special object that stores a numeric matrix that can cache its inverse.
## The second function, cacheSolve, calculates the inverse of the special matrix created with the first function.
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.



## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, 
	     get = get, 
	     setinv = setinv,
	     getinv = getinv)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...) 
	x$setinv(inv) 
	inv
}
