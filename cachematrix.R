## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

      #set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # get the value of the matrix
        get <- function() x
	# set the value of the inverse of the matrix
        setsolve <- function(solve) inv <<- solve
	# get the value of the inverse of the matrix
        getsolve <- function() inv

	# return our list
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Computes the inverse of the cacheable matrix returned by makeCacheMatrix()
## If the inverse has already been calculated and there's no change in the matrix
## then the cacheSolve() returns the cached inverse

cacheSolve <- function(cacheable.matrix, ...) {
 cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve() # retrieve from cache

        if(!is.null(inv)) { # we got something...
                message("getting cached data")
                return(inv)
        }

	# cache isn't filled yet
        data <- x$get()       # get it
        inv <- solve(data, ...) # solve it
        x$setsolve(inv)	      # set it
        inv
}
