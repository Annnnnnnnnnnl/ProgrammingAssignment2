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

	# return list
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Computes the inverse of the cacheable matrix returned by makeCacheMatrix()
## If the inverse has already been calculated and there's no change in the matrix
## then the cacheSolve() returns the cached inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
