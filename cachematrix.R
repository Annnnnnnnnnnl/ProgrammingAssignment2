## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

      #set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # get the value of the matrix
        get <- function() x
	# set the value of the inverse of the matrix
        setsolve <- function(solve) m <<- solve
	# get the value of the inverse of the matrix
        getsolve <- function() m

	# return our list
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

  # Functions for getting and setting cached inv. matrix value
  get <- function() original.matrix
   # Inversing the matrix using build in solve() function in R
  set.inverse <- function(solve) inverted.matrix <<- solve
  get.inverse <- function() inverted.matrix
  
  list(
    set = set, 
    get = get,
    set.inverse = set.inverse,
    get.inverse = get.inverse)


## Computes the inverse of the cacheable matrix returned by makeCacheMatrix()
## If the inverse has already been calculated and there's no change in the matrix
## then the cacheSolve() returns the cached inverse

cacheSolve <- function(cacheable.matrix, ...) {
 cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve() # retrieve from cache

        if(!is.null(m)) { # we got something...
                message("getting cached data")
                return(m)
        }

	# cache isn't filled yet
        data <- x$get()       # get it
        m <- solve(data, ...) # solve it
        x$setsolve(m)	      # set it
        m
}
