## This is a pair of functions that cache the inverse of a matrix
## This function creates a special "matrix" object that can cache its inverse.
## The output of this function is a list of 4 elements that can be checked by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve retrieves the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  message("calculating inverse")
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}