## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. The purpose of these functions is to use caching to avoid
## repeated calculation of a matrix inverse. 

## The makeCacheMatrix function provides an object "wrapper" around a 
## matrix, with functions to set and get the matrix and its inverse

# makeCacheMatrix: This function creates a special "matrix" object
# that can cache its inverse.
#   set: set the value of the matrix
#   get: get the value of the matrix
#   setinv: set the value of the matrix inverse
#   getinv: get the value of the matrix inverse
#

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(minv) m <<- minv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix`. If the inverse has
##  already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

# cacheSolve: This function computes the inverse of a makeCacheMatrix matrix
# returning a cached version if possible
# cacheSolve(x, ...)
#   x: an invertible matrix
#

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
