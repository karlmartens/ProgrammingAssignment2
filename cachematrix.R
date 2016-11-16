## The functions makeCacheMatrix and cacheSolve are used to wrap a matrix and cache the
## inverse of the matrix once it is computed.

## Creates a matrix that will wrap a matrix and retain a cache of its inverse matrix. 
## The available operations are:
##   set - accepts a matrix parameter to wrap; clears the cache.
##   get - returns the wrapped matrix
##   setinverse - accepts a matrix parameter (the inverse) and caches it.
##   getinverse - returns the cached matrix or NULL if one has not been set.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve accepts a matrix generated with makeCacheMatrix and computes the inverse 
## of the matrix. When the cache is NULL will compute and cache the inverse matrix. 
##
## Returns the inverse matrix.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
