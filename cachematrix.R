## The following 2 functions will return the inverse of a matrix.
## The inverse will only be calculated if it hasn't already been calculated 
## in this session, if it has been calculated before it will be pulled from cache.

## This caches and returns from cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks the cache for the inverse of the given matrix, and if it isn't there, 
## it calculates the inverse, stores it in cache, and returns it.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
