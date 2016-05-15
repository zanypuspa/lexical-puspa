## Underlying are a couple of functions that cache the inverse of a matrix.
## Here the functions combine to compute a inverse if a matrix ut before reversing, it checks
## if the cache already holds the required inverse of the gien matrix.
## Also, the code is based on the assumption that the matrix supplied is always invertible.

## Creates cache object for the inverse of a invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
  cacheInvMatrix <- NULL
  set <- function(y) {
    x <<- y
    cacheInvMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cacheInvMatrix <<- inverse
  getInverse <- function() cacheInvMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

## Checks if the cache of inverse of given matrix exists
## Returns the inverse of a given invertible matrix 

cacheSolve <- function(x, ...) {
  
  invFunc <- x$getInverse()
  if(!is.null(invFunc)) {
    message("<< cached data >>")
    return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data, ...)
  x$setInverse(invFunc)
  invFunc
}