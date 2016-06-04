## Caching Inverse of a Matrix using functions makeCacheMatrix and cacheSolve

## makeCacheMatrix: This function creates a special "matrix"
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
}
  get <- function() x
  setInverse <- function(inverse) inv_m <<- inverse
  getInverse <- function() inv_m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv_m <- x$getInverse()
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  dmat <- x$get()
  inv_m <- solve(dmat, ...)
  x$setInverse(inv_m)
  inv_m
}
