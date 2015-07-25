## makeCacheMatrix is a function that sets the values of matrix x, gets the values of the matrix x,
## sets the inverse of the matrix x, and gets the inverse of matrix x. This function is caching the 
## values of matrix x and the values of its inverse.

## cacheSolve is a function that will check if the inverse of the matrix x is cached. If it is
## not then, this function will calculate the inverse of matrix x and cache it.

## This function will cache the inverse of the matrix x.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)

}

## This function will check if the inverse of matrix x in the cache.
## If the cache is empty it will calculate the inverse and cache this value.


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
