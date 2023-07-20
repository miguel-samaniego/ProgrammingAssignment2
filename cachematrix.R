## The functions in this script allow for the implementation of a Cache Matrix
## makeCacheMatrix creates this special type of matrix, which allows to cache
## its inverse
## cacheSolve retrieves the value from the cache or computes the inverse and 
## then stores it in the cache

## Create a special matrix that allows to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  cache.inv <- NULL
  set <- function(my.matrix) {
    x <<- my.matrix
    cache.inv <<- NULL
  }
  get <- function() x
  set.inv <- function(my.inv) cache.inv <<- my.inv
  get.inv <- function() cache.inv
  list(set=set, get=get,
       set.inv=set.inv,
       get.inv=get.inv)
}


## Check to see if inverse was already calculated
## Otherwise, calculate it and store the value in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cache.inv <- x$get.inv()
  if (!is.null(cache.inv)) {
    message("getting cached data")
    return(cache.inv)
  }
  data <- x$get()
  cache.inv <- solve(data, ...)
  x$set.inv(cache.inv)
  cache.inv
}
