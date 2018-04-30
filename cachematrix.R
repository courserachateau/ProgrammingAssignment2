## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The function makeCacheMatrix generates a special 'matrix'
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  make.inverse <- NULL
  set <- function(y) {
    x <<- y
    make.inverse <<- NULL
  }
  get <- function() x
  set.inverse <- function(inversed) make.inverse <<- inversed
  get.inverse <- function() make.inverse
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}


## Write a short comment describing this function
# Creates a inverse matrix: inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  make.inverse <- x$get.inverse()
  if(!is.null(make.inverse)) {
    message("getting cached data")
    return(make.inverse)
  }
  data <- x$get()
  make.inverse <- mean(data, ...)
  x$set.inverse(make.inverse)
  make.inverse
}