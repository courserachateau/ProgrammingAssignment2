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

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
