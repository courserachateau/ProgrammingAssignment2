## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The function makeCacheMatrix generates a special 'matrix'
## object that can cache its inverse.

set.seed(20180429)

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
  make.inverse <- solve(data, ...)
  x$set.inverse(make.inverse)
  make.inverse
}


# Verify the script


test_matrix <- matrix(rnorm(25),5,5)

cached_matrix <- makeCacheMatrix(test_matrix)


cacheSolve(cached_matrix)


## Output 

# [,1]        [,2]       [,3]       [,4]       [,5]
# [1,] -0.5827930  0.04149969  0.1782068 -0.5089351  0.4128447
# [2,]  0.1105671  0.49056438  0.7052986 -0.4716935  0.2418285
# [3,] -0.6503459  0.07915577 -0.9248238  0.3046792  0.8282769
# [4,]  1.1440808 -0.60107950  2.1279077 -0.2201511 -1.2259513
# [5,] -0.2491548 -0.14440125 -0.3681268  0.5862543 -0.1702922