setwd("~/COURSERA")
## To create a matrix that can store its content and its inverse as well

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix(sample(1:100, 16),4,4)) {
 i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve is a function which computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$Inverse()
  if(!is.null(i)) {
    message("getting cached datamatrix")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat, ...)
  x$setInverse(i)
  i
}
