## These two functions take a matrix, cache it, compute
## the inverse, and then return that value.

## This function takes a matrix and places it into 
## a cached space for subsequent computations.

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inverse <<- solve
      getinverse <- function() inverse
      list(set = set, get = get, 
           setinverse = setinverse,
           getinverse = getinverse)
}

## This function takes a matrix that has been cached
## and computes the inverse of that matrix. It then
## returns that computed inverse matrix.

cacheSolve <- function(x, ...) {
      inverse <- x$getinverse()
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      inverse
}
