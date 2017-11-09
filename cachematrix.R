## Combined, these two functions allow the inverse of a matrix to be calculated and stored in the cache (global-
## environment) to avoid recalculating repeatedly.

## Initial function gives a list of functions to get and set the matrix and the inverse. 

## Author: Robert Emberson, Nov 2017

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #assigns Null value for inverse value in local environment
  set <- function(y) { 
    x <<- y #sets global value for x
    inv <<- NULL #assigns Null value for inverse value in global environment
  }
  get <- function() x 
  setinv <- function(inverse) inv <<- inverse #sets the global value for inv 
  getinv <- function() inv #gets the inverse of the input matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve finds the inverse of the matrix and saves it into the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinv() #gets inverse value of matrix from local environment
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) #Gets the local value of inv if it's already calculated
  }
  data <- x$get() #if no cached, then gets the local value for the matrix
  inv <- solve(data, ...) #finds inverse value of matrix, assigns to local inv
  x$setinv(inv) #sets the global inverse value
  inv #prints inverse value
}
