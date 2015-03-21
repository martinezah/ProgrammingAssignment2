## This file demonstrates caching a calculation for the inverse of a matrix. The first function
## creates a "cache matrix" object, and the second one calculates and caches the solution on the 
## first call, and returns that result on subsequent calls.

## makeCacheMatrix creates a "cache matrix" object that can store the solved inverse of the
## matrix for subsequent calls for the solution.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolution <- function(sol) s <<- sol
  getSolution <- function() s
  list(set = set, get = get,
       setSolution = setSolution,
       getSolution = getSolution)
}


## cacheSolve takes a "cache matrix" object produced by makeCacheMatrix and returns a solved
## inverse of the matrix, using a cached solution if the matrix has not been modified since 
## the last call to cacheSolve.

cacheSolve <- function(x, ...) {
  s <- x$getSolution()
  if(!is.null(s)) {
    return(s)
  }
  d <- x$get()
  s <- solve(d)
  x$setSolution(s)
  s
}
