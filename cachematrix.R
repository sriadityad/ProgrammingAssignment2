# Author : Sri Aditya Dosapati 
# Purpose : Function created as a part of course assingment on Coursera 
# The Following function creates
# a special function Matrix object
# the solve function can cache its inverse when necessary
# Programming Assignment 2


# Function for creating a special function that could be used for caching the response for evaluating matrix inverse

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #Create the components for the matrix
  get <- function() x
  setinverse <- function(invers) inv <<- invers
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


# Function for evaluating  the inverse of the matrix that is returned by function makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # Compute the inverse of the matrix and cache any relevant responses 
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
