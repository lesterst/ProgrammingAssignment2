## These are the functions for programming assignment 2.
## The purpose is to use object oriented programming methods to make a matrix object
## that will store a record of its own inverse so that if it is accessed multiple times
## it does not have to redo the calculations.  This is done by using superassignmnet
## to store the inverse outside the function scope at a higher level environment.

## The first function makes the special matrix object that can cache
## its inverse.  It takes a matrix as input and produces a new matrix object
## that has 4 functions attached to it.  The inverse is initially set to NULL.

makeCacheMatrix <- function(x = matrix()) {       ## matrix input
  m <- NULL 
  set <- function(y) {                            ## set new matrix values
    x <<- y                                       
    m <<- NULL
  }
  get <- function() x                             ## get the value of the matrix
  setinverse <- function(inverse) m <<- inverse   ## set stored value of inverse
  getinverse <- function() m                      ## get the stored value of inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This second function returns the value of the inverse of the matrix.
## If it is the first time it calculates from scratch and stores it.
## if it is the not the first time it retrieves the stored value

cacheSolve <- function(x, ...) {

  m <- x$getinverse()                             ## ask first function for inverse
  if(!is.null(m)) {                               ## if not null return stored value
    message("getting cached data")
    return(m)
  }
  data <- x$get()                                 ## if null then calculate inverse
  m <- solve(data, ...)
  x$setinverse(m)                                 ## and store it
  m
}
