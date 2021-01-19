## Put comments here that give an overall description of what your
## functions do

## This function creates a special “matrix” object that can cache its inverse

makeMatrix <- function(x = matrix()) {  # x is a square invertible matrix
  inverse <- NULL
  set <- function(y){     #Set the matrix
    x <<- y
    inverse <<- NULL
  }
  get <- function() x     #Get the matrix
  setInverse <- function(solveMatrix) inv <<- solveMatrix     #Set the inverse
  getInverse <- function() inverse                            #Get the inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above

 ## Return a matrix that is the inverse of 'x'
cacheSolve  <- function(x, ...) {
  # x output of makeMatrix()
  # return: inverse of the original matrix input to makeMatrix()
  inverse <- x$getInverse()
  # if the inverse has already been calculated
  if(!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inverse)
  }
   # otherwise, calculates the inverse 
  data <- x$get()
  inverse <- solve(data)
   # sets the value of the inverse in the cache via the setinv function.
  x$setInverse(inverse)
  inverse      
}
}
