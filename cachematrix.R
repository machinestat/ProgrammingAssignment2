## The first function set the scope of the variable that we want
## to use later in the global environment. The second function
## can check and retreive the inverse of a marix from the cache.

## This function return a list of function set and get the value 
## and inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  # function set the value of the matrix
  set <- function(y){
    x << y
    i << NULL
  }
  
  # function get the matrix
  get <- function() x
  
  # function set the value of inverse
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  # function get the inverse of the matrix
  getInverse <- function() i
  
  # Return a list of functions
  list(set = set, get = get, setInverse= setInvese,
       getInverse = getInverse)
}


## For the matrix returned from previous function, this function
## first check if the inverse of the matrix has already been
## calculated, if not, this function compute the inverse of the
## matrix, and retreive the inverse from the cache otherwise.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # get the inverse of the matrix x
  i <- x$getInverse()
  
  # retrieve the inverse of the matrix from the cache
  # if the inverse already has been calculated.
  if(!is.null(i)){
    message("getting the inverse from the cache")
    return(i)
  }
  
  # calculate the inverse if it is not in the cache
  # get the matrix
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
