# The first function, makeVector creates a special "vector", which is really a list containing a function to
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the mean
# 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {

  # initialize the inverse matrix 
  inv <- NULL
  
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the value of the inverse
  setinverse <- function(inv_input) inv <<- inv_input
  
  # get the value of the inverse
  getinverse <- function() inv
  
  # return a list of all the above functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## The following function calculates the mean of the special "vector" 
## created with the above function. However, it first checks to see 
## if the mean has already been calculated. If so, it gets the mean 
## from the cache and skips the computation. Otherwise, it calculates 
## the mean of the data and sets the value of the mean in the cache 
## via the setmean function.

cacheSolve <- function(x, ...) {
  
  # check if the inverse is already cached,
  # if true, get the inverse from the cache directly
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  # else, we first get the matrix
  data <- x$get()
  
  # calculate the inverse
  inv <- solve(data, ...)
  
  # cache the inverse of the matrix
  x$setinverse(inv)
  
  # return the result
  inv
}

