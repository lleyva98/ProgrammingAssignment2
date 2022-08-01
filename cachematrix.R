## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#### makeCacheMatrix ####

# Creates a special matrix object than can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL  #Set the inverse matrix
  
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(inverse) {
    cachedInverse <<- inverse
  }
  
  getInverse <- function(){
    cachedInverse
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



#### cacheSolve ####

# Computes the inverse of the special matrix returned by makeachaMatrix avove

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invFunc <- x$getInverse
  
  if(!is.null(invFunc)) {
    message("getting cached data")
    return(invFunc)
  }
  
  data <- x$get()
  
  invFunc <- solve(data, ...)
  
  x$setInverse(invFunc)
  invFunc
}


f <- makeCacheMatrix(matrix(c(4, 7, 3, 6), ncol = 2))
f$get()
f$getInverse()
cacheSolve(f)
