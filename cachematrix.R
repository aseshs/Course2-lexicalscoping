## Below are 2 functions that are used to create an object that 
## stores a matrix and caches its inverse.

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    
    x <<- y
    
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set,
       
       get = get,
       
       setInverse = setInverse,
       
       getInverse = getInverse)
}

## This function results in the inverse of the special "matrix" created by makeCacheMatrix function
## above. It retrieves the inverse from the cache if the inverse has already been calculated.

cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    
    message("bringing cached data")
    
    return(inv)
  }
  
  mat <- x$get()
  
  inv <- solve(mat, ...)
  
  x$setInverse(inv)
  
  inv
}
