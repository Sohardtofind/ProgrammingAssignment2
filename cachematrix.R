## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse.
  
  ## 'inv' stores the cached inverse; NULL means "not yet computed"
  inv <- NULL
  
  ## Set a new matrix value; reset cached inverse because it is no longer valid
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Get the current matrix
  get <- function() {
    x
  }
  
  ## Store the inverse of the matrix in the cache
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## Get the cached inverse (or NULL if not available)
  getinverse <- function() {
    inv
  }
  
  ## Return a list of functions (methods) that share access to x and inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## This function computes the inverse of the special "matrix" object.
  ## If the inverse has already been computed, it returns the cached version.
  
  ## Try to get cached inverse
  inv <- x$getinverse()
  
  ## If cached inverse exists, return it immediately
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Otherwise, compute the inverse and cache it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  ## Return the computed inverse
  inv
}

