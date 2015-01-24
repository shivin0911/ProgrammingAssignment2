  ## SHIVIN SINGH 
  ## 25-01-2015
  ## makeCacheMatrix 
  ## 1. Initializes a variable 'inv' 
  ##    (which will be used to save inverse matrix latter, i.e. a cached data);
  ## 2. Provides function get() to obtain "raw" matrix (of which one needs to find 
  ##    its inverse);
## 3. Provides function setinv() to assign computed inverse matrix (of x) to inv;
## 4. Provides function getinv() to obtain the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function(){ x}
  setinv <- function(inverse){ inv <<- inverse}
  getinv <- function() {inv}
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## calculating the inverse of the matrix
##also checks whether the inverse has already been calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {                # cached inverse found
    message("getting cached data")
    return(inv)
  }
  message("calculating for the first time")
  data <- x$get()
  inv <- solve(data, ...)           #  finds inverse of the matrix
  x$setinv(inv)
  inv                                #  returning the obtained matrix
}
