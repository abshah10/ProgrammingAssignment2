## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  get <- function(){x}
  setinv <- function(solve) {invr <<- solve}
  getinv <- function() {invr}
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invr <- x$getinv()
  if(!is.null(invr)) {
    message("getting cached data")
    return(invr)
  }
  data <- x$get()
  invr <- solve(data, ...)
  x$setinv(invr)
  invr
}
