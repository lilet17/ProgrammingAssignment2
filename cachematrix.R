## write a pair of functions that cache the inverse of a matrix.
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inversefunc <- NULL
  set <- function(y) {
    x <<- y
    inversefunc <<- NULL
  }
  get <- function() x
  setinversefunc <- function(inverse) inversefunc <<- inverse
  getinversefunc <- function() inverse
  list(set = set, get = get,
       setinversefunc = setinversefunc,
       getinversefunc = getinversefunc)
}


## This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inversefunc <- x$getinverse()
  if(!is.null(inversefunc)) {
    message("getting cached data")
    return(inversefunc)
  }
  data <- x$get()
  inversefunc <- solve(data, ...)
  x$setinverse(inversefunc)
  inversefunc
}
