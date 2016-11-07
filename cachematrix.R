## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
