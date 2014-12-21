## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(cacheSolve) m <<- cacheSolve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) { ## If the inverse has already been calculated
    message("getting cached data")
    return(m) ## Return cache
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
