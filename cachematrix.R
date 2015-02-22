## Programming assignment - Andreas Hoelzl

## Given the matrix object x create a "special matrix object", i.e. list of functions to get and set the matrix object and to  getinverse and setinverse the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) cache <<- inv
  getinverse <- function() cache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculate the inverse of the given "special matrix object". Use a cached version if exists 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
