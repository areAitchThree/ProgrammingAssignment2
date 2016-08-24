## Taken together, these functions provide an efficient way of dealing with matrices wherein the inverse is
## expected to be repeatedly referenced. We'd like to avoid the calculation cost inherent in the operation.

## This function creates a matrix wrapper, which is really a list containing functions to:
## 	"set" set the value of the underlying matrix
##	"get" get the value of the underlying matrix
##	"setinv" set and cache the value of the underlying matrix's inverse
##	"getinv" get the value of the matrix's inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function calculates the inverse of the matrix wrapper created with the makeCacheMatrix function. However, 
## it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
} ## Return a matrix that is the inverse of 'x'

