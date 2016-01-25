## This function creates a special "matrix" object that can cache its inverse
## Specifically,it contains a list of functions: set, get, setinv and getinv
## set assigns values to a matrix "x"
## get returns the matrix "x"
## setinv can be used to overwrite or assign values to inv_x, the inverse of the matrix "x"
## getinv returns the inverse of the matrix "x"

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv_x <<- solve
  getinv <- function() inv_x
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above 
## If the inverse has already been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.
## the Solve function is part ofthe base R library that inverts a matrix (assuming invertability)

cacheSolve <- function(x, ...) {
  inv_x <- x$getinv()
  if(!is.null(inv_x)) {
    message("getting cached data")
    return(inv_x)
  }
  data <- x$get()
  inv_x <- solve(data, ...)
  x$setinv(inv_x)
  inv_x
}
