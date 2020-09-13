## The pair of functions calculate the inverse of a matrix. 
## The functions cache the inverse of a matrix for repeated use.

## Creates a list of function that set/get the matrix and set/get its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setresolver <- function(solve) inv <<- solve
  getresolver <- function() inv
  list(set = set, get = get,
       setresolver = setresolver,
       getresolver = getresolver)
}


## Calculates the inverse of the special "matrix" created with the above function. Returns the inverse from cache if the inverse was calculated before

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getresolver()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setresolver(inv)
  inv
}
