# The first function, makeCacheMatrix creates a special "matrix", 
# which is really a list containing a function to:
# * set the matrix
# * get the matrix
# * set the inverse matrix
# * get the inverse matrix
makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  get <- function() m
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# The following function calculates the inverse matrix 
# of the special "matrix" created with the above function.
# However, it first checks to see if the inverse matrix 
# has already been calculated. 
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
