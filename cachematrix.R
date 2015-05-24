# A function for generating matrices with a cached inverse operation (ehanced matrix)
# and a function returning the inverse supporting an invisible cache access are provided here


# Creates an enhanced matrix supporting cached inverse operation.
# Initialized the matrix content with the vector x, if provided, otherwise it will be
# initialized with an empty matrix
#
# The inverse matrix cache is initially uninitialized and return in the uninitialized state
# whenever the matrix content is overwritten
# The matrix supports the following methods
# * set : set the matrix and reset the inverse
# * get : set the matrix
# * setInverse : set the inverse matrix
# * getInverse : set the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(newInverse) inverse <<- newInverse
  getInverse <- function() inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# Return the inverse of the enhanced matrix x. If the inverse matrix has not been cached,
# the inverse is computed, cached and returned. Apart from the first parameter (containing
# the enhanced matrix) other paramaters will be passed to the function solve for inverting
# the matrix
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
