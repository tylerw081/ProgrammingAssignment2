## The following functions are used to cache the inverse of a given matrix and 
## allow the user to retrieve the information later without the need to recalculate..

## makeCacheMatrix takes the input of a matrix and creates a special vector which contains
## a list of functions to get and set the value of the vector and get and set the value of
## the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrix <- function(solve) m <<- solve
  getMatrix <- function() m
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)

}


## cacheSolve will input the makeCacheMatrix object and solve for the inverse of the matrix 
## if that has not already been calculated. The results are then stored so it will not need
## to calculate again.

cacheSolve <- function(x, ...) {
    m <- x$getMatrix()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setMatrix(m)
    return(m)
}


