## Create a matrix and calcuate its inverse. Cache the matrix data so it may be 
## used externally and save computation time

makeCacheMatrix <- function(x = matrix()) {

  ## @x: an invertible matrix that is has equal length rows and columns
  ## return back a list containing functions to perform the following matrix operations
  ##  a. set the matrix
  ##  b. get the matrix
  ##  c. set the inverse
  ##  d. get the inverse
  ## use this list as input for the cacheSolve() function
  
  inv = NULL
  set = function(y) {
    # the special operator `<<-` is used to assign a value to an object in an environment 
    # that is not the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## Retrieve the inverse matrix calculation to solve the problem if possible

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  
  # check if the inverse has been calculated
  if (!is.null(inv)){
    # retrieve the cached data and avoid the computation.
    message("retrieving stored data")
    return(inv)
  }
  
  # if necessary, calculate the inverse of the matrix
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # cache the inverse matrix data object using the setinv() function 
  
  x$setinv(inv)
  
  return(inv)
  
}
