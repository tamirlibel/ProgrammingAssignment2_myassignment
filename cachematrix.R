## creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function (x = matrix()) {
    ## x: a square invertible matrix
  ## return: a list containing functions to:
  ## 1. set the matrix (x)
  ## 2. get the matrix  (x)
  ## 3. set the inverse (m)
  ## 4. get the inverse (m)
  ## this list is used as the input to cacheSolve()
  inv = NULL
  set = function(y) {
    ## use '<<-' to assign a value to an object in environment 
    ##different than the current environment
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinverse = function(inverse) inv <<- inverse
  getinverse = function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  ##x: output of makeCacheMatrix()
  ##return: inverse of the original square invertible matrix
  inv = x$getinverse()
  ##if the inverse has already been calculated
  if(!is.null(inv)) {
    ##get it from the cache and skip the calculation
    message("getting cached data")
    return(inv)
  }
  ##otherwise, caculating the inversed matrix
  mat = x$get()
  inv = solve(mat, ...)
  ##set the value of the inversed matrix in the cache via the setm function
  x$setinverse(inv)
  inv
}