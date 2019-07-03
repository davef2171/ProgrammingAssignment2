## Twin functions to a) create a complex object which contains a variable
## structure of a matrix, its inverse and two operators set, get for the value and its inverse value; and
## b) to return either a valid cached value or in the case on no valid cache set it.
## Limitations - Matrix must be invertible

## makeCacheMatrix create a complex variable and the operators which work on it.
## values - a matrix and its inverse
## operators - set, get matrix;  setinverse, getinverse on the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Presumes x has been created with makecacheMatrix
## Then verifies the cached inverse value returning it if valid and, if not sets it

cacheSolve <- function(x, ...) {
  ## x is a Cached Matrix 
  ## Return a matrix that is the inverse of matrix in 'x'
  ## Check if there is an inverse
  data <- x$get()
  inv <- x$getinverse()
  
  if (!is.null(inv) ){
    
    ## Still same values and inverse is cached
    return(inv)
  }
  
  ## cache is invalid or not set
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
