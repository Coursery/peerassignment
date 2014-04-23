## Function taking invertible matrix as argument creating a matrix of which 
## the values and the inverse can be (re-)set by subsetting the functions set(), and setInverse() 
## (e.g. a$set(matrix(5:8.2)))and values of the matrix and its inverse can be returned with the functions
## get() and getInverse()
## subsetting of functions is made possible through list created at end of function 

makeCacheMatrix <- function(matrix=matrix()){
  
  inverse <- NULL
  set <- function(y) {
    matrix <<- y
    inverse <<- NULL }
  get <- function() matrix
  setInverse <- function(inv) inverse<<- inv
  getInverse <- function () inverse
  list (set=set, get=get,
        setInverse=setInverse,
        getInverse=getInverse)
}

## function taking 'special' matrix as argument,
## checks if inverted matrix is cached, if yes, returns cached data,
## if not, computes inversed matrix and prints it

cacheSolve <- function(matrix, ...) {
  inverse <- matrix$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)  
  }
  else{
    data <- matrix$get()
    inverse <- solve(data, ...)
    matrix$setInverse(inverse) 
    inverse
  }
}
