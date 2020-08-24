## Put comments here that give an overall description of what your
## functions do

##Special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  invMatrix <- NULL
  
  set <- function(actualMatrix)
  {
    x <<- actualMatrix 
    invMatrix <<- NULL  
  }
  
  ## Get input matrix
  get <- function() x
  setInV <- function(inverse) invMatrix <<- inverse
  
  ## Get inverse matrix
  getInv <- function() invMatrix
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


##Compute the inverse of the special "matrix" 

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInv()
  
  if (!is.null(invMatrix))
  {
    #If exits, we get cached instead of recalculating it
    message("cached inverse")
    return(invMatrix)
  }
  
  #if not exists, the inverse matrix is be calculated
  
  newMatrix <- x$get()
  invMatrix <- inverse(newMatrix)
  x$setInv(invMatrix)
  invMatrix
}

