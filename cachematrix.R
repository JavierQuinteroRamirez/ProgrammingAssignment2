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
  
  get <- function() x
  setInV <- function(inverse) invMatrix <<- inverse
  getInv <- function() invMatrix
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


##Compute the inverse of the special "matrix" 

cacheSolve <- function(x, ...) {
  
  invMatrix <- x$getInv()
  
  if (!is.null(invMatrix))
  {
    message("cached inverse")
    return(invMatrix)
  }
  
  newMatrix <- x$get()
  invMatrix <- inverse(newMatrix)
  x$setInv(invMatrix)
  invMatrix
}

