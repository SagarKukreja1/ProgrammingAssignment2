##Assignment 2: Caching the Inverse of a Matrix

##This programs assumes that the input matrix is invertible

##The ultimate goal of this program is to save valuable computational time by caaching the inverse of a 
##matrix, so there is is no need to recalculate it over and over.

##makeCacheMatrix takes an invertible matrix (x) as its argument and creates a list of 4 functions: getMatrix, setMatrix, getInverse, setInverse.
##This function will be passed through another function (cacheSolve) to determine whether an inverse for the x has already been calculate--and
##thus returns the inverse--or calculates the inverse and caches it.
makeCacheMatrix <- function(x = matrix()) {
  ##First, we initialize the object inv in the parent frame (the makeCacheMatrix function)
  ##This is done so the setInverse function can assign a value to the object inv in 
  ##the parent frame.
  inv <- NULL
  
  ##The setMatrix function allows one to change the special matrix and simultaneously 
  ##reset the 'inv' object to NULL. Resetting the object to NULL is essential if an inverse for 
  ##the previous matrix has already been cached. Otherwise, cacheSolve will return the inverse
  ##for the previous matrix and not the new matrix. The matrix can be returned using getMatrix()
  setMatrix <- function(y = matrix()){
    x <<- y
    inv <<- NULL
  }
  
  getMatrix <- function() {x}
  
  
  setInverse <- function(inverse){
    inv <<- inverse
  }
  
  
  getInverse <- function() inv
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       setInverse = setInverse, getInverse = getInverse)
}

## This function calculates the inverse of the matrix x,stores it in the object 'inv' --which resides in makeCacheMatrix-- and returns the inverse.
## However, the function first checks to see if a non NULL value has already been assigned to 'inv'. If this is true, the function returns
## The value of 'inv'. Warning: if the user decides to manually assign a value to inv using the setInverse  function, cacheSolve may not return 
## the correct inverse of the matrix!
cacheSolve <- function(x,...){
  ##x$getInverse will return NULL if the inverse was not previously calculated
  inv <- x$getInverse()
  
  ##logical test to see if an "inverse" (see above warning) has already been calculated. If yes, the function returns the inverse.
  if(!(is.null(inv))){
    message("Retrieving the inverse of your matrix from the cache...")
    message("(Warning: if inverse manually assigned via setInverse function, may be incorrect)")
    return(inv)
  } 
  ##If the inverse of the matrix is not in the cache, cacheSolve uses the "solve()" function to 
  ## compute the inverse of the special matrix and assigns it to 'inv'. 
  data <- x$getMatrix()
  inv <- solve(data)
  x$setInverse(inv)
  return(inv)
}
