# Caching the Inverse of a Matrix:
# The computation of a matrix inversion is usually a costly one, and there may be some
# benefit to caching the inverse of a matrix rather than computing it repeatedly.
# I have written a pair functions below that cache the inverse of a matrix.
# I assume here that the matrix supplied is always invertible.


# This function creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()){
  # initializing the inverse of the matrix as a place holder to be used later
  inv <- NULL
  # function to reset the matrix in the parent environment 
  setMatrix <- function(y){
    x <<- y
    inv <<- NULL
  }
  # fUnction to get the matrix
  getMatrix <- function() x
  # function to set the inverse of the matrix in the parent environment
  setInverse <- function(val) inv <<- val
  # function to get the inverse 
  getInverse <- function() inv
  # a list which will make it possible to access these functions using the $ sign
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
}



# This function computes the inverse of the matrix returned by makeCacheMatrix above.
# If the inverse has already been calculated, then the cached inverse is retrieved.
cacheSolve <- function(x, ...){
  # This first checks whether or not an inverse has been calculated  
  inv <- x$getInverse()
  # If there's an inverse, this retrieves the inverse from the cache
  if(!is.null(inv)){
    message("getting cached data...")
    return(inv)
  }
  # If not, the inverse of the matrix is calculated
  data <- x$getMatrix()
  inv <- solve(data)
  # the calculated inverse is cached
  x$setInverse(inv)
  # and the inverse is printed
  inv
}
