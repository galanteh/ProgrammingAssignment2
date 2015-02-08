## Hernan Galante - Programming Assignment 2
## Feb 07, 2015
## Computing the inverse of a square matrix can be done with the `solve`
## function in R. For example, if `X` is a square invertible matrix, then
## `solve(X)` returns its inverse.

# How to use it. Example:
# Remember, to inverse a matrix, we need a squared matrix
# > m <- matrix(1:4, 2,2) 
# > mcm <- makeCacheMatrix(m)
# > cacheSolve(mcm) # first time message
# > cacheSolve(mcm) # cached message


## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(originalMatrix = matrix()) {
  inverseMatrix <- NULL
  setMatrix <- function(y) {
                  originalMatrix <<- y
                  inverseMatrix <<- NULL
                }
  getMatrix <- function() originalMatrix
  setInverseMatrixOnCache <- function(im) inverseMatrix <<- im
  getInverseMatrixOnCache <- function() inverseMatrix
  list(setMatrix = setMatrix, 
      getMatrix = getMatrix,
      setInverseMatrixOnCache = setInverseMatrixOnCache,
      getInverseMatrixOnCache = getInverseMatrixOnCache)
}
  
## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # get the inverse from cache
  inverseMatrix <- x$getInverseMatrixOnCache()
  # if we have it in cache, we return it
  if(!is.null(inverseMatrix)) {
      message("getting cached inverse matrix")
      return(inverseMatrix)
  }
  # otherwise, we generate the inverse
  # to do so, we get the original value of the matrix
  matrix <- x$getMatrix()
  # inverse the matrix with the solve function
  inverseMatrix <- solve(matrix)
  # store it on cache
  x$setInverseMatrixOnCache(inverseMatrix)
  # return
  message("inverse matrix resolved for first time")
  inverseMatrix
}
  

