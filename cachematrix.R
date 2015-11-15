## The functions makeCacheMatrix and cacheSolve work in conjunction with each other to calculate 
## the inverse of a matrix and store it in cache after it has been calculated once. 
##
## Usage Example:
## > m <- matrix(1:4, 2, 2)
## > M <- makeCacheMatrix(m)
## > cacheSolve(M)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(M)
## getting cached inverse matrix
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## makeCacheMatrix is a closure for a matrix that keeps in cache the matrix itself 
## together with its calcluated inverse matrix. It does this by defining functions  
## inside the overall makeCacheMatrix function and assigning variables within those sub-functions 
## so that they are maintained in the envirnment of makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverse) invMatrix <<- inverse
  getInverseMatrix <- function() invMatrix
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix, 
       getInverseMatrix = getInverseMatrix)
    
}

## cacheSolve takes a matrix that was constructed by the makeCacheMatrix function
## and returns the inverse of that matrix. The calculation of the inverse matrix happens only once
## and the inverse matrix is stored in cache. Subsequent calls simply retrieve the cached value without 
## recalculating it.
cacheSolve <- function(x, ...) {
  inverse = x$getInverseMatrix()
  if(!is.null(inverse)) {
    message("getting cached inverse matrix")
    return (inverse)
  }
  matr <- x$get()
  inverse <- solve(matr)
  x$setInverseMatrix(inverse)
  inverse
}
