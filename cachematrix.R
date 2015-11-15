## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
