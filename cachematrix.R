## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.
## <<- operator is used to assign a value to an object in an environment that is
## different from the current environment 


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL          ## initialy inverse Matrix as NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x  ## get the value of the matrix
  setInverse <- function(solveMatrix) inv <<- solveMatrix ## set the value of the invertible matrix
  getInverse <- function() inv                           ##get the value of the invertible matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
}


## The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
# input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
# In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
# and set the invertible  matrix by using the solve function.
# In case inverse matrix from makeCacheMatrix((matrix) has some value in it (always works
#after running the code 1st time), it returns a message  "Getting Cached Invertible Matrix" 
#and the cached object

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){        ##if inverse matrix is not NULL
    message("getting cached data")  ##Type message: Getting Cached Invertible Matrix 
    return(inv)       ##return the invertible matrix
  }
  data <- x$get()     ##get the original Matrix Dat
  inv <- solve(data)  ##use solve function to inverse the matrix
  x$setInverse(inv)   ##set the invertible matrix 
  inv                 ##return the invertible matrix
}


### Testing Final Funtions ###

Matrix <- matrix(1:4,2,2)
Matrix

CMatrix <- makeCacheMatrix(Matrix)
CMatrix$get()
CMatrix$getInverse()

cacheSolve(CMatrix)




