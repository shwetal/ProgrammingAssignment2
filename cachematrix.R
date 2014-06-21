## Put comments here that give an overall description of what your
## functions do
##### makeCacheMatrix: initializes a list having getter and setter functions that set and get matrix and
# its inverse values. Each set is stored in a separate environment. It can be thought of as symbols
# having scope of calling function.
##### cacheSolve: first searches for inverse of an matrix in the memory / cache / environments of calling
# functions and returns it if found. If not, it computes the inverse and then saves it in the cache so
# that next time when the inverse is asked for it can be taken for memory.


## Write a short comment describing this function
# This function defines the getters and setters for the matrix and its inverse
# This function create a new matrix and inverse value in different environments
# giving an effect of global array of matrix and inverse pairs

makeCacheMatrix <- function(primaryMatrix=matrix()) {
  #initiate the inverse to null
  inverseMatrix <- NULL 
  
  # getter and setter function definitions
  setMatrix <- function(inputMatrix) {
    primaryMatrix <<- inputMatrix
    inverseMatrix <<- NULL
  }
  getMatrix <- function() {
    primaryMatrix
  }
  
  setInverse <- function(inverse) {
    inverseMatrix <<- inverse
  } 
  
  getInverse <- function() {
    inverseMatrix
  }
  
  # retrun a list of functions. This list sets values in separate environments when set is called
  list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse = setInverse, getInverse = getInverse)
} # end of makeCacheMatrix function




## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
# This function checks to see if the inverse of x exists in global environment
# If inverse exists, it returns it if not then it computes the inverse
# and sets it in the global environment

cacheSolve <- function(input, ...) {
  # Get inverse of the input matrix and return if available in one of the environments (cache)
  matrixInverse <- input$getInverse()
  if(!is.null(matrixInverse)) {
    message ("getting cached data")
    return(matrixInverse)
  }
  
  # if inverse is not found in the memory (cache), compute it and store it in global memory
  # using a different symbol for better readability.
  data <- input$getMatrix()
  inverseMatrix <- solve(data, ...)
  input$setInverse(inverseMatrix)
  inverseMatrix
} # end of cacheSolve function
