## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#set value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solveMatrix) inverse <<- solveMatrix
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above.
## If the inverse has already been calculated, (NULL= False), then the inverse is retrieved from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setinverse(inverse)
  inverse
}
