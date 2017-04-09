## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
#set inv as NULL
  inv = NULL
  
  set = function(y){
    #use <<- operator for assigning x current environment
    x <<- y  ## Assign, to the parent environment, the value of y to x. This row and the row below will essenatially erase the value stored in the cache if the matrix is a new matrix. 
    inv <<- NULL
  }
  # x and inv are sored in the parent environment. To be able to access these values with the CacheSolve function. Set new objects below
  get = function () x
    setInverse = function(solMatrix){
      inv <<- solMatrix 
    }
    
    getInverse= function() {
      inv
    }
    list(set=set, get=get, setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getInverse()
  
  # if inv is calculated, return inverse
  if(!is.null(inv)){
          message("getting cached data")
          return(inv)
  }
  
        #else calculate inverse
        invMat = x$get()
        inv = solve(invMat) ## Function to solve the inverse of the matrix
        x$setInverse(inv)
        inv
}
