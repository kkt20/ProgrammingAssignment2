## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
#set inv as NULL
  inv = NULL
  
  set = function(y){
    #use <<- operator for assigning x current environment
    x <<- y
    inv <<- NULL
  }
  
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
          message("getting inverse matrix cache data")
          return(inv)
  }
  
        #else calculate inverse
        invMat = x$get()
        inv = solve(invMat)
        x$setInverse(inv)
        inv
}
