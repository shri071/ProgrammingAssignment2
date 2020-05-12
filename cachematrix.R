## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set<-function(y){
      x<<-y                                                 #input x can be reset
      i<<-NULL                                              #inverse is set to null
    }
    get<-function()x                                        #returns matrix x
    setInverse<-function(inverse) i<<-inverse               #sets inverse to i
    getInverse<-function()i                                 #returns the inverse
    list(set = set, get = get,                               
         setInverse = setInverse,
         getInverse = getInverse)
  }
  
  
  




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    i<-x$getInverse()                 #retrives inverse of matrix and assigns to i
    if(!is.null(i)){                  #if result is false get inverse
      message("getting inverse")
      return(i)
    }
    data<-x$get()
    i<-solve(data, ...)             
    x$setInverse(i)
    i
  }
  
  ## Return a matrix that is the inverse of 'x'

