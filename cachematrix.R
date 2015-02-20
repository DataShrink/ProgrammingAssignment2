makeCacheMatrix<-function(matrix) {
  
  ## When calling this function, variable 'cachedInverse' is made to contain the cached inverse of
  ## the matrix.  
  ## Since no inverse is calculated or cached yet when you first call this function, the value of
  ## 'cachedInverse' is set to NULL. 
  
  cachedInverse<-NULL
  
  ## the set function is used to assign a new value to the matrix. The 'cachedInverse' is reset
  ## to NULL. The <<- are used because 'cachedInverse' and 'matrix' are outside of the scope of this
  ## function
  set<- function(newValue) {
    matrix<<-newValue
    cachedInverse<<-NULL
  }
  
  ## the get function just returns 'matrix'
  get<-function() matrix
  
  ## the setInverse function is used to set a new value to 'cachedInverse'
  setInverse<-function(inverse) cachedInverse<<-inverse
  
  ## the getInverse function returns 'cachedInverse'
  getInverse<-function() cachedInverse
  
  ## this is what the function makeCacheMatrix() returns, a list of functions
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
}

## The argument 'cacheMatrix' is a special matrix object, made by the function makeCacheMatrix

cacheSolve<- function(cacheMatrix, ...){
  
  ## The value returned by the function getInverse is assigned to the variable 'inverse' 
  inverse<-cacheMatrix$getInverse()
  
  ## this if statement is used to check if there is a value (so no null) assigned to 'inverse'.  
  ## If so, a cached inverse was retrieved and gets returned here. 
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## if 'inverse' is NULL, then the get function will be called (this returns the original matrix)
  ## This value will be assigned to the variable 'matrix'. 
  matrix <- cacheMatrix$get()
  
  ## the function solve is called with the argument 'matrix'. The resulting value is
  ## assigned to 'inverse'. In the next step, the function setinverse is called with 'inverse' as
  ## an argument, so 'inverse' is stored as the cachedInverse in the cacheMatrix object. 
  ## Finally, 'inverse' is returned. 
  inverse <- solve(matrix, ...)
  cacheMatrix$setInverse(inverse)
  inverse
  
}
