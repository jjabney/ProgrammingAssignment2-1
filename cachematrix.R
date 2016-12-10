#This function takes an invertible matrix that provides it's inverse value
makeCacheMatrix <- function(x=matrix()){
  
  inv = NULL
 
 set = function(y){
   x <<- y
   inv <<- NULL
 }
 
 get = function() x
 
 setinv = function(inverse) inv <<- inverse
 
 getinv = function() inv
 
 list(set=set,get=get,setinv=setinv,getinv=getinv)
  
}


 #This function takes a makeCacheMatrix as it's input and caches the calcuated inverse 
cacheSolve <- function(x, ...) {

  
  inv = x$getinv()

  if (!is.null(inv)){
    return(inv)
  }

  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  return(inv)
}

