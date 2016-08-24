## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 xrow = nrow(x)
 yrow = ncol(x)
 i <- matrix(nrow = xrow, ncol = yrow)
 
 set <- function(y = matrix()){
   x <<- y
   xrow = nrow(x)
   yrow = ncol(x)
   i <- matrix(nrow = xrow, ncol = yrow)
 }
 
 get <- function(){ 
   x
 }
 
 setInverse <- function(inverseMatrix){
   i <<- inverseMatrix
 }
 
 getInverse <- function(){ i }
 
 list(set = set, get = get, 
      setInverse = setInverse, getInverse = getInverse)
  
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!all(is.na(i))){
          message("Cached inversed matrix")
          return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        i
}
