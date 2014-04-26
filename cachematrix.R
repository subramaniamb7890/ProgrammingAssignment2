## The two functions written in this code help cache the inverse of a matrix. 
## Since the inverse of a matrix is a costly operation, caching it solves several constraints, 
## instead of calculating it repeatedly. 

## The makeCacheMatrix does the functions of getting a matrix, setting a matrix, getting the inverse of a matrix 
## and setting the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinverse <- function(inverse) i<<-inverse
  getinverse <- function() i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The cacheSolve function frst checks if the inverse of the matrix has already been calculated. 
## If it has, it returns the result. If not, it calculates the matrix inverse and returns the result.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  i_data <- x$get()
  i <- solve(i_data, ...)
  x$setinverse(i)
  i
}
