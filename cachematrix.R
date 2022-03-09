## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## library(MASS) for calculating inverse of non squared and squared matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
                    x <<- y
                    inv <<- NULL
                     }
  get <- function()x              ##function to get matrix x
  setinv <- function(inverse)inv <<- inverse
  getinv <- function(){
                      inver <-ginv(x)
                      inver%*%x       ##function to get the inverse of matrix
  }  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function


cacheSolve <- function(x, ...)     ## getting the cache data
  {
  inv <- x$getinv()
  if(!is.null(inv)){             ## controlling wether inverse is NULL
                    message("Getting cached data")
                    return(inv)            ## gives inverse value
  }
  data <- x$get()
  inv <- solve(data, ...)       ## calculates inverse value
  x$setinv(inv) 
  inv    ## Return a matrix that is the inverse of 'x'
}
f <- makeCacheMatrix(matrix(1:8,2,4))
f$get()
f$getinv()
cacheSolve(f)