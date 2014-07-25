## This function retrives the Cahced matrix inversion instead of generating the Matrix inversion 
##repeatedly.


## makeCacheMatrix function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invs <<- inverse
  getinverse <- function() invs
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix above.
##If the inverse has already been calculated and there is no change in the matric,  cacheSolve 
## will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  invs <- x$getinverse()
  if(!is.null(invs)) {
    message("Retrieve from cache.")
    return(invs)
  }
  data <- x$get()
  invs <- solve(data)
  x$setinverse(invs)
  invs
}
