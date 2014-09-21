## Seiji Armstrong 21/09/2014
## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.
## cacheSolve is a function that computes the inverse of the special "matrix" returned by makeCacheMatrix.

## makeCacheMatrix is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
## It was heavily inspired by the makeVector example on this course's assignment page.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get - get, 
       setinv - setinv, getinv = getinv)

}


## cacheSolve first checks to see if an inverse of the "matrix" has been calculated. 
## If yes, it retrieves it from the cache.
## If not, it calculates the inverse. 
## Again, largely a remix of cachemean, the example function on the Assignment 2 page.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(xInv)){
    message("getting cached data")
    return(xInv)
    xInv <- solve(x)
  } 
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
