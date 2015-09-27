##Programming Assignment 2: Caching the Inverse of a Matrix

##For this assignment, we were instructed to write a pair of functions that 
##cache the inverse of a matrix.  Because matrix inversion is usually a costly computation,
##caching the inverse of a matrix rather than computing it repeatedly may be benificial.

##The first function, makeCacheMatrix, creates a special "matrix" object that can cache its inverse.
##This special "matrix" is really a list containing a function to 
##    1. set the matrix
##    2. get the matrix
##    3. set the inverse
##    4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
  matrixinverse <- NULL
  set <- function (y) {
    x <<- y
    matrixinverse <<- NULL
  } 
  get <- function () x
  setInverse <-function (inverse) matrixinverse <<- inverse
  getInverse <- function () matrixinverse
  list (set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


##The second function, cacheSolve, computes the invers of the special "matrix" returned by 
##makeCacheMAtrix, the function created above.  If the inverse has already been calculated --and the matrix 
##has not changed-- then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  matrixinvers <- x$getInverse()
  if (!is.null(matrixinverse)) {
    message ("getting cached data")
    return (matrixinverse)
  }
  matrixdata <- x$get()
  inversematrix <- solve(matrixdata, ...)
  x$setInverse(matrixinverse)
  matrixdata
}
