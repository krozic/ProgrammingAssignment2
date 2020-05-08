## Calculating the inverse of a matrix can be a costly operation.
## If an inverse is recquired for multiple operations the inverse
## can be "cached" by storing it in a function environment 
## that can be recalled from memory at a later time.

## The makeCacheMatrix() function takes a matrix 'x' as an input
## and returns a special matrix that contains the set(), get(),
## setinv(), and getinv() functions to be used to recall and/or
## store the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv_m <<- inverse
  getinv <- function() inv_m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve() function takes the output of makeCacheMatrix()
## as an input argument. If !is.null(inv_m) is TRUE, the function 
## retrieves the 'inv_m' value from the parent environment and 
## returns the value. 
## If !is.null(inv_m) is FALSE, the matrix is retrieved
## with x$get(), the inverse is calculated and stored in 'inv_m', 
## and the value of inv_m is stored in the parent environment with
## x$setinv(inv_m). The function then returns inv_m.
## To change the matrix to a 'newmatrix' without calling 
## makeCacheMatrix() again, myMatrix$set(newmatrix) can be called to store
## the new matrix in the parent environment.

cacheSolve <- function(x, ...) {
  inv_m <- x$getinv()
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  data <- x$get()
  inv_m <- solve(data, ...)
  x$setinv(inv_m)
  inv_m
}
