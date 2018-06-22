## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates an object, which is a matrix, that stores its inverse into the cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  seti <- function(inverse) i <<- inverse
  geti<- function() i
  list(set = set, get = get,
       seti = seti,
       geti = geti)
}



## Write a short comment describing this function
## The second function checks in the cache if the inverse of the matrix exists, and if it does, the function
## takes it from the cache. If the inverse of the matrix doesn't exist, the function creates it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$geti()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$seti(i)
  i
}

# Test for the functions
a = rnorm(1:9)
a = matrix(a, 3, 3)
a
ab <- makeCacheMatrix(a)
cacheSolve(ab)
