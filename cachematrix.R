## This code entails a pair of functions that cache the inverse of a matrix (a costly computation).
## It's in support of the assignemnet titled - "Caching the Inverse of a Matrix"
## from the 'R Programming' course that's hosted on Coursera, by John Hopkins U

## makeCacheMatrix: A function that creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  ## Computing the inverse of a square matrix can be done with the solve function in R
  ## For example, if X is a square invertible matrix, then solve(X) returns its inverse
  
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: A function that computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    ## if the matrix is in cache, then return 'getting cached data' with the cache & return
    message("getting cached data")  
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
