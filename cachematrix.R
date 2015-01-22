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
  
  setmean <- function(solve) m <<- solve
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## cacheSolve: A function that computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
