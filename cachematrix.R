## In order to save time and computing, these functions used together
## cache the inverse of a matrix so it does not have to be computed more than once.

## makeCacheMatrix is a function that creates special "matrix" that can cache
## the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsol <- function(sol) s <<- sol
  getsol <- function() s
  list(set = set, get = get,
       setsol = setsol,
       getsol = getsol)
}

## cacheSolve is a function that computes the inverse of the "matrix" returned from
## makeCacheMatrix and returns this inverse. If this inverse has already been solved,
## then cacheSolve retrieves this inverse from the cache instead of computing it.

cacheSolve <- function(x, ...) {
  s <- x$getsol()
  if (!is.null(s)){
    message("getting cached data")
    return(s)
  }
  else{
    data <- x$get()
    s <- solve(data,...)
    x$setsol(s)
    return(s)
  }
}
