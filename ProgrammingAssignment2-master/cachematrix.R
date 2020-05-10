## In this program we will write a function for Caching the Matrix Inversion rather than compute it repeatedly
## For this we will use two functions
## makeCachematrix --> creates a special "matrix" object that can cache its inverse
## cacheSolve --> computes the inverse of the special “matrix” returned by makeCacheMatrix
makeCachematrix <- function(x = matrix()) {
  p <- NULL
  set <- function(y) {
    x <<- y
    p <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) p <<- inverse
  getinverse <- function() p
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  p <- x$getinverse()
  if(!is.null(p)) {
    message("getting cached data")
    return(p)
  }
  l <- x$get()
  p <- solve(l, ...)
  x$setinverse(p)
  p
}
