#The functions makeCacheMatrix and CacheSolved are used to create a special matrix and cache its inverse. 
#If inverse has already been calculated, the inverse is retrieved.
#makeCacheMatrix creates a special matrix that can cache its inverse.
 
makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  setmat <- function(y) {
    m <<- y
    i <<- NULL
    }
  getmat <- function() m
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(setmat = setmat,
       getmat = getmat,
       setinv = setinv,
       getinv = getinv)
}

#Calculates the inverse of the matrix returned by function makeCacheMatrix. 
#IF inverse has already been calculated: retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if (!is.null(i)) {
    message("getting cached matrix")
    return(i)
    }
  data <- x$getmat()
  im <- solve(data, ...)
  x$setinv(im)
  im
}