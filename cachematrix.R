## This code contains two main functions:
##The first function ("makeCacheMatrix") creates a creates a special "matrix" object that can cache its inverse.
##The second function computes the inverse of the special "matrix" returned by the first function.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
##
##1.makeCacheMatrix 
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##2.cacheSolve
##The following function calculates the mean of the special "vector" created with the 
##above function. However, it first checks to see if the mean has already been calculated. 
##If so, it gets the mean from the cache and skips the computation. Otherwise, 
##it calculates the mean of the data and sets the value of the mean in the cache via the
##setmean function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  gi <- x$getInverse()
  if(!is.null(gi)) {
    message("getting cached data")
    return(gi)
  }
  data <- x$get()
  gi <- solve(data, ...)
  x$setInverse(gi)
  gi
}
