## The pair of functions "makeCacheMatrix" and "cacheSolve"are designed to cache the inverse of a matrix

## Write a short comment describing this function
##makeCacheMatrix creates a special matrix object that can
## which can get inverse for the input by caching

makeCacheMatrix <- function(x = matrix()) {
  
  inver <- NULL
  set <- function(y){
    
    x <<- y
    inver <<- NULL
  }

  get <- function()x
  setInverse <- function(inverse)inver <<- inverse
  getInverse <- function()inver
  list(set = set,get = get, setInverse = setInverse,getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve computes the inverse of matrix
## If the inverse is computed already it will not calculate ut again
## rather it will retrive it from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inver <- x$getInverse()
  if(!is.null(inver)){
    message("getting result from cache")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setInverse(inver)
  inver
}
