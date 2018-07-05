## These functions save a matrix as a 'CacheMatrix' in the cache. Once its inverse is calculated by the function cacheSolve, 
## it saves its inverse, and no (time consuming) recalculation is needed when this function is recalled. 

## This function saves a copy of given matrix in the cache, and if known, saves its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function returns the inverse of a 'CacheMatrix'. If the inverse is already known, there is no need recalculating the inverse. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
