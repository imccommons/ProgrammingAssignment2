## These pair of functions cache the inverse of a matrix, 
## first by creating a special "matrix" object which caches its own inverse,
## then computes the inverse of the special "matrix" returned.
## If inverse is calculated with no changes to the matrix,
## the inverse should be retrieved from the cache

## The "makeCacheMatrix" function creates “matrix” object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) m <<- inverse
     getinverse <- function() m
     list(set = set, get = get, 
          setinverse = setinverse, 
          getinverse = getinverse)
}


## The "cacheSolve" function computes the inverse of the “matrix”  
## from the input is returned by "makeCacheMatrix."

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}
