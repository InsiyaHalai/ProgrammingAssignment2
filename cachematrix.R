## The functions below calculate the inverse of a matrix (if it has not been calculated previously) 
## and then caches its inverse so that it doesn't have to be calculated repeatedly.

## This function creates a matrix which,
## 1. sets the value of the matrix, 
## 2. gets the value of the matrix,
## 3. sets the value of the inverse and 
## 4. gets the value of the inverse

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


## This function together with the above function calculates the inverse of a matrix.
## It first checks if the inverse has already been calculated and if it is present in the cache.
## If it is present in the cache, then the value is returned without the calculation.
## If it is not present in the cache, then the inverse is first computed then returned.

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
