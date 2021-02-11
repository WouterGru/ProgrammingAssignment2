## Function 1 (makeCacheMatrix) sets an entered matrix in Cache. 
## Function 2 (cacheSolve) computes the inverse of the Matrix entered in makeCacheMatrix

## The function makeCacheMatrix is used to store a Matrix in Cache.
## Line 12 until 15 set the value of the Matrix
## Line 16 gets the value of the Matrix
## Line 17 sets the inverse of the Matrix
## Line 18 gets the inverse of the Matrix, which is set in a list in line 19 until 22

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## The function cacheSolve is used to show the inverted matrix stored in cache 
## The function first checks in an inverse has been calculated (Line 34 until 37)
## If not, it calculates the invert of the data and sets it in the cache.
## Line 42 prints the invert
## If the invert is already calculated, it prints the message "getting cached data"

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}