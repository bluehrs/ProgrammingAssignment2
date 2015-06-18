## Create a special object that stores a numeric matrix and cache's its inverse.

## Creates a special vector to store four functions that cache the inverse
## of a matrix x (and whose inverse can be solved in cacheSolve).
## The four functions:
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of the numeric matrix stored in x (object created
## by makeCacheMatrix that includes a numeric matrix and four functions
## that allow caching) by first checking if the inverse is already cached
## and if not then solving and storing the cached matrix in x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
