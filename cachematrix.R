## This functions store a given matrix in a special list in
## order to store it's inverse in cache instead of calculating
## it over and over again.

## Creates the list with four functions

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


## Checks if the inverse has allready been calculated. If not,
## the function calculates and stores it.

cacheSolve <- function(x, ...) {
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
