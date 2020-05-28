## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function( m = matrix() ) {
  inv <- NULL 
  set <- function( matrix ) {
    m <<- matrix
    inv <<- NULL
  }
  get <- function() {m}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
} 
