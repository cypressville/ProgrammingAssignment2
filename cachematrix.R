## Put comments here that give an overall description of what your
## functions do

##creating a matrix
makeCacheMatrix <- function( m = matrix() ) {
  ##Initialize the inverse
  inv <- NULL 
  ##To set the matrix
  set <- function( matrix ) {
    m <<- matrix
    inv <<- NULL
  }
  ##To get the matrix, return the matrix
  get <- function() {m}
  ##To set the inverse of matrix
  setInverse <- function(inverse) {inv <<- inverse}
  ##To get the inverse of matrix, return the inverse
  getInverse <- function() {inv}
  ##return a list of methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ##To return the inverse of matrix x
  m <- x$getInverse()
  ##return the same inverse if already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  ##get the matrix
  data <- x$get()
  ##Calculate the inverse
  m <- solve(data)
  ##set the inverse
  x$setInverse(m)
  ##return the matrix
  m
} 
