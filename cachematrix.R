## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get_matrix <- function() x
  set_inv <- function(inv) inv <<- m
  get_inv <- function() m
  list(set = set, get = get_matrix, set = set_inv, get_inv = get_inv)
  }


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
##If the inverse has already been calculated (and the matrix has not changed), then
##cacheSolve` should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$get_inv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$set(m)
  m
}

