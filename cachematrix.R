## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  matrix_inv <- NULL
  set <- function(y) {
    x <<- y
    matrix_inv <<- NULL
  }
  get <- function() x
  set_matrix_inv <- function(inverse) matrix_inv <<- inverse
  get_matrix_inv <- function() matrix_inv
  list(set = set, get = get,
       set_matrix_inv = set_matrix_inv,
       get_matrix_inv = get_matrix_inv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrix_inv <- x$get_matrix_inv()
  if(!is.null(matrix_inv)) {
    message("getting cached data")
    return(matrix_inv)
  }
  data <- x$get()
  matrix_inv <- solve(data)
  x$set_matrix_inv(matrix_inv)
  matrix_inv
}
