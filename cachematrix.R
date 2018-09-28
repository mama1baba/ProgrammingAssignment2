## These two functions are cache the inverse of a matrix to avoid costly computation

## This function creates a special "matrix" object that can cache its inverse...

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of special 'matrx'
## returned by the makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)){
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}


matrix_example <- matrix(c(1:4), nrow = 2, ncol = 2)

matrix_example

cacheSolve(makeCacheMatrix(matrix_example))
