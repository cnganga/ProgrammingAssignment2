 ##Since Matrix inversion can be a costly computation, to cache the inverse of a matrix helps to shorten data access times therefore reducing latecency and improving input/output.
 ## The two functions that will be used are: MakeCacheMatrix (function that creates a special "matrix" object) and cacheSolve (caches the inverse of an invertible matrix).

 ## The makeCacheMatxrix is a special "matrix", that contains a function to
 ##        1.    set matrix value
 ##        2.    get matrix value
 ##        3.    setinverse value
 ##        4.    getinverse value

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
              # the '<<-' assigns a value to an object in the parent enviroment
                x <<- y
                m <<- NULL
  }
  get <- function () x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}


 ## cacheSolve is a function that gets the inverse of the special "matrix" returned by "makeCacheMatrix"  above. If the inverse has already been calculated and the matrix has not changed, the inverse wil be retrieved from the cache directly.
 ## Otherwise, the inverse of the data will be calculated and the value of the inverse set in the cache through the setinverse function.

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
