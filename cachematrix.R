## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  if (is.matrix(x)) {
    if (nrow(x)==ncol(x)){
      invm <- NULL
      set <- function(y) {
        x <<- y
        invm <<- NULL
      }
      get <- function() x
      setinv <- function(invm) invm <<- solve(x)
      getinv <- function() invm
      list(set = set, get = get,setinv = setinv,getinv = getinv)}
    else
      message('not a square matrix')}
  else
    message('not a matrix')
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m1=x$get()
  if (is.matrix(m1)) {
    if (nrow(m1)==ncol(m1)){
      invm <- x$getinv()
      if(!is.null(invm)) {
        message("getting cached data")
        return(invm)
      }
      data <- x$get()
      invm <- solve(data)
      x$setinv(invm)
      invm
    }
    else
      message('not a square matrix')}
  else
    message('not a matrix')
}
