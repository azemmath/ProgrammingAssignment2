## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # check if the matrix was submitted
  if (is.matrix(x)) {
    # check if the matrix is square
    if (nrow(x)==ncol(x)){
      invm <- NULL
      set <- function(y) {
        x <<- y
        invm <<- NULL
      }
      # create an object containing an original matrix, an inverse if cached and set and get functions
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
  # get the matrix which needs to be inverted
  m1=x$get()
  # check if it is a matrix
  if (is.matrix(m1)) {
    # check if it is square
    if (nrow(m1)==ncol(m1)){
      invm <- x$getinv()
      # check if the inverse was cached
      if(!is.null(invm)) {
        message("getting cached data")
        return(invm)
      }
      #compute the inverse if not cached
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
