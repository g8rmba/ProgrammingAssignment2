makeCacheMatrix <- function(x = matrix()) {
  # function to set up a static local variable for an inverse matrix
  # calling method <var> <- makeCacheMatrix(<invertableMatrix>)
  #                <var>$get(), <var>$set(),<var>$getmatrix(), <var>$setmatrix()
  # variables x = original matrix, m = inverted matrix
  m <- NULL
  #function to store the original matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # funtion to return the original matrix
  get <- function() x
  # function to store the inverted matrix in the local variable
  setmatrix <- function(solve) m <<- solve
  #function to return the inverse matrix
  getmatrix <- function() m
  #create a list of function names
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

cacheSolve <- function(x, ...) {
  # function to return an inverse matrix
  # the first time the function is called the inverse matrix is solved and cached
  # subsequent calls to the return the cached values
  # calling method <var> <- cacheSolve(<var> as makeCacheMatrix)
  # variables m = static results, data = matrix to be inverted, x = makeCacheMatrix
  m <- x$getmatrix()
  # check to see if first time called and no cached results
  if(!is.null(m)) {
    message("getting cached data")
    # return the cahced inverse matrix
    return(m)
  }
  # no prior results so get matrix to be inverted
  data <- x$get()
  # invert the matrix
  m <- solve(data, ...)
  #store the matrix in the cache location
  x$setmatrix(m)
  # return the inverse matrix
  m
}