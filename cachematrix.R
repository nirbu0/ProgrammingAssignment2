## The code below uses a pair of functions that cache the inverse of a matrix.

##This function(makeCacheMatrix) creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())
{
  m <- NULL
  
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  
  get <- function()
  {
    x
  }
  
  setinvmatrix <- function(inverse)
  {
    m <<- inverse
  }
  
  getinvmatrix <- function()
  {
    m
  }
  
  list(set = set, get = get, setinvmatrix = setinvmatrix, getinvmatrix = getinvmatrix)
}


## This function(cacheSolve) computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...)
{
  m <- x$getinvmatrix()
  
  if(!is.null(m))
  {
    message("cached data used")
    return(m)
  }
  else
  {
    data <- x$get()
    m <- solve(data, ...)
    x$setinvmatrix(m)
    m
  }    
}
