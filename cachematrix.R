## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##
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
    return(x)
  }
  setInverse <- function(inversedMatrix)
  {
    m <<- inversedMatrix
  }
  getInverse <- function()
  {
    return(m)
  }
 
  functionList = list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  # return the list of functions
  return(functionList)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...)
{
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m))
  {
    message("getting inversed data")
    return(m)
  }
  else
  {
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
  }
  
  return(m)
}
