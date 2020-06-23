## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix)
{
  inv <- NULL  #set an empty inverse matrix
  set <- function(y)  #assign the value of the matrix to x in the parent environment
  {
    x <<- y
    inv <<- NULL   #set an empty inverse matrix in the parent environment
  }

  #gets the value of the matrix
  get <- function() x


  #set the value of the inverse matrix in the parent environment 
  setInv <- function(inverse) inv <<- inverse

  #get the value of the inverse matrix
  getInv <- function() inv
  
  # the following list of clouser functions is returned
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
  #gets the value of the inverse matrix
  inv <- x$getInv()

  # if inverse already exists the its cached value is returned
  if(!is.null(inv))
  {
    message('getting cached data')
    return(inv)
  }
  
  # otherwise , gets the value if matrix
  data <- x$get()
  # and its inverse is calculated
  inv <- solve(data, ...)
  # value of inverse is set/cached
  x$setInv(inv) 
  
  #returns the inverse matrix
  inv
}
