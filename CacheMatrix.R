Makecachematrix <- function    (x = matrix()) {   ## for matrix calculation
  inverse <- NULL
  set <- function(y)
    {
    x <<- y
    inverse <<- NULL
  }
     get <- function() x
     setInv <- function(inverse)         inv <<- inverse          ## for inverse of matrix calculation
     getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv= getInv)
}

CacheSolve <- function(x,...) {
  ## return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) 
    {
    message("getting cached data")                              #3 for printing the inverse of matrix data cashed
    
    return(inv)
  }
  m <- x$get()
  inv <- solve(m, ...)
  x$setInverse(inv)
  inv
}
