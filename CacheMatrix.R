Makecachematrix <- function    (x = matrix()) {
  inverse <- NULL
  set <- function(y)
    {
    x <<- y
    inverse <<- NULL
  }
     get <- function() x
     setInv <- function(inverse)         inv <<- inverse
     getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv= getInv)
}

CacheSolve <- function(x,...) {
  ## return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) 
    {
    message("getting cached data")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m, ...)
  x$setInverse(inv)
  inv
}
