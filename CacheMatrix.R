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

SOLUTION
> Makecachematrix <- function    (x = matrix()) {
+   inverse <- NULL
+   set <- function(y)
+     {
+     x <<- y
+     inverse <<- NULL
+   }
+      get <- function() x
+      setInv <- function(inverse)         inv <<- inverse
+      getInv <- function() inv
+   list(set = set, get = get, setInv = setInv, getInv= getInv)
+ }
> 
> CacheSolve <- function(x,...) {
+   ## return a matrix that is the inverse of 'x'
+   inv <- x$getInverse()
+   if (!is.null(inv)) 
+     {
+     message("getting cached data")
+     return(inv)
+   }
+   m <- x$get()
+   inv <- solve(m, ...)
+   x$setInverse(inv)
+   inv
+ }
> source(cachematrix.r)
Error in source(cachematrix.r) : object 'cachematrix.r' not found
> source("cachematrix.r")
> Nmatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
> Nmatrix$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> Nmatrix <- makeCacheMatrix(matrix(1:6, 2, 2))
> Nmatrix$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> Nmatrix$getInverse()
NULL
> cacheSolve(Nmatrix)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> CacheSolve(Nmatrix)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> Nmatrix <- makeCacheMatrix(matrix(1:8, 4, 4))
> Nmatrix$get()
     [,1] [,2] [,3] [,4]
[1,]    1    5    1    5
[2,]    2    6    2    6
[3,]    3    7    3    7
[4,]    4    8    4    8
> Nmatrix$getInverse()
NULL
> CacheSolve(Nmatrix)
Error in solve.default(m, ...) : 
  Lapack routine dgesv: system is exactly singular: U[3,3] = 0
> Nmatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
>  Nmatrix$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> 
> Nmatrix$getInverse()
NULL
> Nmatrix$getInverse()
NULL
> CacheSolve(Nmatrix)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
