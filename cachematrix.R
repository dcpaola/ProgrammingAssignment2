## The “makeCacheMatrix” f(x) creates a special “matrix” object that caches its inverse. 
## It is a list containing a function to:
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
   set <- function(y) {
     x <<- y
     m <<- NULL
  }
 
         get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
      list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## The "cacheSolve" f(x) computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
## If the inverse has already been calculated, and the result is chashed
## then the cacheSolve should retrieve the inverse from the cache.
## If the inverse has not been calculated, data gets the matrix stored with makeCacheMatrix,
## m calculates the inverse, and x$setmean(m) stores it in the object m in makeCacheMatrix.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
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


       
## Example
# a> b <- diag(2, 3)
## > b
##       [,1] [,2] [,3]
## [1,]    2    0    0
## [2,]    0    2    0
## [3,]    0    0    2

## Then use solve(x) to compute the invert of the matrix       
## > solve(b)
##     [,1] [,2] [,3]
##  [1,]  0.5  0.0  0.0
##  [2,]  0.0  0.5  0.0
##  [3,]  0.0  0.0  0.5
       
## Next, use  the makeCacheMatrix and CasheSolve:
## No chache in the firt sun
## > my_matrix <- makeCasheMatrix(b)
## > my_matrix&get()
##       [,1] [,2] [,3]
## [1,]    2    0    0
## [2,]    0    2    0
## [3,]    0    0    2

## No chache in the firt sun
## > casheSolve(my_matrix)
##       [,1] [,2] [,3]
##  [1,]  0.5  0.0  0.0
##  [2,]  0.0  0.5  0.0
##  [3,]  0.0  0.0  0.5

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data
       ##       [,1] [,2] [,3]
##  [1,]  0.5  0.0  0.0
##  [2,]  0.0  0.5  0.0
##  [3,]  0.0  0.0  0.5
