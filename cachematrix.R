## We write a pair of functions that cache the inverse of a matrix.
## The functions created check if matrix inversion is stored in Cache or not. If stored, 
## it prints the value from the cache else compute inverse using solve() function.


## Example:
## > m <- makeCacheMatrix(matrix(c(5, 0, 0, 5), c(2, 2)))
## > cacheSolve(m)
## [,1] [,2]
## [1,]  0.2  0.0
## [2,]  0.0  0.2


## makeCacheMatrix: This function creates a special "matrix" object that can cache its 
##                  inverse. It itself contains four other matrices:
##                  - set:    to set the value of the matrix
##                  - get:    to get the value of the matrix
##                  - setinv: to set the value of the inverse matrix
##                  - getinv: to set the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
##             makeCacheMatrix above. If the inverse has already been calculated 
##             (and the matrix has not changed), then cacheSolve retrieves the 
##             inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if(!is.null(inv)){    ## Logical Statement to check "inv" is null or not.
    print("Getting cached inverse")
    return(inv)
  }  
  
  data <- x$get()
  inv <- solve(data,...)  ## Return a matrix that is the inverse of 'x'
  x$setinv(inv)
  inv
}
