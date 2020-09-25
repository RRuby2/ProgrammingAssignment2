## A set of functions that create a matrix and then create, store
## and return the inverse of the matrix. 

## Creates a matrix and a list of functions that are used to set and 
## get the values of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv_m) inv <<- inv_m
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  
}


## Returns the inverse of the matrix created with makeCacheMatrix()
## First, checks to see if there is a value stored for the inverse matrix 'inv'
## if yes, a message is displayed and the cached inverse matrix is returned
## otherwise, the function creates the inverse matrix and returns it

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached inverse matrix")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
        
}
