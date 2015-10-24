
# The first function, makeCacheMatrix creates a special "matrix", 
# which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
   
    inverse <- NULL
       
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
   
    
    get <- function() x
    
       set_inverse <- function(inv_input) inverse <<- inv_input
        get_inverse <- function() inverse
    
       list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
    
}

## The following function calculates the inverse of the matrix above 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the 
## matrix and sets the value of the inverse in the cache via 
## the setinv function.

cacheSolve <- function(x, ...) {
       inverse <- x$get_inverse()
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
        data <- x$get()
        inverse <- solve(data, ...)
        x$set_inverse(inverse)
        inverse
}