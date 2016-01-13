## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    #accessors
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    setinv <- function(m_in) inv <<- m_in
    getinv <- function() inv
    
    #return a list of named accessors
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
##Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    #get inverted cached matrix
    inv <- x$getinv()
        
    if(is.null(inv)) {
        message("...inverted matrix does not exist in cache")
        message("Storing inverted matrix to cache...")
        m <- x$get()
        inv <- solve(m,...)
        x$setinv(inv)
    } else {
        message("...inverted matrix exists in cache")
        m <- x$get()
        
        #Test to see if the stored inverted matrix matches the input matrix, if not store a new invertd matrix
        if(identical(round(inv %*% m,3),(diag(nrow = nrow(m), ncol = ncol(m))))) {
            message("...cached matrix matches input matrix")
            message("Returning cached matrix...")
            return(inv)
        } else {
            message("...cached matrix does not match input matrix")
            message("Storing updated inverted matrix to cache...")
            inv <- solve(m,...)
            x$setinv(inv)
        }
    }
    
    return(inv)
}
