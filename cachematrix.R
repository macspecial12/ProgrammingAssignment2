## Put comments here that give an overall description of what your functions do
## makeCacheMatrix - creates a special "matrix" object that can cache its inverse matrix. It contains accessor methods for
## fetching (GET) and saving (SET) the inverted matrix

## cacheSolve - fetches a copy of the saved inverted matrix if it exists. If it does nt exist, then it saves a copy of the 
## inverted matrix that was passed in. If a cached inverted matrix exists then it compares it to the matrix sent it via 
## matrix multiplication to see if they are the same. If they are the same, then the cached version is returned. If they 
## are different then a new inverted matrix is generated, saved to cache and then returned to the calling function.

## Write a short comment describing this function
## This function 

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
    inv <- x$getinv() #get inverted cached matrix
        
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
