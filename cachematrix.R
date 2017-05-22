## There are two functions in this script. One function is a wrapper around 
## matrix while other function computes inverse of the matrix if its not already
## computed. As long as 'internal' matrix does not change, inverse is returned
## from the cache.
## 
## makeCacheMatrix - This function is just a wrapper funtion around matrix. It 
##   manages matrix and its inverse.
##   This function has 4 functions
##   a. set - This function sets the value of 'internal' matrix
##   b. get - This function get the value of 'internal' matrix
##   c. setinv - This function set the value of inverse matrix.
##   d. getinv - This function gets the value of inverse matrix.
##   This function retuns the above functions as a list.

makeCacheMatrix <- function(x = matrix()) {
    ## ix is 'interna' inverse matrix. Setting it to NULL will ensure its 
    ## value is computed for the first time.
    ix <- NULL
    
    ## Set the value of matrix to 'internal' matrix. We will assume anytime new
    ## new value is assigned, it is new matrix, so we will set ix to NULL to 
    ## force compute inverse.
    ## Better way would be to actually check if new matrix is identical to old
    ## matric and set ix to NULL only if there is truely a change.
    set <- function(y) {
        x <<- y
        ix <<- NULL
    }
    
    ## This will retuen the existing 'internal' matrix
    get <- function() {
        x
    }
    
    ## Set the value to inverse matrix variable.
    setinv <- function(invx){
        ix <<- invx   
    }
    
    ## Get the inverse matrix
    getinv <- function() {
        ix
    }
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function will takes special matrix, created by makeCacheMatrix, as its
## argument and returns inverse of the matrix.
## If inverse is already computed, it returns the value from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    tempx <- x$getinv()
    
    ## Check if inverse matrix is NULL. If its not NULL, then return its value
    ## from cache
    if (!is.null(tempx)){
        cat("getting cached data\n")
        return(tempx)
    }
    
    ## inverse matrix is not available, so it needs to be computed
    ## get the matrix
    ## assume it is sqaure and invertible matrix. Use solve to get the inverse 
    tempx <- solve(x$get(),...)
    
    ## Set the value of inverse matrix
    x$setinv(tempx)
    tempx
}