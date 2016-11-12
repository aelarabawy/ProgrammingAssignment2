## This is the peer graded assignment for W3 in Course 2
## The following functions perform caching for matrix inverse
## 

## This function creates the matrix Object
makeCacheMatrix <- function(x = matrix()) {

    matSolve <-NULL

    set <- function(y) {
        x <<- y
        matSolve <<- NULL
    }

    get <- function() x
    setSolve <- function(s) matSolve <<- s
    getSolve <- function() matSolve

    #Now return the list of operation    
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## This function calculates and caches the inverse if needed
## Or returns the cached value

cacheSolve <- function(x, ...) {
    
    s <- x$getSolve()
    if(!is.null(s)) {
        message("getting cached Inverse")
        return(s)
    }
    
    message("Calculating the Inverse")
    mat <- x$get()
    s <- solve(mat)
    x$setSolve(s)
    s
}
