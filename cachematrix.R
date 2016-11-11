## This is the peer graded assignment for W3 in Course 2
## The following functions perform caching for matrix inverse
## 

## This function creates the matrix Object
makeCacheMatrix <- function(x = matrix()) {

    #verify that the matrix is square
    if(nrow(x) != ncol(x)) {
        stop("Matrix is not square")
    }
    
    matInverse <-NULL

    set <- function(y) {
        #verify that the matrix is square
        if(nrow(y) != ncol(y)) {
            stop("Matrix is not square")
        }
        
        x <<- y
        matInverse <<- NULL
    }

    get <- function() x
    setInv <- function(inv) matInverse <<- inv
    getInv <- function() matInverse

    #Now return the list of operation    
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## This function calculates and caches the inverse if needed
## Or returns the cached value

cacheSolve <- function(x, ...) {
    
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached Inverse")
        return(inv)
    }
    
    message("Calculating the Inverse")
    mat <- x$get()
    inv <- solve(mat)
    x$setInv(inv)
    inv
}
