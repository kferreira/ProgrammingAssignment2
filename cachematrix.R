## cachematrix.R provides the inverse of a square matrix, caching it first
## It assumes the input is a square matrix

## This function creates caches the matrix in a list of functions,
## so that the result can be easily retrieved using the list, and 
## it needs to use the proper external variable. For example, if you
## create a matrix k <- matrix(c(1,2,3,4),2,2), and then call
## j<-makeCacheMatrix(k), j will contain the list of the set, get,
##setcachematrix, and getcachematrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setcachematrix <- function(solve) m <<- solve
        getcachematrix <- function() m
        list(set = set, get = get,
             setcachematrix = setcachematrix,
             getcachematrix = getcachematrix)

}


## This function calculates the inverse of the matrix by using the solve
## function, and the cached functions from the list created by the
## makeCacheMatrix function

cacheSolve <- function(x, ...) {
        m <- x$getcachematrix()
        if(!is.null(m)) {
                message("Getting cached matrixdata")
                return(m)
        }
        matrixdata <- x$get()
        m <- solve(matrixdata, ...)
        x$setcachematrix(m)
        
        ## Return a matrix that is the inverse of 'x'
        m
}
