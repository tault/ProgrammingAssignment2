## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## takes input of a matrix x, default is a 0x0 "empty" matrix
        sv <- NULL
        ## initializes the object that will eventually host the inverse matrix
        set <- function(y) {
                x <<- y
                sv <<- NULL
        }
        ## Get x and sv configured outside the immediate environment
        get <- function() x
        setsolve <- function(solve) sv <<- solve
        getsolve <- function() sv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve computes the inverse of the matrix-like list returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        sv <- x$getsolve()
        ## Reach back to embedded function in makeCacheMatrix
        if(!is.null(sv)) {
                message("getting cached data")
                return(sv)
        }
        ## if sv is already populated, retrieve cached value for sv
        data <- x$get()
        sv <- solve(data, ...)
        ## if sv is NOT already populated, calculate it now
        x$setsolve(sv)
        sv
}
