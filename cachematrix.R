## Functions which implement calculating inverse matrix with caching results

## creating special vector for specified input matrix,
## then this variable passed to cacheSolve for calculating inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL
        set <- function(y) {
                x <<- y
                invx <<- NULL
        }
        get <- function() x
        setinv <- function(pInvMatrix) invx <<- pInvMatrix
        getinv <- function() invx
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## implementation inverse matrix with caching

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, diag(dim(data)[1]), ...)
        x$setinv(m)
        m
}
