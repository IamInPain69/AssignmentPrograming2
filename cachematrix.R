## The makeCacheMatrix function creates a special matrix that can cache its inverse,
## whilst the cacheSolve function will compute the inverse of the special matrix returned by the function makeCacheMatrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMatrix <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        matrix <- x$get()
                tryCatch( {
                        m <- solve(matrix, ...)
                        },
                         error = function(e) {
                                 message("Error:")
                                 message(e)
                                 
                                 return(NA)
                                 },
                         warning = function(e) {
                                 message("Warning:")
                                 message(e)
                                 
                                 return(NA)
                                 },
                         finally = {
                                 x$setmatrix(m)
                                 } )
                return(m)
}
