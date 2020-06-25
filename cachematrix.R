## Programming Assigment 2 for caching inverse matrix
##
## 'makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.
##
makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setSolve <- function(imatrix) im <<- imatrix
        getSolve <- function() im
        list(
                set = set,
                get = get,
                setSolve = setSolve,
                getSolve = getSolve
        )
        
}

##
## This function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.
##
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        im <- x$getSolve()
        if (!is.null(im)) {
                message("getting cached Solve")
                return(im)
        }
        mdata <- x$get()
        im <- solve(mdata, ...)
        x$setSolve(im)
        im
}
