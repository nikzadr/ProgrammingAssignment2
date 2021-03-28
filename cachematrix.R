# This function is somewhat similar to getMean. Here s = solve, a function which calculates the inverse of a matrix.
# setSolve, getSolve functions replaced setMean and getMean.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL 
    set <- function(y) { 
        x <<- y
        s <<- NULL
    }
    get <- function() x 
    setSolve <- function(inverse) s <<- inverse 
    getSolve <- function() s
    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


# first gets the inverse value (getSolve), if the value exists, it loads the cached value and prints a message
# else, if calculates the inverse (solve(mat)), and caches the result (setSolve)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getSolve()      
    if(!is.null(s)) {     
        message("getting cached inverse matrix")
        return(s)       
    }
    mat <- x$get()      
    s <- solve(mat)      
    x$setSolve(s)    
    s       
}
