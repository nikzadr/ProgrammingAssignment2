## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL #inverse matrix value is null
    set <- function(y) { #sets the matrix value to given value Y
        x <<- y
        s <<- NULL #sets the inverse value to null
    }
    get <- function() x #returns the matrix value X
    setSolve <- function(inverse) s <<- inverse #sets inverse value to the given value
    getSolve <- function() s #returns the inverse value 
    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getSolve() #get the inverse value of X
    if(!is.null(s)) { #if the value exists:
        message("getting cached inverse matrix")
        return(s) #get cached value and print the message
    }
    mat <- x$get() #if not: set tha matrix value to mat
    s <- solve(mat) #calculate the inverse of mat 
    x$setSolve(s) #set (cache) the inverse value 
    s #return the inverse
}
