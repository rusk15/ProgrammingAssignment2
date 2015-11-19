## The task is to inverse a matrix and also store it in a cache, so that it must not be computed everytime. 

## This part of the function creates a list of functions to set the value of matrix, get it,
## then set the value of inverse of that matrix and then get the value of that inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <-function (y) {
          x <<-y
          inv <<-NULL
    }
    get <-function () x
    setinverse <-function(inverse) inv <<-inverse
    getinverse <-function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This next function gives the inverse of the matrix, and if it already exists, it just gets that result.
## So it doesn't calculate it again. As instructed, it assumes matrix is invertible.

cacheSolve <- function(x, ...) {
        inv <-x$getinverse()
        if(!is.null(inv)) {
            message("matrix from cached data")
            return(inv)
        }
        data<-x$get()
        inv <-solve(data)
        x$setinverse(inv)
        inv
}
