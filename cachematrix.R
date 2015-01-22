## The functions in this file are part of the 2nd programming assignment of the
## Coursera R Programming class. 
## 
## Expensive operations can be cached in order to improve the overall program 
## performance. The two function in this file will make use of R's scoping rules 
## and implement a new cache able matrix data structure. The `makeCacheMatrix()` 
## function is can be used to create a cache able matrix. On instances of this 
## new matrix type the second function `cacheSolve()` can be called. The first 
## called of this function on a not cached matrix will try to invert the 
## underlying matrix, store the result and return it. Any following call to 
## `cacheSolve()` on the same cached matrix will not recalculate the inverted 
## matrix, but return the cached result. 

## Creates a cache able matrix from the given argument `x`. It is assumed that 
## the given matrix is a square invertible matrix. 
## 
## Use the `cacheSolve()` function on the returned matrix in order to invert the
## given matrix. 
makeCacheMatrix <- function(x = matrix()) {
    ## The cached inverted matrix. Initially set to `NULL`
    inverted <- NULL 

    set <- function(y) {
        x <<- y 
        inverted <<- NULL
    }
    get <- function() x 

    setInverted <- function(i) inverted <<- i
    getInverted <- function() inverted

    list(set = set, get = get, 
         setInverted = setInverted, getInverted = getInverted)
}

## Use this function to calculate the inverted matrix of a matrix returned by
## calling `makeCacheMatrix()`. The calculation will only be done on the first 
## call on the given cached matrix. All following calls will return the same 
## stored result. 
##
## The inverted matrix will be calculated using the R function `solve()`. All 
## Parameter of that function can be parsed as function arguments to this 
## function. 
## *NOTICE*: If `cacheSolve()` is called with different optional 
## function arguments, the result will not change after the first call. 
##
## If the cached matrix isn't invertible, an error will be thrown. 
cacheSolve <- function(x, ...) {
    inverted <- x$getInverted()
    if (!is.null(inverted)) {
        return(inverted)
    }
    inverted <- solve(x$get(), ...)
    x$setInverted(inverted)
    inverted
}
