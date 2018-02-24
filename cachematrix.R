## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This a function to create a makeCacheMatrix type data.
# This type data can keep cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function
# This cacheSolve function recibe a makeCacheMatrix type data and set its inverse
# If the inverse of makeCacheMatrix is already set, cacheSolve returns the previously
# computed inverse, else, it computes the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInv(inv)
        inv
}
