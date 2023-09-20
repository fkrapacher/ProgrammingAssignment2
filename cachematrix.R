## Inverse a Matrix can be computationally requiring, so caching the inverse 
## can be more efficient in computation cost rather that compute it again and again. 

## This function creates matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <-NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
get <- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse <-function() inv 
list(set =set,
     get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}


## This function calculates the inverse of the matrix cached by function above 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("returning inversed matrix")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

