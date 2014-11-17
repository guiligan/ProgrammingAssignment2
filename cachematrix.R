## Implementaion of Solve() function to calculate a matrix  inverse and cache
## the result in order to avoid future costs to recalculate.

## makeCacheMatrix is the matrix and cache holder for the inverse of the
## set matrix. It contains four sub-functions: set (sets the matrix), get
## (returns the current matrix), setInverse (internal function to cache the
## inverse), and getInverse (internal function to return the inverse cache).

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL;
    set <- function (y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function () x
    setInverse <- function (inv) inverse <<- inv
    getInverse <- function () inverse
    list (set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## cacheSolve is the implementaion of Solve() function to calculate a matrix 
## inverse and cache the result using makeCacheMatrix matrix holder.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if (!is.null (inverse)) {
        ## Return cached value
        return (inverse)
    }
    else {
        ## Cache not found. Calculate inverse, set cache and return result
        matrix <- x$get()
        inverse <- solve (matrix, ...)
        x$setInverse (inverse)
        
        return (inverse)
    }
}
