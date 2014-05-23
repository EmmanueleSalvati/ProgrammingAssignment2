## In analogy to the example, the function makeCacheMatrix
## returns a list of functions: set, get, set_inv, get_inv.
## Its role is simply to cache a matrix in a different scope.

makeCacheMatrix <- function(x = matrix()) {
    # 'inv_x' is the inverse of the matrix x
    inv_x <- NULL
    
    # Stores the matrix into the cache
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    # Retrieves the matrix
    get <- function() x
    
    # Stores the inverse of the matrix into the cache
    set_inv <- function(inverse) inv_x <<- inverse
    
    # Gets the inverse of the matrix from the cache
    get_inv <- function() inv_x
    list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## Given a matrix, this function returns its inverse, by either computing it,
## or by retrieving it from the cache (if it finds it)

cacheSolve <- function(x, ...) {
    inv_x <- x$get_inv()
    if (!is.null(inv_x)) {
        message("getting cached data")
        return(inv_x)
    }
    data <- x$get()
    inv_x <- solve(data)
    x$set_inv(inv_x)
    inv_x
}
