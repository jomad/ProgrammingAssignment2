
## The following are two functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        x
    }
    setInv <- function(solve) {
        inv <<- solve
    }
    getInv <- function() {
        inv
    }
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
}

## The cacheSolve function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setInv
    inv
}

# Sample run of functions if its working

# x <- matrix(c(2,1,4,0,3,1,4,4,2), 3,3 )
# y <- makeCacheMatrix(x)
# 
# cacheSolve(y)

# Output
#         [,1]  [,2]  [,3]
# [1,] -0.050 -0.10  0.30
# [2,] -0.350  0.30  0.10
# [3,]  0.275  0.05 -0.15

# y$get()

# Output
#       [,1] [,2] [,3]
# [1,]    2    0    4
# [2,]    1    3    4
# [3,]    4    1    2
