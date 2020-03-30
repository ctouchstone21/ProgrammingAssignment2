#############################################
#  Author:  C. Touchstone
#  Date: 03/29/2020
#
#  Basename:  week_3_peer_assignment
#############################################

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

# test

mat1 <- matrix(1:4, 2, 2)

matR <- makeCacheMatrix(mat1)

cacheSolve(matR)

# answer:
#       [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
