## makeCacheMatrix and cacheSolve work together to accomplish the problem of
## creating a matrix and solving for the inverse of the stored matrix. 
## More detailed descriptions for each function are shown below.

## makeCacheMatrix takes a matrix as an input and stores it. 
## A new matrix can be added when the function is called initially by passing
## a matrix as the only argument, or an existing matrix can be overridden by 
## calling makeCacheMatrix$set() and passing a matrix in as the only arg.
## Once a matrix is "set" by either method, makeCacheMatrix stores the 
## matrix for use by the second function, cacheSolve, with another pair of
## get and set commands that are used to get and set inverses. The end-user
## does not need to use these arguments, but the cacheSolve program relies
## on them. When a new matrix is "set", the stored cached value in "getinverse"
## is set to NULL.

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

## cacheSolve takes a matrix stored in makeCacheMatrix and calculates the
## inverse of the matrix stored in makeCacheMatrix using the solve() function.
## If the matrix is unchanged, and a value in getinverse exists from 
## makeCacheMatrix, cacheSolve will simply print that value instead of 
## re-calculating the inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       
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
