## This R file includes two functions for creating special matrices and cache their inverses for avoiding
## repetitive inverse calculations


## This fuction creates a "wrapper" matrix that can cache it's own inverse.
##
## Params:
## x - a matrix
##
## Returns:
## a special matrix object

makeCacheMatrix <- function(x = matrix()) {
    ## initialize inverse cache
    inv <- NULL

    ## function to set matrix object
    set <- function(y) {
        x <<- y
        inv <<- NULL # invalidate cache
    }

    ## function to get underlying matrix object
    get <- function() x

    ## function to set inverse cache
    setinverse <- function(y) inv <<- y

    ## function to get cached inverse matrix
    getinverse <- function() inv

    ## return the special wrapper object
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of special wrapper matrix created by above
## makeCacheMatrix function. If inverse exists, this function will return the invese.
## Otherwise, it will compute the inverse and cache it's results
##
## Params:
## x - special wrapper matrix creatd by makeCacheMatrix function
##
## Returns:
## inverse matrix of given matrix

cacheSolve <- function(x, ...) {
    ## get cached inverse matrix from wrapper object
    m <- x$getinverse()

    ## if inverse exists, return it and exit function
    if (!is.null(m)) {
        message("returning cached inverse matrix")
        return(m)
    }

    ## if control reaches here, cache was not found
    data <- x$get() # get underlying matrix
    inv <- solve(data) # solve inverse
    x$setinverse(inv) # cache
    inv # return inverse
}
