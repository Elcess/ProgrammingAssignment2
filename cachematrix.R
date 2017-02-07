## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a (presumed invertible) matrix and defines 4 functions:
## set, get, setinv, and getinv. The functions are output as a list for use by
## cacheSolve.
##
## cacheSolve retrieves a cached inverse for a matrix that has been "inverted" by
## makeCacheMatrix, or returns a newly calculated inverse if there isn't one in
## the cache.

## Note: cacheSolve will *only* work on the "special" matrix object created by
## makeCacheMatrix. It will *not* invert a numeric matrix.

## Write a short comment describing this function

## makeCacheMatrix creates a special "matrix" which is really a list containing a
## function to:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) { 
        inv <- NULL                              # initialize values
        set <- function(y) {                     # set the value of the matrix
                x <<- y                          #   to itself
                inv <<- NULL                     # reset "inv" to NULL whenever
        }                                        #  "set" is called
        get <- function() x                      # get the data
        setinv <- function(solve) inv <<- solve  # calculate the inverse and update "inv"
        getinv <- function() inv                 # get the cached value of the inverse
        list(set = set, get = get,               # define the output list
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

## cacheSolve takes a 4-member list of the structure output by makeCacheMatrix
## and calculates the inverse of the matrix, unless it has already been calculated
## and stored in cache, in which case it will return the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()        # see if there is a previous value for the inverse
        if(!is.null(inv)) {      # if there is, get the cached version
                message("getting cached data")
                return(inv)
        }
        data <- x$get()          # if there isn't a cached version, get the matrix
        inv <- solve(data, ...)  # and invert it
        x$setinv(inv)            # set the cache to this value
        inv                      # and return it
}
