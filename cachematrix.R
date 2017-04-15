## makeCacheMatrix & cacheSolve
## Creasted by Chris Purington on 4/14/2017
##
## The following functions makeCacheMatrix and cacheSolve
## were created for the Coursera R Programming Week 3 
## homework assignment. Together they create a special
## matrix object which can cache its inverse (if it
## is a square invertible matrix), and which can be used
## to either calculate a matrix inverse or, if already
## calculated, retrieve the inverse from the cache.
##
##
## makeCacheMatrix creates a special matrix (really
## a list) which allows its inverse value to be cached
## once calculated. This new matrix can then be sent 
## through the cacheSolve function to obtain the 
## original matrix's inverse, if it is a square
## invertible matrix. 
##
##       Recommended use:
##       m <- makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {

minv = NULL
set <- function(y) {
        x <<- y
        minv <<- NULL
        }
get <- function() x
setinv <- function(inv) minv <<- inv
getinv <- function() minv
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
}


## cacheSolve will check for the matrix object
## created by makeCacheMatrix and attempt to
## solve for its inverse. If it is a square
## invertible matrix, cacheSolve will calculate
## and display its inverse, and store the value
## to the cache. If the inverted matrix has already
## been caclculated, cacheSolve will retrieve the 
## value from the cache instead.
## 
##
##              Recommended use: 
##              minv <- cacheSolve(m)
## where m is a matrix object created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        minv <- x$getinv()
        if(!is.null(minv)) {
                message("retrieving cached inverse matrix")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setinv(minv)
        minv
}
        ## Return a matrix that is the inverse of 'x'
