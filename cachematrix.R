## These functions provide a means by which a matrix object can cache its inverse
## to avoid unnecessary re-calculation of matrix inversion operations

## makeCacheMatrix creates a special matrix with some 'additional' functionality

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                               ##initialize i (inverted matrix) with a null value
        set <- function(y) {                    ##the set function allows a new matrix (y) passed into this env. 
                x <<- y                         ##to overwrite the original matrix and...
                i <<- NULL                      ##reinitializes i with a null value
        }
        get <- function() x                     ##get function simply returns x to caller
        setinverse <- function(solve) i <<- solve
                                                ##setinverse function assigns the passed value (solve) to i
        getinverse <- function() i              ##getinverse function returns i to the caller
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)           ##makeCacheMatrix returns a list of internal
                                                ##functions that operate as handles outside the 
                                                ##environment of this function
}

## cacheSolve will either retrieve a cached matrix inversion OR solve/invert a matrix that has not yet been 'solved'

cacheSolve <- function(x, ...) {
        i <- x$getinverse()                     ##get the inverted matrix from the passed object and assign to i
        if(!is.null(i)) {                        ##if the inverted matrix exists...
                message("getting inverted matrix")      ##notify user the retrieval is in progress
                return(i)                       ##and return the inverted matrix
        }
        my_matrix <- x$get()                    ##if an inverted matrix does not exist, my_matrix gets original matrix 
        i <- solve(my_matrix, ...)              ##the matrix inversion occurs and is assigned to i
        x$setinverse(i)                         ##the matrix inversion is passed back to x for caching
        i                                       ## Finally, Return a matrix that is the inverse of 'x'
}