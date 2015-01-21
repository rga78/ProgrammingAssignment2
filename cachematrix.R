#' Functions for computing and caching inverse matrices.
#' robertgalderman@gmail.com
#' Jan 2015
#'


#'
#' Constructs a "special" matrix based on the given matrix.  The "special" 
#' matrix provides operations for computing and caching the inverse matrix.
#'
#' @param x a matrix
#' 
#' @return a "special" matrix -- i.e. list of functions -- that can be used to 
#'         compute and cache the inverse of the given matrix:
#'
#' Note: this function makes use of the following global variables:
#'      thematrix
#'      theinverse
#'
makeCacheMatrix <- function(x = matrix()) {
    set <- function(y) {
        thematrix <<- y
        theinverse <<- NULL
    }
    set(x)

    get <- function() {
        thematrix
    }

    setInverse <- function(xi) {
        theinverse <<- xi
    }

    getInverse <- function() {
        theinverse
    }

    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#'
#' @param x a "special" matrix previously returned by makeCacheMatrix
#'
#' @return the inverse of x.  The inverse is retrieved from the "special" matrix's
#'         cache if available.  Otherwise it is computed and cached.
#'
#' Note: this function makes use of the following global variables:
#'      thematrix
#'      theinverse
#'
cacheSolve <- function(x, ...) {
    xi <- x$getInverse()
    if (is.null(xi)) {
        # print("computing inverse...")
        xi <- solve(x$get(), ...)
        x$setInverse( xi )
    }
    xi
}


#'
#' Note: These "_noglobals" versions do not require global state.  
#'       Instead the cached values are stored in the special "value-cache"
#'       list returned by the function.
#' 
#' Wrap the given matrix in a special "value-cache" list. The given matrix is stored
#' in the field named "x".  The remaining fields of the list are
#' used for caching the results of various operations on the matrix,
#' e.g. its inverse. 
#' 
#' @param x a matrix
#'
#' @return a list with the following fields:
#'              list$x - the given matrix
#'              list$inverse - the inverse (initially NULL)
#'
makeCacheMatrix_noglobals <- function(x = matrix()) {
    list(x = x, inverse = NULL)
}


#'
#' This method will compute the inverse of the matrix defined
#' at x$x and cache the result in x$inverse.  If x$inverse has
#' already been cached, then this method is a NO-OP.
#'
#' @param x a list previously returned by makeCacheMatrix.  
#'
#' @return x, with x$inverse computed and cached if it wasn't already
#'
cacheSolve_noglobals <- function(x, ...) {
    if ( is.null(x$inverse) ) {
        # print("computing inverse...")
        x$inverse <- solve(x$x)     # cache the inverse matrix
    } 
    x
}
