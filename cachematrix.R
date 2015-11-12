## Two support functions that speed up creation of frequently 
## used data when inverting matrixes. Already calculated data
## (solve for matrix inversion) gets cached and saves calculation
## power when used next time on same data

#' Make a cached matrix that retains the inverse result
#' of the solv function when used in conjunction with
#' cacheSolve()
#' 
#' @param x A matrix. By default an empty matrix is created
#' @return the cachable matrix.
#' @examples
#' create a sample matrix
#' a <- diag(5,5)
#' cachedMatrix <- makeCacheMatrix(a)
#' 
#' create cachable empty matrix
#' cachedEmptyMatrix <- makeCacheMatrix()
makeCacheMatrix <- function(x = matrix()) {
     # s contains the cached solve item
     s <- NULL
     # set a matrix manually, clean cache
     set <- function(y) {
          x <<- y
          s <<- NULL
     }
     # return the matrix
     get <- function() x
     # set the cached solve result of the matrix, is called by @cacheSolve 
     setsolve <- function(solve) s <<- solve
     # get the cached solve result of the matrix
     getsolve <- function() s
     # return the list
     list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

#' With a given makeCacheMatrix object, returns the inverse 
#'   of that matrix
#'
#' @param x A matrix created by makeCacheMatrix and 
#' any additional parameter that function solve allows
#' @return either the new calculated inverse matrix or
#' the cached result
#' @examples
#' # create a sample matrix and cache it
#' a <- diag(5,5)
#' cachedMatrix <- makeCacheMatrix(a)
#' # call matrix inversion twice
#' # first time it gets calculated
#' cacheSolve(cachedMatrix)
#' # and second time taken from cache
#' cacheSolve(cachedMatrix)
#' 
cacheSolve <- function(x, ...) {
     # try to get a precalculated = cached previous result
     # from the cachable matrix
     s <- x$getsolve()
     # if not null we can use the cached result and return
     if(!is.null(s)) {
          message("getting cached data")
          return(s)
     }
     # In case it is null we need to calculate the result
     # 1. Take the matrix from the input parameter
     data <- x$get()
     # 2. Calculate the inverse matrix, keep all parameters
     # from solve functions and assign result into s
     s <- solve(data, ...)
     # 3. Now cache the result in cachedMatrix object for
     # later retrieval 
     x$setsolve(s)
     # return the inversed matrix
     s
}