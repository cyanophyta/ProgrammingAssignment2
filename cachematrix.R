## makeCacheMatrix functon creates a vector of getters and setters
## for a square matrix and its inverse matrix and these functions
## saves/accessess the two matrices to/from cache

makeCacheMatrix <- function(x = matrix()) {
        invmat <- NULL
        setmat <- function(y) {
                  x <<- y
                  invmat <<- NULL
        }
        getmat <- function() {
                  x
        }
        setinv <- function(matinv) {
                  invmat <<- matinv
        }
        getinv <- function() {
                  invmat
        }
        list(setmat = setmat,
             getmat = getmat,
             setinv = setinv,
             getinv = getinv)
}


## Given a square matrix, for which makeCacheMatrix function was called
## that returned a list of getters/setters, using those functions, 
## this function caches inverse of the original matrix
## unless it finds the inverse matrix already in the cache

cacheSolve <- function(x, ...) {
        ## Return inverse of the matrix which was passed to
        ## makeCacheMatrix function.
        invmat <- x$getinv()
        if(!is.null(invmat)) {
          message("getting cached inverse of matrix")
          return(invmat)
        }
        data <- x$getmat()
        invmat <- solve(data, ...)
        x$setinv(invmat)
        invmat
}

#> source("cacheMatrix.R")
#> flist <-makeCacheMatrix(matrix(1))
#> cacheSolve(flist)
#[,1]
#[1,]    1
#> cacheSolve(flist)
#getting cached inverse of matrix
#[,1]
#[1,]    1
#> flist <-makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
#> cacheSolve(flist)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(flist)
#getting cached inverse of matrix
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
