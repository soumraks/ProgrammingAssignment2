## The set of functions below works together.
## The 'cachesolve' function checks if the inverse of the matrix is available
##    in cache, when it is not available it inverses the matrix and return
##    the result; else it displays the inverse matrix available in the cache
##    environment
##
## Comments are included inline in the functions.
## SAMPLE RUN after the function definition

## ---------------------------------------------------
## -- Function makeCacheMatrix -----------------------
## This function creates and returns 4 functions that are used by
##   by cacheSolve to get or set the inverted matrix in cache.
##   'matc' is the used to store the cached matrix
##   'set'  create the matrix in the working environment
##   'get'  gets the value of the matrix
##   'setmati'  innverts the matrix and stores in cache
##   'getmati'  gets the inverted matrix from cache

makeCacheMatrix <- function(x = matrix()) {
  # initialize cache 'matc' to NULL
  matc <- NULL
  set <- function(y) {
    x <<- y
    matc <<- NULL
  }
  get <- function() x
  setmati <- function(solve) matc <<- solve
  getmati <- function() matc
  
  # return all the functions created to the working environment
  list(set = set, get = get, setmati = setmati, getmati = getmati)
}

## ___________________________________________________


## ---------------------------------------------------
## -- Function cacheSolve ----------------------------
## The cacheSolve function calcluates the inverse of a matrix created
##   in the makeCacheMatrix function
## If the inverted matrix is not available in cache, it it created in
##   the working environment and the calculated inverted inverted matrix
##   is stored in cache.
## This function also checks for errors with matrix inversion before
##   fetching and/or calculating the inverse of the matrix.

cacheSolve <- function(x, ...) {
  # get the inverse matrix from cache
  matc <- x$getmati()
  
  # if available, return the cached inverted matrix and exit from function
  if(!is.null(matc)) {
    message("This inverted matrix is fetched from cache")
    return(matc)
  }
  
  # if not available, create matrix
  matrix <- x$get()
  
  # handle matrix inversion calculation errors and return
  tryCatch({
    matc = solve(matrix, ...)
  }, error = function(e) {message("Error: " ) 
              message(e) 
              return(NA)}
   , warning = function(e) {message("Warning: " ) 
                message(e) 
                return(NA)}
     , finally ={
        x$setmati(matc)
       }
  )
  
  return(matc)
}

## ___________________________________________________


## ---------------------------------------------------
## -- S A M P L E    R U N ---------------------------
## > source("cachematrix.R")
## > m <- makeCacheMatrix()
## >
## defining the matrix
## > mat <- matrix(rnorm(4), 2, 2)
## > mat
##            [,1]        [,2]
## [1,]  0.1972364  0.01910708
## [2,] -1.2017155 -3.91865669
## > 
## > m$set(mat)
## > 
## > cacheSolve(m)
##           [,1]        [,2]
## [1,]  5.225292  0.02547813
## [2,] -1.602415 -0.26300275
## > 
## > cacheSolve(m)
## This inverted matrix is fetched from cache
##           [,1]        [,2]
## [1,]  5.225292  0.02547813
## [2,] -1.602415 -0.26300275
## > 
## defining the matrix to test error check
## > mat <- matrix(rnorm(6), 3, 2)
## > 
## > mat
##            [,1]       [,2]
## [1,] -1.3346317  1.2257057
## [2,]  0.1621681  0.5318113
## [3,]  1.5161963 -0.2379762
## > m$set(mat)
## > cacheSolve(m)
## Error: 
## 'a' (3 x 2) must be squareNULL
## ___________________________________________________
