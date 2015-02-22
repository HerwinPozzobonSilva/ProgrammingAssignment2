##--------------------------------------------------------------------------------------------
## The first function makeCacheMatrix creates the special vector containing the functions to
## set/get the matrix and set/get the inverse of the matrix :
#     - to set the matrix use setMatrix
#     - to get the matrix use getMatrix
#     - to set the inverse of the matrix use setInverse
#     - to get the inverse of the matrix use getInverse
#
## Important Notes:
#     - pay close attention to the capital letters in the names when typing
#     - the matrix must be inversible
#  Version: v1.3
#---------------------------------------------------------------------------------------------
#
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   setMatrix <- function(y) { # the function to set
      x <<- y
      m <<- NULL
   }
   getMatrix <- function() x # the function to get
   setInverse <- function(inverse) m <<- inverse # the function to set the inverse
   getInverse <- function() m                    # the function to get the inverse
   list(setMatrix = setMatrix, getMatrix = getMatrix,
        setInverse = setInverse,
        getInverse = getInverse)                # the list of functions
}
##--------------------------------------------------------------------------------------------
## The second function cacheSolve returns the inverse of the matrix x
## Important Notes:
#     - pay close attention to the capital letters in the names when typing
#     - the matrix must be inversible
#---------------------------------------------------------------------------------------------
cacheSolve  <- function(x, ...) {
   m <- x$getInverse() # getting the pre-calculated inverse matrix from the cache
   if(!is.null(m)) {   # if m is not null than we have a pre-calculated inverse matrix in cache
      message("getting cached inverse matrix")
      return(m)
   }
   
   # there is no pre-calculated inverse matrix, so we calculate now
   m <- solve(x$getMatrix()) # we get the original matrix and solve it in m
   x$setInverse(m)         # we set the inverse matrix contained in m to x 
   message("calculating the inverse matrix for the first time")
   m
}

##--------------------------------------------------------------------------------------------
## TESTING PROCEDURES COMMENTED
## To test both functions follow the instructions bellow:
#
#  1- clear ALL your environment by typing:
#     > rm(list=ls())
#
#  2- run ALL the scripts in cachematrix.R in your environment by typing:
#     > source("cachematrix.R")
#
#  3- Create an instance of the makeCacheMatrix list function by typing:
#     >n <- makeCacheMatrix()
# 
#  4- Set the original INVERSIBLE matrix (attention: the matrix must be inversible)
#     >n$setMatrix(matrix(3,3, data=c(10,40,1,50,6,7.5,4,90,25)))
#
#  5- Call the function cacheSolve to get the inverse matrix (call twice to see the messages):
#     >cacheSolve(n) # the first calculation of the inverse matrix is taken
#     >cacheSolve(n) # the cached inverse matrix is taken
#

