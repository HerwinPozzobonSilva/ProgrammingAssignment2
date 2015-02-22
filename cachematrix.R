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
   m <- x$getInverse() # getting the pre-calculated inverse matrix
   if(!is.null(m)) {   # if it is not null than we have a pre-calculeted inverse matrix
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
##
#  1- clear ALL your environment by typing:
#     > rm(list=ls())
#
#  2- run ALL the script cachematrix.R in your environment by typing:
#     > source("cachematrix.R")
#
#  3- Create an instance of the makeCacheMatrix list function by typing:
#     >m <- makeCacheMatrix()
# 
#  4- Set the original INVERSIBLE matrix
#     >m$setMatrix(matrix(3,3, data=c(10,40,1,50,6,7.5,4,90,25)))
#
#  5- Call the function cacheSolve to get the inverse matrix (call twice to see the messages):
#     >cacheSolve(m) # the first calculation of the inverse matrix is taken
#     >cacheSolve(m) # the cached inverse matrix is taken
#