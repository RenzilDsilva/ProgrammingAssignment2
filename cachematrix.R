## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse. 
## The output of this function is a special list of functions 
## The functions perform the following
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of matrix in a global variable
## 4.  get the value of the inverse of matrix from global variable

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set_mat <- function(y) {
    ## initialize the matrix to set global variable x
    x <<- y
    ## Here we initialize the global variable of inverse matrix to null
    inv_mat <<- NULL 
  }
  get_mat <- function() x
  set_inv <- function(invermat) inv_mat <<- invermat
  get_inv <- function() inv_mat
  list(set_mat = set_mat, get_mat = get_mat,
       set_inv = set_inv,
       get_inv = get_inv)
}


## The following function calculates the inverse of the special "vector"
##created with the above function "makeCacheMatrix". 
##However, it first checks to see if the
##inverse has already been calculated. If so, it gets using `get_inv`s 
##the inverse from the
##cache and skips the computation. Otherwise, it calculates the inverse of
##the matrix and sets the value of the inverse in the cache via the `set_inv`
##function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$get_inv()
  if(!is.null(inv_mat)) {
    message("getting cached data/matrix")
    return(inv_mat)
  }
  data <- x$get_mat()
  inv_mat <- solve(data)
  x$set_inv(inv_mat)
  inv_mat
}


## ------ Testing cache Matrix ------- ###
## Creating 3 matrix
## > a <- matrix(1:4, 2,2)
## > b<- matrix(1:4, 2,2)
## > c <- matrix(5:8, 2,2)

##

##> q2 <- makeCacheMatrix(b)
##> cacheSolve(q2)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(q2)
##getting cached data/matrix
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

