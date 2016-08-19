# Mike Ahmad 
# Coursera - R Programming  
# Programming Assignment 2: Lexical Scoping 
# https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping
#
# Title: Caching the Inverse of a Matrix
#  
# Background: Matrix inversion is usually a costly computation and there may be some benefit to caching 
# the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion 
# that we will not discuss here). Write a pair of functions that cache the inverse of a matrix.
#
#
#
# makeCacheMatrix creates a list containing a function to:
# The first function, makeVector creates a special "vector", which is really a list containing a function to
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the inverse of the matrix 
# 4.get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse 
# has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse 
# from the cache. If not, it computes the inverse and sets the value in the cache. 
# The function assumes the matrix is always invertible. 


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
