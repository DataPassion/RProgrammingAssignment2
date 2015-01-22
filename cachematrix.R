## Name:- 						        Nikhil Bangad
## Subject :- 					      R Programming (Coursera)
## Assignment:- 				      Assignment 2 : Caching the Inverse of a Matrix
## Assignment Description:- 	Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. 
##                          	One of the technique is demonstrated below
## Date:-						          01/21/2015



## This function generates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getinverse<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
