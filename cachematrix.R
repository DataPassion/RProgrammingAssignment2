########################################################################
## Name:-Nikhil Bangad                                                 #
## Subject :- R Programming (Coursera)                                 #
## Assignment:- Assignment 2 : Caching the Inverse of a Matrix         #
## Date:- 01/21/2015                                                   #
########################################################################

#################################################################
## This function generates a matrix that can cache its inverse. #
#################################################################

makeCacheMatrix <- function(x = matrix()) {
  
  #Variable m is declared uniquely and is allocated separate addresses in memory
  m<-NULL
  
  set<-function(y)
  {
    x<<-y   ## <<- is a 'super-assignment' operator
    m<<-NULL
  }
  
  get<-function() x
  
  setmatrix<-function(solve) m<<- solve
  
  getinverse<-function() m
  
  list(set=set, get=get,
       setmatrix=setmatrix,
       getinverse=getinverse)
}

###############################################################################################################
## Notes for better understanding                                                                             #
## In function makeCacheMatrix() you’ll notice variable m is declared immediately and assigned the value NULL #  
## using the standard assignment operator (<-).                                                               # 
## However, the “set” functions defined within the containing makeCacheMatrix() function require the special  #
## assignment operator (<–) to update the value of variable m; it is important to remember variable m was     #
## declared and initialised by makeCacheMatrix().                                                             #
## Had functions set() and setmatrix() not used the special assignment operator, these functions would        #
## have allocated memory to store the value and labelled the address as m.                                    #  
## The variables named m would effectively be isolated and distinct variables.                                #
###############################################################################################################


##################################################################################################
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. #
## If the inverse has already been calculated (and the matrix has not changed), then the         #
## cachesolve should retrieve the inverse from the cache.                                        #
##################################################################################################

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  #Variable m is declared uniquely and is allocated separate addresses in memory
  m<-x$getinverse()
  
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  
  matrix<-x$get()
  
  m<-solve(matrix, ...)
  
  x$setmatrix(m)
  
  m  # Return the matrix
}
