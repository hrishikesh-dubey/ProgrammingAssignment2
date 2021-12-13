## Put comments here that give an overall description of what your
## functions do

# There are 2 functions in this assignment i.e. "makeCacheMatrix" and "cacheSolve"

## Write a short comment describing this function
 
# "makeCacheMatrix" creates a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  setInverse<-function(inverse)i<<-inverse
  getInverse<-function()i
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function

# "cacheSolve" calculates the inverse of matrix which is returned by "makeCacheMatrix" and will retrive
# the inverse from the cache if the matrix has been previously solved
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getInverse()
  if(!is.null(i)){
    return(i)
  }
  d<-x$get()
  i<-solve(d,...)
  x$setInverse(i)
  i
}

# Testing the functions
m<-matrix(rnorm(9),3,3)
s<-makeCacheMatrix(m)
cacheSolve(s)
