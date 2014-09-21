## The pair of functions that can cache the inverse of a matrix.

## This function creates a special "matrix" object and cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){ ## set and get value of matrix  
    x<<-y
    m<<-NULL
  }
  get<-function() x ## set and get value of inverse
  setmatrix<-function(solve) m<<- solve 
  getmatrix<-function() m
  list(set=set, get=get, ## Return matrix
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## This function computes the inverse of the matrix.
cacheSolve <- function(x=matrix(), ...) {
cacheSolve <- function(x, ...) {
  m<-x$getmatrix() 
## Return a matrix that is the inverse of 'x'
## If inverse is already calculated, return the directly.
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
## Calculate and cache inverse if it is not calculated yet.
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
