## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function will create a new matrix and define functions that can be used 
## for that matrix
## The 'set' function will set the value of the matrix and initialize the master matrix
## and the inverse matrix
## The 'get' function will return the matrix
## The 'getInverse' function will return the matrix inverse
## The 'setInverse' function will set the matrixinverse to the value passed in the 
## argument


makeCacheMatrix <- function(vmx = matrix()) {
  
  ##initialize the matrix inverse
  vmxinv<-NULL
  
  ##function to set the matrix
  ## set the matrix to the value passed. Re-initialize the matrix inverse and
  ## the master matrix values.
  ## the master matrix will be used for comparing to see if the matrix passed in
  ## cacheSolve function is the same as the matrix stored in memory
  set<-function(y){
  vmx<<-y  
  vmxinv<<-NULL
  
  vmxmst<<-vmx
  
  }
  
  
  ##function to get the matrix
  ## return the matrix
  get<-function() vmx
  
  ##to set the inverse
  ## set the matrixinverse value to the value passed in the matrix
  setinverse<-function(xtinv) vmxinv<<-xtinv
  
  ##function to get the inverse
  ## return the matrix inverse value
  getinverse<-function() vmxinv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
  

}


## Write a short comment describing this function
## this function will 
## i. check if the matrix passed in argument is same as the matrix originally stored in
##  memory
## ii. If the matrix is same, it will check inverse is already available for the given matrix 
##  iii. If the matrix inverse is already available, it will return the matrix inverse without redoing
##  the calculation
##  iv. If the matrix inverse is not available, it will calculate the inverse and set 
##  the matrix inverse to the calculated value and return the matrix inverse
## 

cacheSolve <- function(vmx, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ##check if matrix's have not changed
  
  if (matequal(vmx$get(), vmxmst)) {
    
    xinv<-vmx$getinverse() ##get the matrix inverse
    
    
    if (!is.null(xinv)){
      message("getting cached data")
      return(xinv)
    }
    
    data<-vmx$get()
    xinv<-solve(data)
    vmx$setinverse(xinv)
    xinv
  }
  else {
    message("matrix has changed, first set the matrix")
  }
}

## This function will check if the matrices passed in the argument are equal
## function obtained from the website https://stat.ethz.ch/pipermail/r-help/2012-June/315408.html
matequal <- function(x, y)
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)