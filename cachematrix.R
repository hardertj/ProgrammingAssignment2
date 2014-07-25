## Method creates a special "matrix" function. It is actually a list that contains
## 4 functions; a set, get and a setmatrixinverse and getmatrixinverse. 
## These functions then passed to the cacheSolve method below, which uses the methods
## as appropriate. 

# set - the set method sets a variable x, the value of y
# get - the get method returns the value of x. 
# setmatrixinverse - sets the value of m to the calculated value of m. 
# get matrixinverse - returns the value of m. 

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrixinverse<-function(solve) m<<- solve
  getmatrixinverse<-function() m
  list(set=set, get=get,
       setmatrixinverse=setmatrixinverse,
       getmatrixinverse=getmatrixinverse)
}


## This function is passed a special "matrix" created by the makeCachematrix method above. 
## The cacheSolve function then calls the appropriate method as required from the special matrix. 
## First the method attempts to obtain the inverse using the getmatrixinverse function. Then it checks
## to see if the returned matrix is null. If it is not null the inverse has been previously calculated and
## is stored in the matrix.m, and this matrix is returned rather the recalulating the inverse. It not the inverse
## is calculated and returned. 

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrixinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrixinverse(m)
  m  
}
 
# Here is a sample run from the RStudio prompt. 
# > source('C:/Users/Thomas/rprog/ProgrammingAssignment2/cachematrix.R')
# > source('C:/Users/Thomas/rprog/ProgrammingAssignment2/cachematrix.R')
# > mat8 <- matrix(1:4, 2) 
# > cacheMatrix = makeCacheMatrix(mat8)
# > inmat8 <- cacheSolve(cacheMatrix)
# > inmat8 <- cacheSolve(cacheMatrix)
# getting cached data
# > inmat8
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
>
