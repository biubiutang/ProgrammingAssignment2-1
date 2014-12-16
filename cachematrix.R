## cache the inverse of a matrix

## the first section makeCacheMatrix creates a special Matrix, but really is a list containing
## following function)(set the value of matrix....)
makeCacheMatrix<-function(x){
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  setInverse<-function(inverse) i<<-inverse
  getInverse<-function()i
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}
##The following function calculates the mean of the special "matrix" created with the above
## function. However, it first checks to see if the inversehas already been calculated.
## If so, it gets the inverse from the cache and skips the computation.Otherwise, it calculates
## the inverse of the data and sets the value of the inverse in the cache via the setInverse function.
cacheSolve<-function(x,...){
  i<-x$getInverse()
  if(!is.null(i)){
    message("getting inversed matrix")
    return(i)
  }
  else data<-x$get()
  i<-solve(data,...)
  x$setInverse(i)
  ## Return a matrix that is the inverse of 'x'
  i
}