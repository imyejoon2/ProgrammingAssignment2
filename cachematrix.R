#The first function creates an R object that stores a vector and its inverse matrix. The second function requires 
#an argument that is returned by makeCacheMatrix() in order to retrieve the Inverse from the cached value that is 
#in the makeCacheMatrix() object's environment.

#The makeCacheMatrix() function contains four functions: set(),get(),setmean(),getmean()
#There is also 2 data objects: x and m. By assigning a name to the function, 
#(e.g Vector<- makeCacheMatrix()),memories consumed by makeCachematric() function will 
#be realised easily and the entire fmakeCacheMatrix() environment stays in the memory.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function(){
    x
  }
  setInv <- function(Inv){
    m <<- Inv
  }
  getInv<- function(){
    m
  }
  list(set=set,get=get,setInv=setInv,getInv=getInv)
}

#makeCacheMatrix() is incomplete without the cacheSolve() function.
#cacheSolve is required to retrieve the inverse matrix
#from makeCacheMatrix() function.
#cacheSolve has the ability to calculate the inverse of the matrix
#but the value will be stored in the makeCacheMatrix() function.

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInv(m)
  m       ## Return a matrix that is the inverse of 'x'
}
