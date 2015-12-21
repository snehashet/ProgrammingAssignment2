
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

       k<<- x
       inverse<- matrix()
       inverse<<-NULL
   
       set <- function(y=matrix) {
              k <<- y
              
        }
       
       setinverse<- function(){inverse<<- solve(k)}
       get<- function()k
       getinverse<- function()inverse
       list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
cacheSolve <- function(y, ...) {
        ## Return a matrix that is the inverse of 'x'
#       t<-matrix()
#       t<-y
       inv<-y$getinverse()
       if(!is.na(inv)&&!is.null(inv))
       {
       message("Getting Cached data")
#       inverse<-w$getinverse()  
       return(inv)
       }
       else{
              matr<-y$get()
              y$set(matr)
              y$setinverse()
              inv<-y$getinverse()
       }
       return(inv)
}
