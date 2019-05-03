## makeVector creates an R object that stores a vector and its mean.
## cachemean() requires an argument that is returned by makeVector() in order to
## retrieve the mean from the cached value that is stored in the makeVector()
## object's environment

## builds a set of function and returns the 
## functions within a list to the parent environment

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setinverse<-function(inverse)m<<-inverse
        getinverse<-function()m
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)

}


## this function computes the inverse of the special" matrix
## returned by makeCacheMatrix above. if the inverse has already been
## calculated (and the matrix has not changed), then the cacheSolve should 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-inverse(data,...)
        x$setinverse(m)
        m
}














