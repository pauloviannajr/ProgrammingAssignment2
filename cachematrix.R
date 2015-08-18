## These functions allow to avoid recomputing a matrix's inverse by storing 
## the first computation of the inverse of a given matrix in cache 

## This function creates a special matrix, which is really a list
## containing a function to:
## 1. set the value of the matrix;
## 2. get the value of the matrix;
## 3. set the value of the matrix's inverse;
## 4. get the value of the matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<- function() x
        setinv<-function(inversematrix) inv<<-inversematrix
        getinv<-function() inv
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function returns the inverse of the special matrix created with the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setinv(inv)
        inv
}
