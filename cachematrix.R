##These are the functions for assignment2
## 1.makeCacheMatrix calculates inverse of a matrix and cache it.
## 2.cacheSolve shows the inverse if it is already calculated, otherwise 
##   it calculates the inverse.

## makeCacheMatrix makes matrix to cache the inverese of a matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(){
                x <<- y
                inv <<- NULL
        }
        get <- function () x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        
        list(set=set,get=get,
             setinv=setinv,getinv=getinv)
}

##cacheSolve shows the calculated inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cache data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}


