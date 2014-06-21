##These functions enable the user to store
##and retrive a matrix and its inverse from an alternate environment		



## This function creates a list object containing functions that
## can be called to store a matrix and its inverse in
## an alternate environment

makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function takes a "makeCacheMatrix" 
## and returns the matrix inverse from the 
##alternate environment or solves for the 
##inverse and then stores it



cacheSolve <- function(x, ...) {

        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}