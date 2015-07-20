# makeCachematrix create a "matrix" object to cache its inverse.
# makeCachematrix returns a list of functions to:
# set the value of a matrix
# get the value of the matrix
# set the value of the invert o fthe matrix
# get the value of the inverse of a matrix 
makeCacheMatrix <- function(x = matrix()) {
    #initialize the inverse
    inv <- NULL
    #set the matrix 
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    #get the matrix 
    get <- function() x
    # set the inverse 
    setinverse <- function(inverse) inv <<- inverse
    # get the inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#cacheSolve computes the inverse of the “matrix” returned by makeCacheMatrix(); 
#if has been alreasy calculated retrieves from cache;

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    #if inverse has been already calculated get it from cache 
    if(!is.null(inv)) {
        message("getting cached matrix.")
        return(inv)
    }
    #otherwise calculate it
    data <- x$get()
    inv <- solve(data)
    #set the value of the inverse in the cache  
    x$setinverse(inv)
    inv
}
