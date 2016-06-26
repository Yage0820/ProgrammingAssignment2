## Matrix inversion is usually a costly computation and there may be some benefit to
##caching the inverse of a matrix rather than computing it repeatedly 
##(there are also alternatives to matrix inversion that we will not discuss here).
##Your assignment is to write a pair of functions that cache the inverse of a matrix.

##  This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<-NULL
        }
        get <- function() x
        setinverse <- function(inverse) inver <- inverse
        getinverse <- function()inver
        list(set = set, get = get,
             setinverse =setinverse,
             getinverse =getinverse)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getinverse()
        if(!is.null(inver)){
                return(inver)
                message("getting cached data")
                return(inver)
        }
        mat <- x$get()
        inv <- solve(mat,...)
        s$setinverse(inver)
        inv
}
