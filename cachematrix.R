makeCacheMatrix <- function(x = matrix()) {
        ## being x a square invertible matrix
                ## It's returning a list containing functions to
                ##              1. set and get the matrix
                ##              2. set and get the inverse
                ## The list will be used as the input to cacheSolve()
                inv = NULL
        set = function(y) {
                        x <<- y
        inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
        }

cacheSolve <- function(x, ...) {
                         ## being now x the output of makeCacheMatrix()
                        ## It's returning the inverse of the original matrix input to makeCacheMatrix()  
                       inv = x$getinv()
                    # in case the inverse was previously calculated
                      if (!is.null(inv)){
                                        message("getting cached data")
                               return(inv)
                               }
                mat.data = x$get()
                inv = solve(mat.data, ...)
                        x$setinv(inv)
                     return(inv)
}
