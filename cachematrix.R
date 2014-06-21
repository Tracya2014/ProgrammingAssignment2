## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create a new object which has following character
## It keep can store the matrix and its inverse 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) inv <<- inv
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## Following function get the inverse of above new object.  
## If the inverse is already existed. The function just return the inverse got from cache. 
## Otherwise, function calculates the inverse and returns it. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
