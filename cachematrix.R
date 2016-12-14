## Caching the Inverse of a Matrix

## makeCacheMatrix is a function that stores a matrix and caches its inverse

makeCacheMatrix <- function(mat = matrix()) {
        inv <- NULL
        setMatrix <- function(m) {
                mat <<- m
                inv <<- NULL
        }
        getMatrix <- function() mat
        setInverse <- function(solve) inv <<- solve
        resetInverse <- function() inv <<- NULL
        getInverse <- function() inv
        list(set = setMatrix, 
             get = getMatrix,
             setinverse = setInverse,
             getinverse = getInverse,
             resetinverse = resetInverse)

}


#cacheSolve calculates the inverse of the matrix. However, it 
#first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix and sets the 
#value of the inverse in the cache via the setInverse function.


cacheSolve <- function(m, ...) {
        inv <- m$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- m$get()
        inv <- solve(mat, ...)
        m$setinverse(inv)
        inv

}
