## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. 
##Below are a pair of functions that are used to create a special object that stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        mat <- NULL
        set <- function(y) {
                
                x <<- y
                inv <<- NULL #matrix changed, reassign to NULL
                
        }
        
        get <- function() x #value of matrix
        setInverse <- function(inverse) inv <<- inverse #inverse of matrix
        
        getInverse <- function() inv #get inverse of matrix
        
        
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse) #list of all functions defined above
        
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" created by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'


        
        inv <- x$getInverse()
        
        if (!is.null(inv)) {
                
                message("getting cached data")
                return(inv) #if inverse exists, check if already cached, if yes, return cached inverse
                
        }
        
        mat <- x$get()
        inv <- solve(mat, ...) #compute inverse of matrix
        
        x$setInverse(inv) #cache inverse of matrix
        
        inv
        
}

