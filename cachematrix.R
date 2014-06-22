## I am writing two functions makecacheMatrix and cacheSolve 
##which will 1) cache the inverse of a Matrix and 2) compute the inverse of Matrix. 
## If the input Matrix has not changed then the cached inverse is returned 
## otherwise for every new matrix input, a new compute is performed by using solve()
## function and value is cached/returned.

##The first function, makeCacheMatrix creates a special "Matrix", which is really caching the inverse
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL
        set <- function(y) 
        {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setInverseMatrix <- function(solve) m <<- solve
        getInverseMatrix <- function() m
        
        list(setMatrix = set, getMatrix = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}

## This Second function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix function above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.
## Else it computes the inverse of the matrix using solve() and sets the value in cache
## using the setInverseMatrix() function.

cacheSolve <- function(x, ...) 
{
        m <- x$getInverseMatrix()
        
        if(!is.null(m)) 
        {
                message("getting cached inverse matrix data")
                return(m)
        }
        
        data <- x$getMatrix()
        m <- solve(data, ...)
        x$setInverseMatrix(m)
        m
        
}

