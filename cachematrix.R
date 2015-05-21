## This function is meant to create a local cache for storing matrix and its inverse values
## to save computation time.

## makeCacheMatrix accepts a matrix as input and, depending on which function is called,
## getter and setter methods either sets the value of the matrix/inversed-matrix or returns the value

makeCacheMatrix <- function(x = matrix()) 
{
    matrixInverse <- NULL
    getMatrix <- function()
    {
        x
    }
    
    setMatrix <- function(input)
    {
        x <<- input
    }
    
    getInverse <- function()
    {
        matrixInverse
    }
    
    setInverse <- function(input)
    {
        matrixInverse <<- input
    }
    
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
    
}


## cacheSolve checks and see if there is a cached version stored in getInverse() function
## if there is none, the inverse is then computed using solve() function and then stored in cache
## using the setInverse() function

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    solution <- x$getInverse()
    if(!is.null(solution))
    {
        message("Getting cached inversed matrix...")
        return(solution)
    }
    
    data <- x$getMatrix()
    solution <- solve(data)
    x$setInverse(solution)
    solution
}
