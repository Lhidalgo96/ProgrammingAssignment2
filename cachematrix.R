# Put comments here that give an overall description of what your
## functions do

## creates and sets the values of the matrix and then saves its inverse to an object

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y)
        {
                x <<- y ##To assign value from different environement
                inv <<- NULL
        }
        
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv  ##inverse the Matrix
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ##Cache the mean
}


## Checks the inverse value in the cache, and then gets the result and skips computation or sets the value into the cache
## depending on the first check, uses the setInverse to assign the value of the matrix, and then retrieve it

cacheSolve <- function(x, ...) 
{
        inv <- x$getInverse()
        if (!is.null(inv)) #If its already calculated, and the matrix hasn't change, returns the value
        {
                return(inv) 
        }
        ##If not will compute
        mat <- x$get()  #Computes the inverse of the matrix (If squared)
        inv <- solve(mat, ...) 
        x$setInverse(inv)
        inv
}
