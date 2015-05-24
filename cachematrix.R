## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.

#####################
### By newja1390
################

## A pair of functions that cache the inverse of a matrix:

## =============================================================================
## The 'makeCacheMatrix' function creates a special "matrix" object that can 
## cache its inverse, which is really a list containing a function to :
##      set the value of the vector
##      get the value of the vector
##      set the value of the mean
##      get the value of the mean


makeCacheMatrix <- function(x = matrix())
{
     m <- NULL
     set <- function(y) 
     {
        x <<- y
        m <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) m <<- inverse
     getinverse <- function() m
     list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)       
}

## =============================================================================
## The 'cacheSolve'function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
  
    m <- x$getinverse()
    if(!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m    
}

## =============================================================================
## a sample run
# > x <- matrix(1:4, nrow = 2, ncol = 2)
# > x
#        [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > source ("cachematrix.R")
# > chachMatrix = makeCacheMatrix(x)
# > chachMatrix$get()
#       [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(chachMatrix)
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(chachMatrix)
# getting cached data
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
