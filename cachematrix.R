##	This R script has two function that are used to set the matrix and compute the inverse of a matrix and store it in cache.
## The next time if the matrix has not changed, the inverse is returned from the cache instead of computing it again and thus saving time.

##	1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##	cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##	If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## This is the first function makeCacheMatrix which sets and returns the matrix and the inverse of the matrix storing and retrieving from cache the second time

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL							## set the inverse to NULL for init
        set <- function(y) {
                x <<- y
                i <<- NULL			## reset the inverse to NULL if the matrix is changed
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse		## set the inverse in other environment
        getinverse <- function() i						## returns the inverse
        list(set = set, get = get,						## a list of functions returned to be used as objects
             setinverse = setinverse,	
             getinverse = getinverse)


}


## This is the second function that calculates the inverse of the matrix the first time and sets it in the other environment, If the matrix is not changed, then the inverse if returned from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()           #query the x matrix's cache         
 if(!is.null(i)) {           #if there is a cache
    message("getting cached data") 
    return(i)                #just return the cache, no computation needed
  }
  data <- x$get()             #if there's no cache
  i <- solve(data, ...)        #we actually compute the inverse here
  x$setinverse(i)                #save the result back to x's cache
  i                           #return the result

}
