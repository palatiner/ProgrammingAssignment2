## There are two functions here: 
## makeCacheMatrix() creates a matrix object with four different 
## methods:
## - get: recall the original version of the matrix
## - setInverse: save the inverse of the original matrix
## - getInverse: recall the inverse of the matrix
## - set: change the original matrix which also sets the inverse 
##        to NULL since it needs to be recalculated again (this
##        method is not really needed or used here).
##
## cacheSolve() creates an object first and then sees if the inverse 
## was calculated already by calling getInverse method. If it returns NULL 
## then the inverse is calculated and then stored using setInverse.
## 
## To use these functions first create a matrix which has an inverse. 
## One way to do this is the following: mx <- matrix(runif(16), ncol = 4) 
## where mx is now 4 x 4 matrix with random values. Other square matrices 
## can be created in a similar manner. Then the matrix with the 4 methods 
## can be created by using the following: 
## new_mx <- makeCacheMatrix(mx) where new_mx is now a list/object with
## 4 different methods:
## - new_mx$get
## - new_mx$setInverse
## - new_mx$getInverse
## - new_mx$set
## 
## cacheSolve(new_mx) will now calculate the inverse of the matrix if
## it was not calculated before. If it was then the inverse would have
## been saved/cached so there is no need to recalculate it again.

## This function receives a matrix and will create an object with that
## matrix along with 4 methods mentioned above. This function uses
## R's lexical scoping rules to avoid calculating the inverse of the matrix
## more than once.
makeCacheMatrix <- function(x = matrix()) {
        ## x is a matrix which is assumed to have an inverse
        ## m is a variable which is in the environment of makeCacheMatrix()
		## and will be used to store the inverse.
        m <- NULL
        
        ## set the matrix
        ## this function is not needed unless the matrix needs to be modified
        ## after the call to cacheSolve()
        ## cacheSolve() must be called again to recalculate the inverse afterwards
        set <- function(y) {
                ## x is in the scope of the parent function which is the original object
                ## x is going to be changed to y, the passed in parameter
                x <<- y
                ## this resets the inverse to null since the original matrix was changed
                ## and needs to be recalculated again
                m <<- NULL
        }
        
        ## get the original matrix
        get <- function() x
        
        ## set the inverse
        ## the <<- operator tells the function setInverse to use m
        ## from the calling function which is the parent environment
        ## and not local to setInverse
        setInverse <- function(inverse) m <<- inverse
        
        ## get the inverse
        ## m is a free variable inside getInverse
        ## and its value is defined in makeCacheMatrix
        getInverse <- function() m
        
        ## list contains methods for the matrix operation
        ## the list is returned by this function
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This function will calculate the inverse of the matrix but only
## if the inverse was not calculated before. If the inverse was
## already calculated then it's simply recalled from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'. Note that
		## x is an object with 4 methods.
		
        ## Attempt to get the inverse of the matrix if it was already calculated
        m <- x$getInverse()
        if(!is.null(m)) {
                ## if the function to get the inverse returns a non-null value
                ## then it means it was already calculated and stored
                message("getting cached inverse")
                temp <- m
        } else {
                ## get the original matrix
                data <- x$get()
                ## calculate the inverse and save in m
                ## solve() returns the inverse of the matrix
                m <- solve(data, ...)
                ## store the inverse
                x$setInverse(m)
                ## return the inverse
                temp <- m
        }
        
        ## return the inverse
        return(temp)
}

