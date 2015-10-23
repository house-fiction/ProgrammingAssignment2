## The following functions makeCacheMatrix and cacheSolve calculate the inverse 
## of a matrix, the result is saved to the cache. If a second matrix inverse 
## has to be calculated and the contents of a vector have not changed then the 
## result can be looked up in the cache rather then recomputed.


## Function makeCacheMatrix 
##
## Creates a special "matrix" object which is a list containing a function to:
##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse
##  get the value of the inverse
##

makeCacheMatrix <- function(x = matrix()) {
        
        ## Define cache as m
        m <- NULL
        set <- function(y) {
                ## Assign input matrix y to variable x in the parent environment
                x <<- y 
                ## Re-initialize Cache m in the parent environment
                m <<- NULL 
        }
        ## Return matrix x
        get <- function() x 
        
        ## Set cache m equal to the inverse of matrix x
        setinverse <- function(inverse) m <<- inverse 
        
        ## Return cached inverse of x
        getinverse <- function() m 

        # List with 4 functions        
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Function cacheSolve 
##
## computes the inverse of the special "matrix" returned by function 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix 
## has not changed), then this function should retrieve the inverse from the 
## cache directly.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        # Check if the inverse has already been caclulated
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # Otherwise the matrix inverse has to be calculated by using the 
        # solve() function in R
        data <- x$get()
        m <- solve(data, ...)
        
        # Set value of the inverse in the cache
        x$setinverse(m)
        m
        }
