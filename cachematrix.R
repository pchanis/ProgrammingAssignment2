## makeCacheMatrix() and cacheSolve() are two functions that can be used together to compute and cache the 
## inverse of a matrix. Given a matrix, makeCacheMatrix() will construct a list containing four functions.
## The four functions in the list are used to get or set the matrix and to get or set the inverse of the matrix.
## The function cacheSolve() is passed the list built by makeCacheMatrix() and will return the inverse of the matrix.
## If the inverse of the matrix has already been computed then cacheSolve() will return the cached inverse.
## If the inverse has not been calculated then cacheSolve will calculate the inverse and then cache it.
## The caching is accomplished by storing the value in variables defined
## in the lexical scope of makeCacheMatrix()

## return a list of functions used to access and set the inverse of an invertible matrix passed as an argument.
## The returned list also contains functions to access or re-set the matrix 
makeCacheMatrix <- function(x = matrix()) {
        
        ## variable to hold the inverse of the matrix
        inverse <- NULL
        
        ## define a function that stores the matrix. This function will also reset the inverse to NULL.
        set <- function(new_matrix) {
                x <<- new_matrix
                inverse <<- NULL
        }
        
        ## define a function that returns the stored matrix
        get <- function() x
        
        ## define a function that stores the value of the matrix inverse
        setinverse <- function(inv) inverse <<- inv
        
        ## define a function used to return the inverse of the matrix
        getinverse <- function() inverse
        
        ## the returned list object containing the four functions defined above
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## this function is passed a list object 'x' created by makeCacheMatrix() and will return
## the inverse of the matrix stored by makeCacheMatrix(). If the inverse has not
## been calculated yet then it will calculate and store it before returning it.
cacheSolve <- function(x, ...) {
        
        # retrieve the inverse of the matrix
        i <- x$getinverse()
        
        # if the inverse is not NULL then exit the function returning the inverse
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        # inverse is NULL so it must be calculated
        # retrieve the matrix
        data <- x$get()
        
        #calculate the inverse
        i <- solve(data)
        
        #store the inverse
        x$setinverse(i)
        
        #return the inverse
        i
}
