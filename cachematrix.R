## MakeCacheMatrix function creates a matrix and returns a list of functions 
## to access and manipulate the matrix that you've created. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        geti <- function() i
        ## The set function is an alternative to creating the matrix using the makeCacheMatrix. The set 
        ## function can also be used to assign a different value to x after the makeCacheMatrix object 
        ## is created.
        set <- function(y = matrix()) {
                x <<- y
                i <<- NULL        
        }
        ## The get function simply return the matrix that you've created using makeCacheMatrix or 
        ## using the set function
        get <- function() {
                x
        }
        ## The setInverse function takes a matrix as input and sets the enclosing environment variable
        ## to store the inverse of a matrix. The setInverse function does not calculate the inverse. 
        ## It just  sets the value of the enclosing environment variable.
        setInverse <- function(inverse = matrix()) {
                i <<- inverse
        }
        ## The getInverse function returns the value of i which is set to NULL in the evaluation 
        ## environment or is set using the setInverse function. The function will check the 
        ## evaluation environment first for the definition of i and move to the parent Environment.
        getInverse <- function() {
                i
        }
        ##The makeCacheMatrix function returns a list of functions defined that can be called 
        list(geti = geti, get = get, set = set, setInverse = setInverse, getInverse = getInverse)

}


## cacheSolve function uses the makeCacheMatrix function to either return the cached environment
## variable. If the cached value is NULL, the function calculates the inverse of a matrix using the 
## solve function and sets the value of the enclosing Environment variable by calling the setInverse 
## function of the makeCacheMatrix object ad also return the inverse matrix to the caller.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
