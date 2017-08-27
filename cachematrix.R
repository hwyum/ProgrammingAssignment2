## Getting cached inversed matrix to prevent repeaed inverse caclulation for the same matrix
## You can set the object matrix by using makeCacheMatrix
## You can calculate the inverse with cacheSolve() with the argument of type 'makeCacheMatrix()'
## If inversed matrix is already cached in the 'makeCacheMatrix()' environment, then
## No repeated cacluation is occured when cacheSolve() is called. just retrive data. 


## makeCahceMatrix ##
## It returns list of 4 basic functions consisting of 2 getters and 2 setters.
## 2 getters: getting matrix passed by argument and getting inversed matrix
## 2 setters: setting matrix with passed argument and 
## setting inversed matrix to cache in the parent environment. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ## i = inverse of matrix x, initialized with NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve ##
## argument x is a list of functions defined within makeCacheMatrix environment
## with this argument we can access to already cached inverse matrix, 
## or newly calculate inverse matrix with 'solve' function
## Finally it returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
