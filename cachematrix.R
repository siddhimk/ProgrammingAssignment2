## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
# Function to cache inverse of input data
    # Initialize inverse matrix 
    inv <- NULL
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    # Get matrix x
    get <- function() x
    
    setinv <- function(inverse) inv <<- inverse
    
    # Get inverse of matrix x
    getinv <- function() inv

    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
# Function to get the inverse of matrix
# If new input data is provided then this function calculates the inverse of matrix
# If same input data is provided then this function will return cached results of previous computation
    inv <- x$getinv()
    if(!is.null(inv))
    {
        message("read cached data")
        return(inv)
    }
    input_data <- x$get()
    
    # solve function below calculates the inverse of a matrix
    inv <- solve(input_data, ...)
    x$setinv(inv)
    inv
## Return a matrix that is the inverse of 'x'
}
