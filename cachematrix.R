# Function 1 (makeCacheMatrix) --> create a matrix that can cache its inverse.
# Function 2 (cacheSolve)      --> create the inverse of makeCacheMatrix (in function 1)

# makeCacheMatrix:

# the input is a matrix 'x'
# 'm' is the inverse matrix and when makeCacheMatrix is called, m will be set to NULL.
# 'get' returns the value of the matrix
# 'setinversematrix' stores the matrix
# 'getinversematrix' returns the matrix
# 'list' returns a list of the internal functions 'get', 'setinversematrix', 'getinversematrix', so a calling function will have access to these functions

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL       
    get <- function() { x }  
    
    setinversematrix <- function(matrix)  { m <<- matrix }
    
    getinversematrix <- function() { m } 
    
    list(get = get, 
         setinversematrix = setinversematrix,
         getinversematrix = getinversematrix) 
}

# cacheSolve:

# the input is a matrix 'x', which was created with the previous function 'makeCacheMatrix'
# 'm' will get the value of the matrix within x
# if the matrix is not NULL, print "getting cached data" and retrieve the matrix from cache.
# if the matrix is NULL the inverse matrix is returned 
# store the inverse matrix in x with 'x$setinversematrix(m)'
# 'm' prints the inverse matrix of matrix 'x'

cacheSolve <- function(x, ...) {
    m <- x$getinversematrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinversematrix(m)
    m
}
