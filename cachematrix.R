#makeCacheMatrix  function creates a special "vector", 
#which is really a list containing a function to
#
#set the matrix
#get the matrix
#set the inverse matrix
#get the inverse matrix


makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}
#The following function copmutes the inverse of the special "vector" 
#created with the above function. 
#If the inverse matrix has already been computeso, 
#it gets the result from the cache and skips the computation. 
#Otherwise, it computes the inverse matrix of the matrix and sets 
#the inverse matrix in the cache via the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}

