## The following two functions implement caching of matrix inversion
## The first function , makeCacheMatrix, gets a matrix and constructs a 
## Data structure that holds on top of the matrix, the history of its 
## inversion attempts, and thus allows caching by using cacheSolve

## makeCacheMatrix gets a matrix (square and invertible) and returns
## a data structure that holds the matrix, along with its inversion histroy
## in order to allow caching of the inversion operation 

makeCacheMatrix <- function(x = matrix()) { ## x is a square and invertible matrix
        m <- NULL   ## since the matrix is defined in this function, an inversion 
                    ## was not yet performed, therefore m, the inversion value, is set to NULL
        set <- function(y) { ## defining a function that sets the value of x 
                x <<- y
                m <<- NULL
        }
        get <- function() x ## a function that returns the matrix to be inverted
        setinv <- function(invmat) m <<- invmat ## a function that sets the inv of the matrix to the value invfunc
                                                ## this function should not be called directly
                                                ## for it will corrupt the data
        getinv <- function() m    ## this function returns the value of the matrix inversion
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv) ## makeCacheMatrix returns the data structure as a list
}


## cacheSolve performs a cached calculation of the matrix inversion operation 
## it gets a data structure created by makeCacheMatrix
## if the inversion was already operated on the matrix in this data structure it will return its value
## without repeating the inversion
## however, if the calculation was not yet performed, it will perform it and store it accordingly

cacheSolve <- function(x, ...) {
    m <- x$getinv() ## loading the stored matrix inversion
    if(!is.null(m)) { ## if it is not null, it means it has already been calculated, and the cached value is loaded
        message("getting cached data")
        return(m)
    }
    ## otherwise:
    data <- x$get() ## the matrix is loaded into data
    d <- det(data) ## checking if the matrix is invertible (was not requested in the assignment, but it is good practice)
    if(d!=0) { ## matrix is invertible: its inverse is storeed in m
        m <- solve(data, ...)
    } else{ ## matrix is non-invertible: Inf is storeed in m and a message is presented to the user
        message("matrix is not invertible")
        m <- Inf
    }
    x$setinv(m) ## setting the value of the inversion to m and returning m
    return(m)
}
