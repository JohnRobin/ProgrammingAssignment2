## makeCacheMatrix returns a list containing four functions for storing and
##      retrieving matrix data, as well as storing and retrieving its inverse
## By creating a makeCacheMatrix object, repeated use of the solve() function to 
##      determine its inverse may be avoided, provided the data are unchanged
## A makeCacheMatrix object is created by calling the function with a matrix argument
##      i.e. with solveable matrix y e.g yCacheMatrix<-makeCacheMatrix(y), at this stage the matrix 
##      data from y are stored in yCacheMatrix, and the functions become available
##      yCacheMatrix can now be interrogated for its inverse using the cacheSolve function below

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                ## Initialise m as NULL
        set <- function(y) {     ## Create a function to store the data, as a matrix
                x <<- y
                m <<- NULL       ## Reset m to NULL as new data is entered
        }
        get <- function() x      ## Create a function to return the data, as a matrix
        setinverse <- function(inverse) m <<- inverse  ## Create a function to store the inverse as a matri
        getinverse  <- function() m                    ## Create a function to return the inverse as a matrix
        list(set = set, get = get,                     ## Name the functions        
             setinverse = setinverse,
             getinverse = getinverse)
}


## Cachesolve returns the inverse of its makeCacheMatrix object
##      it first checks to see if an inverse matrix for the object exists
##      if so it returns this immedately
##      if not if gets the originl matrix data from the makeCacheMatix object
##      calls the solve function, stores the invere back into the makeCacheMatrix object
##      and returns the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## Example use
## y <- matrix(rnorm(64))

## ycache <- makeCacheMatrix(y)
## cacheSolve(ycache)
##      on its first call this will solve the matrix, which can be time consuming
##      subsequent calls return the solved result from storeage 
## y=matrix(rnorm(64),nrow=8,ncol=8)
## y    ## The original matrix
## [,1]       [,2]        [,3]         [,4]
## [1,] -0.3668298 -0.2744042 -0.05431100 -0.130539664
## [2,] -0.3037478 -0.2324054  1.40633770  0.005813335
## [3,] -0.2449634 -0.4798002 -0.08583045  0.909985081
## [4,]  0.7864438  1.7612811  1.38339762  0.518948416
##
## ycachematrix <- makeCacheMatrix(y)
##
## cacheSolve(ycachematrix) ## First call inverse returned
## [,1]       [,2]        [,3]       [,4]
## [1,] -4.47053478  0.4987692 -0.24652738 -0.6978447
## [2,]  2.43155193 -0.7201019 -0.11444864  0.8204028
## [3,] -0.56384538  0.7005337 -0.07614861 -0.0161529
## [4,]  0.02543616 -0.1793411  0.96502842  0.2431870

## cacheSolve(ycachematrix) ## Scond call inverse returned from the cache
## getting cached data
## [,1]       [,2]        [,3]       [,4]
## [1,] -4.47053478  0.4987692 -0.24652738 -0.6978447
## [2,]  2.43155193 -0.7201019 -0.11444864  0.8204028
## [3,] -0.56384538  0.7005337 -0.07614861 -0.0161529
## [4,]  0.02543616 -0.1793411  0.96502842  0.2431870
