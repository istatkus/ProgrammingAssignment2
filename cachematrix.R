## Coursera Assignment 2: Cache Inverse of Matrix
## To confirm my result went to Khan Academy and watched the Matrix video
## the inverse is 1/determinate(x) * Adjugate(x)
## In R solve(x) seems to perform this function
## Used their 2 by 2 as my test case
##
## How used
## 
## m3<-makeCacheMatrix(matrix(c(3,-7,5,2),2,2)) ## my test case
## this creates the options for get,set,getinvrse and setinvrse
## m3$get() ## returns the above matrix
##
## m3$set(matrix(c(1,2,3,4),2,2) ) ## changes the matrix to 1:4
## m3$get() ## now returns the new matrix
## m3$set(matrix(c(3,-7,5,2),2,2)) ## set the saved one back to my test case
## 
## [1,] 0.04878049 -0.12195122
## [2,] 0.17073171  0.07317073
## cacheSolve(m3) ## displayed expected result
## cacheSolve(m3) ## repeating it now included the message
## m3$set(matrix(c(2,3,4,5),2,2) ) ## changes the matrix 
## cacheSolve(m3) ## displayed expected result of new matrix
##
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL             ## Inverse is initialized
        set <- function(y) {
          x <<- y             ## make x the new matrix
          i <<- NULL          ## reset result for the new matrix
        }
        get <- function() x
        setinvrse <- function(solve) i <<-solve
        getinvrse <-function() i
        list(set = set, get = get,
             setinvrse = setinvrse,
             getinvrse = getinvrse
          )
}


## cacheSolve is what actually calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinvrse()    ## get the stored inverse or null if not there
        if (!is.null(i)) {    ## check if the inverse exists
                              ##
            message("getting cached inverse of matrix")
            return(i)         ## the message helps you see that it was 
                              ## retrieved and not calculated
        }
        data <-x$get()        ## gets the saved matrix
        i <- solve(data,...)  ## this is where the real work is done 
                              ## for the inverse which in R is solve()
        x$setinvrse(i)        ## stores the result
        i                     ## displays it
}
