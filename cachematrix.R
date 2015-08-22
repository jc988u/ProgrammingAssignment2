## The following two functions are defined to cache the inverse of a matrix rather than compute it repeatedly.
##  
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## It is really a list containing a function to
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse (solve result)
##    get the value of the inverse (solve result)

makeCacheMatrix <- function(x = matrix()) {
                sol <- NULL
                set <- function(y) {
                        x <<- y
                        sol <<- NULL
                }
                get <- function() x
                setsolve <- function(solve) sol <<- solve
                getsolve <- function() sol
                list(set = set, get = get,
                     setsolve = setsolve,
                     getsolve = getsolve)
}


## 
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## It first checks to see if the matrix inverse has already been calculated. 
## If so, it gets the matrix inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache 
## via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        sol <- x$getsolve()
        if(!is.null(sol)) {
                message("getting cached data")
                return(sol)
        }
        data <- x$get()
        sol <- solve(data, ...)
        x$setsolve(sol)
        sol
}
