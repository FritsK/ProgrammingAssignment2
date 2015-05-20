## these functions are build for the Coursera R Assessment 2
## my GitHub repo is at https://github.com/FritsK/ProgrammingAssignment2

## call the functions for testing using statements similar to the following:
## a <- matrix(1:4,2,2)
## b <- matrix(2:5,2,2)
## c <- matrix(c(2,2,2,1,0,0,0,0,1),3,3)
## test.a <- makeCacheMatrix(a)
## test.a ## this shows the connected functions and cache-environment
## cacheSolve(test.a) # first time inverse will be calculated
## cacheSolve(test.a) # next times inverse will be found from cache
## test.b <- makeCacheMatrix(b)
## cacheSolve(test.b) # first time inverse will be calculated
## cacheSolve(test.b) # next times inverse will be found from cache
## cacheSolve(test.a) #  cache for test.a is still available


## the function makeCacheMatrix creates a list of functions
## that are connected to the object to which the function output is assigned
## and reserves a cache-environment for each object

makeCacheMatrix <- function(x = matrix()) {
        x.inverse.cache <- NULL              #create empty object x.inverse.cache
        #setmatrix (as in example) is not necessary
        getmatrix <- function() {
                x                            #return the input matrix
        }
        setinv <- function(forcache) {
                x.inverse.cache <<- forcache #load x.inverse.cached located in cache with matrix calculated in cacheSolve
        }
        getinv <- function() {
                x.inverse.cache              #get cached x.inverse.cache matrix
        }
        list (getmatrix=getmatrix, setinv=setinv, getinv=getinv) #return list of functions
}


## the function cacheSolve takes an object as input that is the
## functionlist constructed by assigning makeCacheMatrix() to the object 
## in which a matrix and its inversion have a place in cache.
## if the cache is still empty, it will calculate the inversion 
## and subsequently place it in cache using te x.setinv function from the functionlist
## so it is present for future calls.
## if the cache for the object is filled, it gets the cached value
## finally it returns the inversion matrix (calculated or cached)

cacheSolve <- function(x, ...) {
        x.inverse <- x$getinv()
        if(!is.null(x.inverse)) {       #FIRST try if cached data is available (x.inverse is not null)
          message("getting cached data")
          return(x.inverse)             #exit function now the result is already there
        }
        data <- x$getmatrix()           #if function arrives here, no cache was available; put matrix in data
        message("calculating inverse ...")
        x.inverse <- solve(data, ...)   #compute inverse
        x$setinv(x.inverse)             #call setinv to cache the calculated inverse matrix
        x.inverse                       #return calculated inverse
}