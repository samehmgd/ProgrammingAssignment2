## Here is a code i wrote to compute the inverse of the special "matrix" returned by makeCacheMatrix (function that returns a list of functions) 
## followed by retriving the cachesolve which retrieved the inverse from the cache.
## Here, i called makeCacheMatrix without arguments first

makeCacheMatrix <- function(x = matrix()) {
        loc <- NULL
        set <- function(y) {
                x <<- y
                loc <<- NULL
        }
        get <- function() x
        setlocerse <- function(locerse) loc <<- locerse
        getlocerse <- function() loc
        list(set=set, get=get, setlocerse=setlocerse, getlocerse=getlocerse)
}

## # cache the argument 
cacheSolve <- function(x, ...) {
        loc <- x$getlocerse()
        if(!is.null(loc)) {
                message("generating cached data")
                return(loc)
        }
        data <- x$get()
        loc <- solve(data)
        x$setlocerse(loc)
        loc
}


#testing some data
testdata <- matrix(data = c(1,15,9,4), nrow = 2, ncol = 2)
cachedmatrix = makeCacheMatrix(testdata)
cachedmatrix$get()
# There is no cache in the first run
cacheSolve(cachedmatrix)
# running the code for the second time will give the cached value of "cachematrix"
cacheSolve(cachedmatrix)

