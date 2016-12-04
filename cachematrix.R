## The combination of the two following functions calculates the inverse of a given matrix and stores it to cache memory.
## When recalled, it returns the cached matrix without calculating it again.

## To work properly, you need to run your matrix through makeCacheMatrix, assign the result to a variable.
## The environment of makeCacheMatrix as well as these of its functions are now stored in the variable.
## Now call cacheSolve using the variable as the arguement. This calculates the inverse matrix and stores it in cache.
## If you recall cacheSolve using the same variable, you will get the cached matrix and a message.

## makeCacheMatrix takes a matrix as an argument and returns a "special" vector of its functions (as well as its environment).

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolved <- function(solve) m <<- solve
    getsolved <- function() m
    list(set = set, get = get,
         setsolved = setsolved,
         getsolved = getsolved)
}


## cacheSolve takes a "special" vector as an argument and examines its environment to see if theres already a cached value stored in it (m).
## If so, it returns the value of m.
## If not, it calculates the inverse and returns it (as well as storing it for future use). 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolved()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolved(m)
    m
}
