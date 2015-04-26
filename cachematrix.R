## Function for given matrix x returns the list of 6 available functions:
## set, get -sets or gets the matrix 'x' for which the inverse one will be calculated
##setinverse,getinverse-the same as above but for the inverse matrix 'm'
##ifchanged-returns TRUE if the inverse matrix 'm' was not updated since updating basic matrix 'x'
##resetchangesflag-sets changed to FALSE to keep the information that there was no changes
## on basic matrix 'x' since last inverse matrix 'm' calculation

makeCacheMatrix <- function(x = matrix()) {
	   m <- NULL
        changed<-FALSE
        set <- function(y) {
            x <<- y
            m <<- NULL
            changed<<-TRUE
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        ifchanged <- function() changed
        resetchangesflag<-function() changed<<-FALSE
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse,
             ifchanged=ifchanged,
             resetchangesflag=resetchangesflag)
}


## Function takes as argument makeCacheMatrix object.
##Checks if there is inverse matrix already in cache and if there was no changes on the basic matrix from last computation 
## the inverse. If not (for both conditions) then just taking the inverse matrix from cache otherwise it calculates it
##with function solve and sets resets changes flag on the argument as the inverse matrix is updated now and 
## it references the basic matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m) && !x$ifchanged()) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$resetchangesflag()
    x$setinverse(m)
    m
}
