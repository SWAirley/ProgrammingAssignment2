
## Creates a special list that contains functions to get matrix, get the inverse of the matrix, set the inverse.
##
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        ## solve() function will creat the inverse of a square matrix
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        
        ## Creates a list with the functions, set(), get(), setinv(), getinv()
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

##Solves for the inverse of a matrix but checks if the solution is cached and returns that if solved.
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        
        ##Checks to see if the inverse is cached already and returns it if it is.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		
        ##Solves for the inverse of a matrix.
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}