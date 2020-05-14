##Author: Wei Zhang
##DAte: 5/13/2020
##R-Programming Assignment2: Lexical Scoping
##cache potentially time-consuming computation of the inverse matrix

##makeCacheMatrix creates a list, containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse of the matrix
##get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function() x
        
        setInverseMatrix <- function(m) inverseMatrix <<- m
        getInverseMatrix <- function() inverseMatrix
        
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix
        )
}


##cacheSolve returns the inverse of the matrix in the above function, 
##it first checks if the inverse has already been solved, if so, it gets the inverse from the cache,
##otherwise it would solve and store the inverse in the cache for future fetching.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getInverseMatrix()
        if (!is.null(inverseMatrix)) {
                message("getting cached inverse matrix")
                return(inverseMatrix)
        }
        inverseMatrix <- solve(x$get())
        x$setInverseMatrix(inverseMatrix)
        inverseMatrix
}
