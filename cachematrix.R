
##  This function creates a special "matrix" object that can cache its inverse
##  get is a function to display the matrix
##  setInverse is a function to set the inverse of the matrix
##  getInverse is a function to display the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This function returns a matrix that is the inverse of 'x'
## It first checks if there is already a value existing for inverse
## If there is, then it displays "getting cached data" followed by the inverse
## If not, it computes the inverse of the matrix and then displays it

cacheSolve <- function(x) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
        
}

