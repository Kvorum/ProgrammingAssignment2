## Here is a pair of functions that cache the inverse of a (square) matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m_inv <- NULL
        set <- function(y) {
                x <<- y
                m_inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m_inv <<- solve
        getInverse <- function() m_inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated(and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m_inv <- x$getInverse()
        if(!is.null(m_inv)) {
                message("getting cached matrix inverse")
                return(m_inv)
        }
        matrix <- x$get()
        m_inv <- solve(matrix, ...) ## here might be an error 
                                    ## (ex matrix(17:32,4))
                                    ## needed to improved
        x$setInverse(m_inv)
        m_inv
        
}
