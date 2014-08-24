## ======================== makeCacheMatrix ===================================
##This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ##The function return NULL if we previously not evaluate the inverse matrix with cacheSolve
        ##m is evaluated in cacheSolve
        m <- NULL
        
        ##Get and set methods of makeCacheMatrix as likely to be used in
        ##a "public" way, while setInverse and getInverse would only be 
        ##used by the cacheSolve function.
        
        ##In case we want to create a new matrix, destroying the old one 
        ##in the process; variable 'x' is reassigned before the next 
        ##call to get() unlike the original call to makeCacheMatrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        } 
        ##Get defined matrix in the current environment
        get <- function() x
        
        ##<<- operator can be used to assign a value to an object 
        ##in a different environment, this will be the environment of "cacheSolve"
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        
        ##Return values - list with its factors names-
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## ======================== cacheSolve ===================================
##This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		##We assign the variable getinverse (of x) to m. Initially m is Null
		m <- x$getinverse()
		
		##In a secound round, m is the inverse of the matrix. So "getting cache data"
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		
		##matrix defined in "makeCacheMatrix"
        data <- x$get()
		
		##calculate the inverse
        m <- solve(data, ...)
		
		##and setinverse in x. Thus, in a second round, m is not null and will return the inverse of the cache
        x$setinverse(m)
        m
}
