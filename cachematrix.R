


#
# Define makeCacheMatrix() & cacheInverse() functions
#

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
             setInverse = setInverse ,
             getInverse = getInverse )

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getInverse ()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse (m)
        m
}


#
#   Create matrix 
# 

A = matrix( 
   c(2, 4, 3, 1), # the data elements 
   nrow=2,              # number of rows 
   ncol=2)              # number of columns 
print(A)

#
# Use makeCacheMatrix() & cacheInverse() functions
# 

m = makeCacheMatrix(A)
print(m)


mi = cacheInverse(m)  # will calculate inverse 
print(mi)

mi = cacheInverse(m)  # will returned cached inverse 
print(mi)
