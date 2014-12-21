# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
cacheSolve <- function(x, ...) { 
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m                             
}

#testing
# mydata<-rnorm(36)
# mymatrix<- matrix(mydata, nrow=6, ncol=6)
# mymatrix
# myinv<-solve(mymatrix)
# myinv
# a<-makeCacheMatrix(mymatrix)
# b<-cacheSolve(a)
