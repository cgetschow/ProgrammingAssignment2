
# makeCacheMatrix constructs a list of functions to support
#   caching a matrix and its inverse.
#
#  set : store matrix in parent environment of set function
#  get : get the stored matrix value
#  setinverse : calculate matrix inverse and store it in parent env
#  getinverse : get the inverse value 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# cacheSolve returns the inverse of the matrix, either from
#  the cache or by calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

### Usage example:
# > m<-makeCacheMatrix()
# > m$set(matrix(c(1,2,3,4),nrow=2,ncol=2))
# > m$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(m)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(m)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > 