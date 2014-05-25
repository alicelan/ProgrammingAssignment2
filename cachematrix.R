## The following pair of functions are used to create an object 
## that stores a matrix and caches its inverse.

## The "makeCacheMatrix" function creates a special "matrix", 
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set=set, get=get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the 
## special "matrix" created by the makeCacheMatrix function above. 
## If the inverse of matrix has already been calculated, then
## the cacheSolve function gets the inverse from the cache and
## skips the computation. Otherwise, it calculates the inverse
## of the matrix and sets the value of the inverse via the 
## setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
              message("getting cached data")
              return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
