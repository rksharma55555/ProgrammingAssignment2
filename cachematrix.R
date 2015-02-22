## It takes too much of time to compute inverse of a matrix if the source matrix is too big and hence caching of inverse of a ##finction is done to prevent its compilation repeatedly.


## makeCacheMatrix function creates list of functions
## 1. Set value of matrix
## 2. Get value of matrix
## 3. Set the value of inverse of a matrix
## 4. Get the value of inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
x <<- y
inv <<- NULL
        }
   
get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(set = set, get= get,setinverse = setinverse,getinverse = getinverse)
}





## The below function returns inverse of matrix. It first checks if the inverse of matrix is already computed.It skips if yes.
## Otherwise it calculates the inverse and sets the value in cache using function setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        ##Returns inverse of matrix
        inv <- solve(data)
        x$setinverse(inv)
        inv
}




## Test Run

## > x=rbind(c(1,2),c(3,4))
## > m = makeCacheMatrix(x)
## > m$get()
##     [,1] [,2]
## [1,]    1    2
## [2,]    3    4
## > cacheSolve(m)
##     [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
## > cacheSolve(m)
## getting cached data
##     [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5

