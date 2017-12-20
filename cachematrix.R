## makeCacheMatrix creates a list of functions which completely contain the environment for makeCacheMatrix (due to lexical scoping)
## cacheSolve returns the inverse of the matrix by 1) retrieving a cached inversion or 2) calculating it's inverse if no cached exists


## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getsolved <- function() inv
    list( set = set, get = get, setinverse = setinverse, getsolved = getsolved)
}


## cacheSolve returns the inverse of the matrix by:
## 1. checking if the inverse has been previously calculated and stored in cache
## 2. if a cached value exisits it will return this
## 3. if no cached value exisits it will compute and return the inverse using the solve() function

cacheSolve <- function(x, ...) {
    inv <- x$getsolved()
    if(!is.null(inv)) {
        message("getting cached matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}

## practice run
> m <- matrix(9:12,nrow = 2, ncol = 2)
> myMatrix <- makeCacheMatrix(m)
> myMatrix$get()
[,1] [,2]
[1,]    9   11
[2,]   10   12
> 
    > cacheSolve(myMatrix)
[,1] [,2]
[1,]   -6  5.5
[2,]    5 -4.5
> cacheSolve(myMatrix)
getting cached matrix
[,1] [,2]
[1,]   -6  5.5
[2,]    5 -4.5












