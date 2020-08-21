## Put comments here that give an overall description of what your
## functions do

## creates a cache object for the inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL 
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function () {x}
    setinv <- function(inv) {inverse <<- inv}
    getinv <- function() {inverse} 
    object = list(set = set, get = get, setinv = setinv, getinv = getinv)
    return(object)
}


## calculates the inverse if no cache is cached or retrieves the cache if present
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinv()
    if (!is.null(inverse)) {
        print('get cached data')
        return(inverse)                   #will return here and break out of function if cached is present 
    }
    data <- x$get() 
    inverse <- solve(data,...)
    x$setinv(inverse)
    return(inverse)
}

n=100
mat = matrix(sample(1:(n*n), n*n), nrow=n, ncol=) #create matrix with random numbers

mat_special = makeCacheMatrix(mat) #make special matrix for use with cache


#call nr 1, calculate inverse
inverted = cacheSolve(mat_special) 

#call nr 2, should retrieve cache instead and print "get cached data" 
inverted2 = cacheSolve(mat_special)

#check if identical 
identical(inverted,inverted2)


