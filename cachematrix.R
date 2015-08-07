## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## return: a list containing functions to
##              1. set the matrix 2. get the matrix3. set the inverse 4. get the inverse
##              input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {        
        inv = NULL
        set = function(y) {
                
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
        ## Return a matrix that is the inverse of 'x'
        ## @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
cacheSolve <- function(x, ...) {
        
        inv = x$getinv()
        
        
        if (!is.null(inv)){
                 
                message("getting cached data")
                return(inv)
        }
        
        
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
       
        x$setinv(inv)
        
        return(inv)
}





        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}











}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
