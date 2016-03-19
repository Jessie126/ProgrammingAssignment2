## This two functions together could cache the inverse of a squared matrix
## when we need it again, it can be looked up in the cache rather than recomputed.


## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
            mat.inv <- NULL #inversion result is stored here
            set.mat <- function(y) {
                x <<- y
                mat.inv <<- NULL #initializes mat.inv to null
                }
            get.mat <- function() x  
            ## set.mat(a); get.mat()
            set.inv <- function(inv) mat.inv <<- inv  #inverse matrix set
            get.inv <- function() mat.inv    #inverse matrix returned
            ## inv <- solve(a) ; set.inv(inv); get.inv()
            
            list(set.mat = set.mat, get.mat = get.mat, 
                 set.inv = set.inv, get.inv = get.inv)
}


## function cacheSolve computes the inverse of the special "matrix", returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
             mat.inv <- x$get.inv() # obtaining inversed matrix from object x
             if(!is.null(mat.inv)) {  #if inversion already exist
                 message("getting cached data")
                 return(mat.inv) #returns the value of inversion
        }
            
                my.mat <- x$get.mat() #retrieve matrix 
                inv <- solve(my.mat) ## calculate inverse of object (matrix) 
                x$set.inv(inv) # get value of inverse
                get.inv()
            
}

## testing
#a<-matrix(c(1,2,0,4),2,2);a
#solve(a)
#makeCacheMatrix(a) 
#cacheSolve(makeCacheMatrix(a))
