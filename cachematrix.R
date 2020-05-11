##write a pair function that cache the inverse of a matrix. 

##creates a special 'matrrix' object that can be cache its inverse
makeCacheMatrix <- function(x = matrix()) {
     ##initlize inverse property 
     i <- NULL
     
     ##set the matrix
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     
     ##get the matrix
     get <- function() {x}
     
     ##set the inverse of the matrix
     setInverse <- function(inverse) {
          inv <<- inverse
     }
     
     ##get the inverse of the matrix
     getInverse <- function() {inv}
     
     ##return a list of the methods
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##computes the inverse of the special matrix returned by above. If the inverse has already been calculated, then the catchsolve should retrive the inverse from the cache. 
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inv <- x$getinverse()
     
     ##return inverse if its already set 
     if(!is.null(inv)) {
          message("getting cached data.")
          return(inv)
     }
     
     ##get the matrix from object 
     data <- x$get()
     
     ##calculate the inverse using matrix multiplication 
     inv <- solve(data)
     
     ##set the inverse to the object 
     x$setinverse(inv)
     
     ##retun matrix
     inv
}
