# Function designed to create a list containing functions to set a matrix in an 
# environment and to get the matrix back from the environment


makeCacheMatrix <- function(x = matrix()) {
        # Setting the NULL value to the inverse matrix variable
        inverse <- NULL
        
        # A function to set the new matrix to the environment
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        # A Function to get the matrix from the environment
        get <- function() x
        
        # A funtion to call cachesolve and set the inverse matrix to the environment
        setinverse <- function() inverse <<- cacheSolve(x)
        
        # A function to get the inverse matrix from the environment
        getinverse <- function() inverse
        
        # Adding the functions to the list
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


# This function checks if the inverse of a matrix had already been calculated and,
# if so, returns the stored inverse matrix. Otherwise, calculates and returns the 
# inverse function.

cacheSolve <- function(y, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #Check if the inverse matrix was calculated and stored previously
        inverse = y$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        # Calculate and return the inverse matrix
        originalmatrix <- y$get()
        inverse <- solve(originalmatrix)
        inverse
        
}
