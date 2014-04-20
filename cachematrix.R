## makeCacheMatrix will cache the inverse of a matrix
## It will test for a square matrix

## get() will get the matrix of type makeCacheMatrix
## set() will set the matrix of type makeCacheMatrix and initialize its inverse
## getinverse() will get the inverse of the matrix
## setinverse() will set the inverse of the matrix

## Annie Flippo - 4/16/2014  Initial Creation
## Annie Flippo - 4/20/2014  Added comments

makeCacheMatrix <- function(x = matrix()) {
        
        x <- x
        
        # Initialize inv_x (inverse of X) to NULL 
        inv_x <- NULL
        
        # Find number of rows and columns for x
        nCol <<- ncol(x)
        nRow <<- nrow(x)
        
        # If it's not a square matrix then get out - you're all done
        if (nRow != nCol) {
                stop("It is not a square matrix")
        } 
        
	# Sets the x matrix with the argument passed in as y
	# Sets the inv_x as a matrix of a NULL
        set <- function(y) {
                x <<- y
                inv_x <<- matrix() 
        }

	# gets the original matrix 
        get <- function() {
                x
        }

        # Sets the inverse of the matrix with the argument passed in as matrixInverse
        setinverse <- function(matrixInverse) {
               inv_x <<- matrixInverse
        }

        # Gets the inverse of the matrix, inv_x
        getinverse <- function() {
                inv_x
        }

        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse )

}


## cacheSolve will try to get the cached value of the inverse of the matrix
## otherwise it will calculate and set the inverse for the matrix

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        
        # Gets the inverse of x
        inv_x <- x$getinverse()
        
        # Check to see if the inverse of matrix is NOT NULL
	# If it's NOT NULL, return inv_x as retrieved above and ends cacheSolve function
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        
        # Inverse of matrix is NULL so calculate and set it
        data <- x$get()
        inv_x <- solve(data, ...)
        x$setinverse(inv_x)

	# Returning inv_x
        inv_x
}
