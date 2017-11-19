## Put comments here that give an overall description of what your
## functions do
This function will cache the inverse of a matrix, which could potentially be time consuming calculations.
It will hold this data for future use.  Furthermore, when in encounters a matrix without a cached value, it will calculate
the inverse and cache its data as well.

The cached matrixed will be held for future use to save time in calculations.
If a new matrix is encountered, the function will calculate its inverse of the matrix and cache the data for future use.

## Write a short comment describing this function
The function creates a matrix that will cache the inverse data of a matrix.

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL			#m initalizes as NULL, will hold value of inverse matrix
            set <- function(y) {        #Define the set function
                    x <<- y		#Value of matrix in parent environment will be held in variable x
                    m <<- NULL		#If there is no value for matrix, reset m to NULL
            }
            get <- function() x		#Define get function, returns value of the matrix
            setinverse <- function(inverse) m <<- inverse   #Set value of inverse in parent environment
            getinverse <- function() m			  #Gets the value of the inverse when called
            list(set = set, get = get,		#To refer to functions with $ operator
                 setinverse = setinverse,
                 getinverse = getinverse)
    }

}


## Write a short comment describing this function
This function provides the inverse calculation of a matrix. 
First it will check to see if the calculation is already cached.
If it is cached, it will return the value.
If not, it will take the matrix value, solve() for the inverse of the matrix.  Store the data in $setinverse.
And return the value of the inverse of the matrix. 

cacheSolve <- function(x, ...) {
            m <- x$getinverse()			   #Take getinverse value and record as variable m 
            if(!is.null(m)) {                      #If the value is already exsists in cache do the following
                    message("getting cached data") #Get the value
                    return(m)			   #Return the value
            }					   #If value was stored in m 
            data <- x$get()                        #Take value of matrix and store as variable data
            m <- solve(data, ...) 		   #Take the inverse of the variable data, and record as value "m" 		                  
            x$setinverse(m)			   #Record m as the value of the inverse in the parent environment, can be used in future.
            m					   #Return the value of m	
    }
}
