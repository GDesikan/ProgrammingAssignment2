## makeCacheMatrix function creates a special "martrix, which is really a martrix containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
      # sets inverse to NULL as a placeholder for a future value
      inverse_mat <- NULL 
      # defines a function to set the matrix x, to a new matrix y, and resets the inverse to NULL
      set <- function(y) { 
            x <<- y
            inverse_mat <<- NULL
      }
      # reuturns matrix x
      get <- function() x 
      # sets inverse of matrix to to inverse_mat
      setinverse <- function(solve) inverse_mat <<- solve 
      # returns inverse of matrix
      getinverse <- function() inverse_mat 
      # returns the matrix containing all the functions defined
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}

## cacheSolve function returns the inverse of the matrix if the inverse does not already exist. If the inverse exists in the cache,
## it takes the inverse from cache and skips the inverse computation

cacheSolve <- function(x, ...) { 
      # Return a matrix that is the inverse of 'x'
      inverse_mat <- x$getinverse()
      # checks if the inverse already exists
      if(!is.null(inverse_mat)) { 
            message("getting cached data")
            return(inverse_mat)
      }
      data <- x$get()
      # if not calculates the inverse and returns the inverse of the matrix
      inverse_mat <- solve(data, ...) 
      x$setinverse(inverse_mat)
      inverse_mat
}

## Command to execute the function:
# > a<-makeCacheMatrix()
# > a$set(matrix(1:4,2,2))

## Case 1:
# > cacheSolve(a)
## Output showed as inverse is: 
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

## Case2: When cacheSolve(a) is rerun, the following is the output:
# > cacheSolve(a)
# getting cached data
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5