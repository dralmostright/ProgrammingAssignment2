# Create a matrix object which gets and sets as martrix as well as inverse
makeCacheMatrix <- function(mat = matrix()) {
  
  # Intitialize the inverse as null matrix
  inv <-NULL
  
  # Set the input matrix
  set<-function(y){
		mat <<- y 	# Set the matrix to chche inverse
		inv <<- NULL	# Set the inverse to null
	}
	
  # Get the matrix whose inverse is to be cached
  get<-function() mat
  
  # Populate the inverse matrix to be cached.
  setmatrix<-function(solve) inv <<- solve
  
  # Get the inverse matrix
  getmatrix<-function() inv

  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}

cacheSolve <- function(mat=matrix(), ...) {
	# Retrive the inverse
    inv <- mat$getmatrix()
	
	# Check if inverse is cached if cached return inverse and end function
    if(!is.null(inv)){
      message("####### Get Inverse from Cache #######")
      return(inv)
    }
	
	# If inverse is not cached get the martix
    matrix <- mat$get()
	
	# Calculate the inverse
    inv <- solve(matrix, ...)
	
	# Cache the inverse
    mat$setmatrix(inv)
	
	# Dispaly Inverse
    inv
}

## Sample run of the function to test its functionaity
## Creat 3x3 matrix and sotre it
## > d = rbind(c(1, 3, 3), c(1, 4, 3), c(1, 3, 4))
## >

## Verify the matrix
## > d
##      [,1] [,2] [,3]
## [1,]    1    3    3
## [2,]    1    4    3
## [3,]    1    3    4

## Calculate the inverse of matrix
## > solve(d)
##      [,1] [,2] [,3]
## [1,]    7   -3   -3
## [2,]   -1    1    0
## [3,]   -1    0    1

## Initize the cache functoin
## > prod <- makeCacheMatrix()

## Set the input
## > prod$set(d)

## Verify the input
## > prod$get()
##      [,1] [,2] [,3]
## [1,]    1    3    3
## [2,]    1    4    3
## [3,]    1    3    4

## Calculate the inverse for first time using the cache
## > cacheSolve(prod)
##      [,1] [,2] [,3]
## [1,]    7   -3   -3
## [2,]   -1    1    0
## [3,]   -1    0    1

## Verify if the inverse is cached
## > cacheSolve(prod)
## ####### Get Inverse from Cache #######
##      [,1] [,2] [,3]
## [1,]    7   -3   -3
## [2,]   -1    1    0
## [3,]   -1    0    1
## >
