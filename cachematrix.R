## Put comments here that give an overall description of what your
## functions do

## These two functions are used to calculate the inverse of a given matrix. Storing a 
## matrix and the inverse of that given matrix, so that if the value of the inverse 
## of the same matrix is needed, it can be retrieved from cache rather than being calculated again.

## Write a short comment describing this function

## This function, named makeCacheMatrix is ued to create a list of function collection
## for setting/getting the given matrix and its inverse, correspondingly.

makeCacheMatrix <- function(x = matrix()) {
        matrix_inverse <- NULL
        #matrix_inverse is the cache used to store the inverse of a matrix
        set <- function(y = matrix()){
                x <<- y
        #this set function is used to change the matrix
        } 
        get <- function()  x #get the matrix x
        set_inverse <- function(new_inverse) matrix_inverse <<- new_inverse
        #set the inverse for the new matrix and cache it under the name matrix_inverse
        get_inverse <- function() matrix_inverse #get the inverse of the given matrix
        list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function

## This value is used to calculate/retrieve the inverse of a given matrix. The first if 
## conditional statement takes care of the situation that it's the first time that a matrix
## is given, and there is no old matrix to compare with. The second if conditional statement
## deals with the issue that when the inverse of a same matrix is required by calling cachesolve
## function, the value can just simply retrieved from cache. If a new matrix is given, we need 
## to calculate the inverse, and set the matrix to be an old one for comparing later on.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cache = x$get_inverse() #get the value of the inverse of a matrix
        if(is.null(cache)){
                matrix1 <<- x$get()
                matrix_to_cal <- x$get()
                cache <- solve(matrix_to_cal, ...)
                x$set_inverse(cache)
                return(cache)        
        }
        #compare the newly given matrix "x$get()" and the old matrix "matrix1" stored
        #in the if statement above
        if(identical(matrix1, x$get())){     
                message("getting cached data")
                return(cache)
        }
        matrix_to_cal <- x$get()
        matrix1 <<- x$get() #set the matrix to be an old one for comparing later on.
        cache <- solve(matrix_to_cal, ...)
        x$set_inverse(cache)
        return(cache)       
}
