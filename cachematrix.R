## Put comments here that give an overall description of what your
## functions do

## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly.

## The following is pair of functions that that are used to create a 
## special object that stores a matrix and cache's its inverse

## Two Test Cases are also provided after the second funcion (cacheSolve)

## Write a short comment describing this function

## makeCacheMatrix is function that outputs a list of functions.
## Its purpose is to store a martix and cache the value of the inverse.
## The list contains the following functions:
## - set_matrix      set the value of a matrix
## - get_matrix      get the value of a matrix
## - cache_inverse   get the cached value of the inverse of the matrix
## - get_inverse     get the cached value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        set_matrix <- function(y) {
                x <<- y
                cache <<- NULL
        }
        get_matrix <- function() {
                x
        }
        cache_inverse <- function(solve) {
                cache <<- solve
        }
        get_inverse <- function() {
                cache
        }
        
        list(set_matrix = set_matrix, get_matrix = get_matrix,
             cache_inverse = cache_inverse,
             get_inverse = get_inverse)
}


## Write a short comment describing this function

## The following function calculates the inverse of the matrix created 
## with the above function. However, it first checks to see if the inverse 
## has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the 
## data and sets the value of the inverse in the cache via the cache_inverse 
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$get_inverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get_matrix()
        cache <- solve(data, ...)
        x$cache_inverse(cache)
        cache
}

## The following are two test cases for testing the above two functions

## TEST CASE 1

## > test_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## > test_matrix$get_matrix()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > test_matrix$get_inverse()
## NULL
## > cacheSolve(test_matrix)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(test_matrix)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > test_matrix$get_inverse()
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## TEST CASE 2

## > test_matrix$set_matrix(matrix(c(1, 1, 2, 3), 2, 2))
## > test_matrix$get_matrix()
## [,1] [,2]
## [1,]    1    2
## [2,]    1    3
## > test_matrix$get_inverse()
## NULL
## > cacheSolve(test_matrix)
## [,1] [,2]
## [1,]    3   -2
## [2,]   -1    1
## > cacheSolve(test_matrix)
## getting cached data
## [,1] [,2]
## [1,]    3   -2
## [2,]   -1    1
## > test_matrix$get_inverse()
## [,1] [,2]
## [1,]    3   -2
## [2,]   -1    1