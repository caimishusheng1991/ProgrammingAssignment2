## These two functions can be used to calculate and cache the inverse of matrices
## so that one doesn't need to recalculate the inverse of matrices multiple times
## in the program.

## This function creats a special matrix. The original matrix and the inverse 
## can be both stored in the special matrix.
makeCacheMatrix <- function(x = numeric()) {
    # when initialization, set Inverse to NULL
    I <- NULL
    # when reset the matrix, set the Inverse to NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    # function used to get the matrix
    get <- function() x
    # function used to set Inverse matrix
    setI <- function(Inverse) I <<- Inverse
    # function used to get the saved Inverse Matrix
    getI <- function() I
    
    list(set = set, get = get,
         setI = setI,
         getI = getI)
}

## This function is used to calculate the inverse of the special matrix
## created by makeCacheMatrix, and cache it as I in the special matrix.
cacheSolve <- function(x, ...) {
    I <- x$getI()
    # if I is not NULL, I had been calculated, return I directly
    if(!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    
    data <- x$get()
    dimensions <- dim(data)
    # if the matrix is not n*n, return NULL
    if(dimensions[1] - dimensions[2] != 0){
        message("only an n*n matrix can have an inverse")
        return(NULL)
    }
    # use solve() function to calculate the inverse of the matrix
    tryCatch({
        I<- solve(data)
        x$setI(I)
        return(I) 
    }, error = function(e){
        message("the matrix does not have an inverse")
        return(NULL)
    })
}
