makeCatchMatrix <- function(x = matrix()){
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             getinverse = getinverse,
             setinverse = setinverse)
        
}

## Writing a function to to return the inverse of the above matrix

catchSolve <- function(x, ...){
        m <- x$getinverse()
        if (!is.null(m)){
                message("getting cashed data")
                return(m)
                
        }
        data <- x$get()
        m <-solve(data, ...)
        x$setinverse(m)
        m
}