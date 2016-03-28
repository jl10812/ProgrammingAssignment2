#These two functions work together to cache a matrix, provide access to read and edit the cache,
#access the cache to solve for its inverse and cache the inverse as well.


#makeCacheMatrix caches the original matrix, creating a cache for its inverse as well. 
#creates the cache and provides access to read and edit this cache. 

#'x' being the uninverted matrix to be cached
makeCacheMatrix <- function(x = matrix()) {

	#when the cache is initialized, no inverted matrix has been solved for yet	
        inverted_mat <- NULL

	#caches a different uninverted matrix
        set <- function(y) {
                x <<- y
                inverted_mat <<- NULL
        }

	#allows the uninverted cached matrix to be accessed
        get <- function() x
	
	#caches the inverted matrix
        setinverse <- function(inverse) inverted_mat <<- inverse

	#allows the inverted matrix in the cache to be accessed
        getinverse <- function() inverted_mat

	#whole list of functions returned that allow the cache to be set and accessed
	#objects of this returned list allow the cache to be read and edited
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#cacheSolve reads the cached matrix from the cache created by makeCacheMatrix
#solves for the inverse of the cached if the inverse is not already cached
#caches the inverse matrix

#'x' being the list returned from makeCacheMatrix
cacheSolve <- function(x, ...) {

	#go into the list, see what the cache says the inverse is
        inverted_mat <- x$getinverse()

	#inverted matrix is already in the cache
        if(!is.null(inverted_mat)) {
        	message("getting cached inverted matrix")
        	return(inverted_mat)
        }

	#obtain original matrix from the cache
        un_inv_mat <- x$get()

	#solve for inverse of the original matrix 
        inverted_mat <- solve(un_inv_mat, ...)

	#put inverted matrix into the cache
        x$setinverse(inverted_mat)

        inverted_mat
}