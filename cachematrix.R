## set()<-set matrix  
## get()<-get the matrix
## setInverse<-transmit the calculated inverse matrix
## getInverse<-get the inverse matrix

## return a list of functions which are related to the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function() x
        setInverse<-function(inverse) i<<-inverse 
        getInverse<-function() i
        list(get = get,set = set,getInverse = getInverse,setInverse = setInverse)
}


## judge whether the inversed matrix already exists,if no then generate it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i<-x$getInverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data<-x$get()
        i<-solve(data,...)
        x$setInverse(i)
        i
        
}
