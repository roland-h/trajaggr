#' @import methods sp spacetime trajectories
NULL

#' @importFrom rgeos gIntersects
NULL

#' @include agg_allGenerics.R over_sp_IgnDom.R
NULL



#
# over.sp.Track
#

over_sp_Track <- function(x, y, returnList = FALSE, fn = NULL, ...,
                           use.data = FALSE, weight.points = NULL) {
  
  # Get point indices matching spatial geometries of x (no respect of time)
  cleanIndexList_sp <- overIgnDom(x, y, returnList = TRUE, fn = NULL, use.data = FALSE)
  
  # Determine which points intersecting spatial geoemtries
  
  # Get unique point indices of matching points
  uniqueSortedIndVec <- unique(sort(unlist(cleanIndexList_sp, use.names=F)))
  len <- length(uniqueSortedIndVec)
    
  # Create a list of complete vectors of indices for each spatial geometry
  # over all time instances (NA if not matching)
  completeIndexList_sp <- if (len > 0) {
    lapply(cleanIndexList_sp, function(z) {
      helpVec <- rep(NA, len)
      for (i in seq_along(z)) {
        w <- which(uniqueSortedIndVec == z[i])
        helpVec[w] <- z[i]
      }
      helpVec
    })
  } else { lapply(cleanIndexList_sp, function(z) { z <- NA; z }) }
    
  # Matrix with indices, one row for one spatial geometry,
  # columns representing machting points / time instances
  indByGeomMatrix <- do.call(rbind,completeIndexList_sp)
  
  cleanIndexVec <- as.vector(indByGeomMatrix)
  
  indexList <- as.list(cleanIndexVec)
  
  # Clean the indexList by setting NA to integer(0)
  cleanIndexList <- lapply(indexList, function(z) {
    if (is.na(z)) {
      z <- integer(0)
    } else {
      z
    }
  })  
  
  if (len == 0) {
    stopifnot(identical(cleanIndexList_sp, cleanIndexList))
  }
    
  if (use.data[1] == FALSE) {
    
    if (returnList) {
      cleanIndexList
    } else {
      cleanIndexVec
    }
    
  } else { # use.data[1] != FALSE
    
    if (returnList) {
      
      dfList <- lapply(cleanIndexList, function(z) {
        # It's (theoretically) possible to have a legal Track object without columns in the data slot
        if (length(y@data) > 0) {
          if (length(z) > 0) {
            
            df <- y@data[z, use.data, drop = FALSE]
            
          } else {
            
            df <- y@data[FALSE, use.data, drop = FALSE]
            
          }
        } else { # length(y@data) == 0
          
          df <- data.frame()
          df
        }
      })
      
      dfList
      
    } else { # retuenList == FALSE, use.data = T or selection of attributes
            
      if (is.null(fn)) {
        
        if (length(y@data) > 0) {
      
          df <- data.frame(y@data[cleanIndexVec, use.data, drop=FALSE], row.names = NULL)  
          
        } else {
          
          df <- data.frame(y@data[cleanIndexVec, FALSE, drop=FALSE], row.names = NULL)
          
        }
        
        # time added additionally 
        df$time <- index(y@time)[cleanIndexVec]
        df$timeIndex <- as.matrix(y@time)[cleanIndexVec]
        df
                
      } else { # !is.null(fn)
        
        fn = match.fun(fn)
        
        # warning in case of fn == sum
        if (paste(deparse(fn), collapse="") == paste(deparse(sum),collapse="")) {
          warning("Aggregation of trajectory data using 'sum' as aggregation function is not meaningful.")
        }
        
        ###
        # number of relocations
        nlocsList <- lapply(cleanIndexList, function(z) length(z))
        nlocs <- do.call(rbind, nlocsList)
        # If nlocs == 0, set to NA
        nlocs[nlocs == 0] <- NA
        
        # approximate duration
        halfConnDuration <- lapply(1:(length(y@time) - 1), function(z) {
          y@connections[z, "duration"] / 2 })
        durMatrix <- matrix(data = as.numeric(c(c(0, halfConnDuration), c(halfConnDuration, 0))),
                            nrow = length(y), ncol = 2)
        approxPointDuration <- .rowSums(durMatrix, length(y), 2, na.rm = TRUE)
        
        # Duration of each Point separated by cells / polygons
        approxPointsInCellsDurList <- lapply(cleanIndexList, function(z) {
          if (length(z) == 0) { NA } else { approxPointDuration[z] } })
        
        # metadata # duration per spatial unit
        approxDurationPerCell <- do.call(rbind, lapply(approxPointsInCellsDurList, sum))
        
        # approximate length
        halfConnDist <- lapply(1:(length(y@time) - 1), function(z) {
          round(y@connections[z, "distance"] / 2, 2) })
        distMatrix <- matrix(data = as.numeric(c(c(0, halfConnDist), c(halfConnDist, 0))),
                             nrow = length(y), ncol = 2)
        approxPointDist <- .rowSums(distMatrix, length(y), 2, na.rm = TRUE)
        
        # Duration of each Point separated by cells / polygons
        approxPointsInCellsDistList <- lapply(cleanIndexList, function(z) {
          if (length(z) == 0) { NA } else { approxPointDist[z] } })
        
        # metadata # duration per spatio-temporal geometry
        approxDistPerCell <- do.call(rbind, lapply(approxPointsInCellsDistList, sum))
        
        metadata <- data.frame(nlocs = nlocs, approx_duration = approxDurationPerCell,
                               approx_distance = approxDistPerCell)
      
        if (length(y@data) > 0) {
          
          if (!is.null(weight.points)) { # apply fn to data without weighting

            warning(paste("weight.points = ", weight.points, ", but no weighting is performed, because each spatio-temporal geometry just contains at most one Track point.", sep = ""))
            
          }
          
          df <- data.frame(y@data[cleanIndexVec, use.data, drop=FALSE], row.names = NULL)
          
          data.frame(df, metadata)
          
        } else { # length(y@data) == 0
          
          warning(paste("Method over was called with use.data = ", use.data, " and fn != NULL, but the data slot of the Track object does not contain any attributes. A data.frame just containing metadata is returned.", sep = ""))
          
          data.frame(y@data[seq_along(cleanIndexList), FALSE, drop = FALSE], metadata)
          
        }
      }
    }
  }  
}

#' @rdname over
#' @aliases over,SpatialPolygons,Track
setMethod("over", signature(x = "SpatialPolygons", y = "Track"), over_sp_Track)

#' @rdname over
#' @aliases over,SpatialPixels,Track
setMethod("over", signature(x = "SpatialPixels", y = "Track"), over_sp_Track)

#' @rdname over
#' @aliases over,SpatialGrid,Track
setMethod("over", signature(x = "SpatialGrid", y = "Track"), over_sp_Track)
