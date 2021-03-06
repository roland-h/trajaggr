################################################################################
# Coercion from move objects to trajectories objects
#-------------------------------------------------------------------------------

#' @import methods sp spacetime trajectories move
NULL


#-------------------------------------------------------------------------------
# Coerce Move object to Track object
#-------------------------------------------------------------------------------

setAs("Move", "Track",
      function(from) {
        
        d <- slot(from, "data")

        Track(STIDF(geometry(from), move::timestamps(from), d))
        
      }
)



#-------------------------------------------------------------------------------
# Coerce MoveStack object to Tracks object
#-------------------------------------------------------------------------------
setAs("MoveStack", "Tracks", 
      function(from) {

        moveList <- move::split(from)
                
        trackList <- lapply(moveList, function(x) as(x, "Track"))
        
        idData_adj <- move::idData(from)
        
        tr <- trajectories::Tracks(trackList, tracksData = idData_adj)
                
      }
)



#-------------------------------------------------------------------------------
# Coerce MoveStack object to TracksCollection object
#-------------------------------------------------------------------------------
setAs("MoveStack", "TracksCollection", 
       function(from) {
         
         moveList <- move::split(from)
         
         tracksList <- lapply(moveList, function(x) {
           trajectories::Tracks(list(as(x, "Track")), tracksData = x@idData) })

         for (i in seq_along(tracksList)) {
           # Set names for Track objects
           names(tracksList[[i]]@tracks) <- paste("Track1_", names(tracksList)[i], sep="")
           # Adjust factor variables by dropping unused factor levels
           factors <- vapply(tracksList[[i]]@tracksData, is.factor, logical(1))
           tracksList[[i]]@tracksData[factors] <- lapply(
             tracksList[[i]]@tracksData[factors], factor)
         }
         
         idData_adj <- move::idData(from)
         
         tr <- trajectories::TracksCollection(tracksList, tracksCollectionData = idData_adj)
         
       }
)



#-------------------------------------------------------------------------------
# Coerce MoveBurst object to Tracks object
#-------------------------------------------------------------------------------

setAs("MoveBurst", "Tracks",
      function(from) {

        moveList <- move::split(from)
        
        trackList <- lapply(moveList, function(x) as(x, "Track"))
                
        idDataList <- lapply(moveList, function(x) move::idData(x))
        idData_adj <- do.call(rbind, idDataList)
                
        idData_adj$burstId <- as.factor(names(moveList))
                
        # Names of Bursts
        names(trackList) <- paste("Burst", 1:length(moveList), "_", names(moveList), sep="")
        
        attr(idData_adj, "row.names") <- names(trackList)
                
        Tracks(trackList, tracksData = idData_adj)
        
      }
)



#-------------------------------------------------------------------------------
# Coerce MoveBurst object to Track object
#-------------------------------------------------------------------------------

setAs("MoveBurst", "Track", function(from) {

  Track(STIDF(geometry(from), move::timestamps(from), data = slot(from, "data")),
        df = data.frame(burstId = slot(from, "burstId")))
  
})
