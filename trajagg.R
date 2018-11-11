### R code from vignette source '/home/harry/trajaggr_furtherDevel/trajaggr_20150730_integrate2traj_r-h-GitHub/trajaggr/vignettes/trajagg.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: load_TestData (eval = FALSE)
###################################################
## load(system.file("extdata","trajaggr_TestData.RData", 
##                  package = "trajaggr"), verbose = TRUE)


###################################################
### code chunk number 2: trajagg.Rnw:553-554
###################################################
options(width=60)


###################################################
### code chunk number 3: exampledata_vulture_movestack_inspect1
###################################################
library(trajaggr)
data(vulture_moveStack)
class(vulture_moveStack)[1]
levels(vulture_moveStack@trackId)
length(vulture_moveStack)
names(vulture_moveStack@data)


###################################################
### code chunk number 4: exampledata_pigeon_movestack_inspect
###################################################
# library(trajaggr)
data(pigeon_R_moveStack)
class(pigeon_R_moveStack)[1]
levels(pigeon_R_moveStack@trackId)
length(pigeon_R_moveStack)
names(pigeon_R_moveStack@data)


###################################################
### code chunk number 5: exampledata_wildboars_ltraj_inspect
###################################################
# library(trajaggr)
data(wildboars_4Ind_ltraj)
class(wildboars_4Ind_ltraj)
# library(adehabitatLT)
adehabitatLT::summary.ltraj(wildboars_4Ind_ltraj)


###################################################
### code chunk number 6: coercion_move_part_1
###################################################
### Coercion of MoveStack to Tracks
# library(trajaggr)
class(vulture_moveStack)[1]
v_Tracks <- as(vulture_moveStack, "Tracks")
class(v_Tracks)[1]

## Compare as-method and generic method
v_Tracks_gen <- as.Tracks(vulture_moveStack)
identical(v_Tracks, v_Tracks_gen)

### Coercion of MoveStack to TracksCollection
v_TrColl <- as.TracksCollection(vulture_moveStack)
class(v_TrColl)[1]

### Coercion of MoveBurst to Tracks
# Create a MoveBurst object from the first individuals' first day track
# First subset the Move object to the first tracked day
v_X1_Move <- vulture_moveStack[[1]]
day1 <- which(as.Date(v_X1_Move@timestamps) == 
                as.Date(v_X1_Move@timestamps[1]))
v_X1_1_Move <- v_X1_Move[day1]
length(v_X1_1_Move) # number of locations

# Create MoveBurst object with bursts specifying the type of 
# locomotion (on.ground or flying) based on the vulture's speed
behav <- rep("on.ground", length(day1))
behav[which(v_X1_Move@data$ground_speed[day1] > 5)] <- "flying"
v_X1_1_mb <- move::burst(v_X1_1_Move, f = behav[1:length(behav) - 1])
length(v_X1_1_mb@burstId) # number of bursts corresponding to segments

# Coerce MoveBurst to Tracks ...
v_X1_1_mbTracks <- as.Tracks(v_X1_1_mb)
v_X1_1_mbTracks@tracksData[c("n", "tmin", "tmax", "medspeed")]


###################################################
### code chunk number 7: coercion_tomove_part_1
###################################################
### Coercion of Tracks to MoveStack
# library(trajaggr)
class(v_Tracks)[1]
library(move)
v_moveSt <- as.MoveStack(v_Tracks)
class(v_moveSt)[1]

### Coercion of TracksCollection to MoveStack
class(v_TrColl)[1]
v_moveSt <- as.MoveStack(v_TrColl)
class(v_moveSt)[1]

### Coerce Tracks to MoveBurst
class(v_X1_1_mbTracks)[1]
v_X1_1_newMB <- as.MoveBurst(v_X1_1_mbTracks)
class(v_X1_1_newMB)[1]


###################################################
### code chunk number 8: coercion_ltraj_part_1
###################################################
# library(trajaggr)
class(wildboars_4Ind_ltraj[1])

# Coerce ltraj track of first individual (first burst) to Track object
wb_1_Track <- as(wildboars_4Ind_ltraj[1], "Track")
class(wb_1_Track)[1]

# Coercion of third and fourth burst which belong to the same individual
wb_Tracks <- as(wildboars_4Ind_ltraj[3:4], "Tracks")
class(wb_Tracks)[1]
dim(wb_Tracks)

# Coercion of whole ltraj object (4 ind., 5 bursts) to TracksCollection
wb_TracksColl <- as(wildboars_4Ind_ltraj, "TracksCollection")
wb_TracksColl@tracksCollectionData


###################################################
### code chunk number 9: coercion_ltraj_part_2 (eval = FALSE)
###################################################
## # Coercion of one burst to Tracks or TracksCollection is inadequate
## wb_1_Tracks <- as(wildboars_4Ind_ltraj[1], "Tracks")


###################################################
### code chunk number 10: coercion_ltraj_part_3
###################################################
cat(try(wb_1_Tracks <- as(wildboars_4Ind_ltraj[1], "Tracks")))


###################################################
### code chunk number 11: coercion_ltraj_part_2 (eval = FALSE)
###################################################
## # Coercion of bursts of several individuals to Tracks is inadequate
## wb_Tracks <- as(wildboars_4Ind_ltraj, "Tracks")


###################################################
### code chunk number 12: coercion_ltraj_part_3
###################################################
cat(try(wb_Tracks <- as(wildboars_4Ind_ltraj, "Tracks")))


###################################################
### code chunk number 13: coercion_toLtraj_part_1
###################################################
# Coercion of Track to ltraj
# library(trajaggr)
wb_1_ltraj <- as(wb_1_Track, "ltraj")
class(wb_1_ltraj)

# Coercion of Tracks to ltraj
wb_3and4_ltraj <- as(wb_Tracks, "ltraj")
class(wb_3and4_ltraj)

# Coercion of TracksCollection to ltraj
wb_ltraj <- as(wb_TracksColl, "ltraj")
class(wb_ltraj)


###################################################
### code chunk number 14: trajagg.Rnw:1003-1004
###################################################
options(width=60)


###################################################
### code chunk number 15: over_overview
###################################################
library(trajectories)
library(trajaggr)
showMethods(over, classes = c("Track", "Tracks", "TracksCollection"))


###################################################
### code chunk number 16: load_TestData
###################################################
load(system.file("extdata","trajaggr_TestData.RData", 
                 package = "trajaggr"), verbose = FALSE)


###################################################
### code chunk number 17: inspect_TestData_1
###################################################
class(Track_A1)[1]
dim(as(Track_A1, "STIDF"))
library(spacetime)
index(Track_A1@time) # Track point timestamps
class(stf_Polys_4t)[1]
dim(stf_Polys_4t)
class(stf_Polys_4t@sp)[1] # class of object in the sp slot
index(stf_Polys_4t@time) # STF timestamps
stf_Polys_4t@endTime # STF end times


###################################################
### code chunk number 18: over_stf_inspect_byFig
###################################################
TA1 <- Track_A1 # sic
TA1@data$Point_Nr <- as.factor(seq(1:length(TA1)))

mycol <- gray((8:0/8)[2:7])

spPolygons <- list("sp.polygons", stf_Polys_4t@sp, col = mycol[length(mycol)], cex=1.5) 
spLines <- list("sp.lines", as(TA1, "SpatialLines"), cex=1.5) 
#library(sp)
sp::spplot(as(TA1, "SpatialPointsDataFrame"), zcol="Point_Nr", key.space = "right", 
       cex = 1.7, scales = list(draw = TRUE, tck = c(1,0), cex = 0.9), col.regions=mycol, 
       xlim = c(-0.5,10.5), ylim = c(-0.5,10.5), edge.col = "black",
       sp.layout = list(spPolygons, spLines),
       legendEntries = c("Track point 1", "Track point 2", "Track point 3", "Track point 4",
                         "Track point 5", "Track point 6"))


###################################################
### code chunk number 19: over_stf_inspect_over1
###################################################
over(x = stf_Polys_4t, y = Track_A1, returnList = FALSE)


###################################################
### code chunk number 20: over_stf_inspect_over2
###################################################
over_indexL <- over(x = stf_Polys_4t, y = Track_A1, returnList = TRUE)
identical(length(stf_Polys_4t), length(over_indexL))
over_indexL[1:3]


###################################################
### code chunk number 21: over_stf_inspect_over3
###################################################
over_data <- over(x = stf_Polys_4t, y = Track_A1, returnList = TRUE,
                 use.data = TRUE)
over_data[1:3]


###################################################
### code chunk number 22: over_stf_inspect_4
###################################################
over(x = stf_Polys_4t, y= Track_A1, returnList = FALSE,
     fn = mean, use.data = c(1))


###################################################
### code chunk number 23: over_stf_inspect_5
###################################################
over(x = stf_Polys_4t[ , 1:2], y = Track_A1, returnList = FALSE, 
     fn = weighted.mean, use.data = "co2", weight.points = "byTime")


###################################################
### code chunk number 24: over_stf_inspect_weightmean_ex
###################################################
(8 * 12 +   # contribution of the third Track point value (12), weight is 8
 4 * 4) /   # contribution of the fourth Track point value (4), weight is 4
 12         # sum of weights


###################################################
### code chunk number 25: over_sp_inspect_over1
###################################################
### over, in which all Track points intersect the Spatial object

# returnList = FALSE:
over_sp <- over(x = stf_Polys_4t@sp, y = Track_A1, returnList = FALSE)
over_sp

# Checking object length:
identical(length(over_sp), length(stf_Polys_4t@sp) * length(Track_A1))

# returnList = TRUE:
over_sp_list <- over(x = stf_Polys_4t@sp, y = Track_A1, returnList = TRUE)

# Checking if data is identical:
bool_indices_1 <- !is.na(over_sp)
bool_indices_2 <- sapply(over_sp_list, function(x) length(x) > 0)
identical(over_sp[bool_indices_1], unlist(over_sp_list[bool_indices_2]))


###################################################
### code chunk number 26: over_sp_inspect_over2
###################################################
### over, whereas Track points partly intersect the Spatial object

# returnList = FALSE:
over_sp_partlyIntersec <- over(x = stf_Polys_4t@sp[1:2], y = Track_A1, 
                               returnList = FALSE)
over_sp_partlyIntersec

# Checking object length:
intersecPoints <- over_sp_partlyIntersec[(!is.na(over_sp_partlyIntersec))]
identical(length(over_sp_partlyIntersec), 
          length(stf_Polys_4t@sp[1:2]) * length(as(Track_A1, "STIDF")[intersecPoints]))

# returnList = TRUE, with SpatialPixels:
class(spPix)[1]
over_spPix_partlyIntersec_list <- over(x = spPix[1:2], y = Track_A1, 
                                       returnList = TRUE)

# Checking if data is identical:
bool_indices_partly_1 <- !is.na(over_sp_partlyIntersec)
bool_indices_partly_2 <- sapply(over_spPix_partlyIntersec_list, 
                         function(x) length(x) > 0)
identical(over_sp_partlyIntersec[bool_indices_partly_1], 
          unlist(over_spPix_partlyIntersec_list[bool_indices_partly_2]))


###################################################
### code chunk number 27: over_sp_inspect_over3
###################################################
# Inspecting the SpatialGrid object
class(spGrid_ul)[1]
spGrid_ul@grid

# over with fn == NULL (default)
over(x = spGrid_ul, y = Track_A1, returnList = FALSE, use.data = TRUE)

# over with fn != NULL
over(x = spGrid_ul, y = Track_A1, returnList = FALSE, use.data = TRUE,
     fn = mean)


###################################################
### code chunk number 28: over_sp_inspect_over2xts
###################################################
over_sp_1cell <- over(x = spGrid_ul, y = Track_A1, returnList = FALSE, 
                      use.data = TRUE)

# Creating the xts object, package xts is required
# library(xts)
xts_obj <- xts::xts(x = over_sp_1cell["co2"], order.by = over_sp_1cell$time,
                    tzone = attr(Track_A1@time, "tzone"))
class(xts_obj)[1]
xts_obj


###################################################
### code chunk number 29: over_sp_noTime_1
###################################################
# Creating STF object with one time interval
stf <- STF(stf_Polys_4t@sp, time = Track_A1@time[1], 
           endTime = index(Track_A1@time[length(Track_A1@time)]) + 1)
dim(stf)

# over returning list of indices
over(x = stf, y = Track_A1, returnList = TRUE)

# over returning (weighted aggregated) data
over(x = stf, y = Track_A1, returnList = FALSE, fn = weighted.mean, 
     use.data = TRUE, weight.points = "byTime")


###################################################
### code chunk number 30: over_sp_noTime_2
###################################################
# over returning a list of indices (again) 
indexL <- over(x = stf, y = Track_A1, returnList = TRUE, use.data = FALSE)
# Creating a list of xts objects
# library(xts)
xts_list <- lapply(seq_along(indexL), function(z) {
  xts::xts(x = Track_A1@data[indexL[[z]], , drop = FALSE], 
           order.by = index(Track_A1@time[indexL[[z]]]),
           tzone = attr(Track_A1@time, "tzone")) })
# Inspecting the returned object
class(xts_list[[1]])
xts_list[[1]]


###################################################
### code chunk number 31: count_vulture_movestack_cverview
###################################################
showMethods(count, classes = c("Track", "Tracks", "TracksCollection"))


###################################################
### code chunk number 32: count_vulture_movestack_prep_0.0
###################################################
data(vulture_moveStack)
vulture_TrC <- as(vulture_moveStack, "TracksCollection")


###################################################
### code chunk number 33: count_vulture_movestack_prep_1
###################################################
# Create a list of adequate MoveStack objects
#library(move)
vulture_moveObjList <- move::split(vulture_moveStack)
vulture_moveStackList <- lapply(vulture_moveObjList, function(x) {
  dates <- as.Date(x@timestamps)
  uniquedates <- unique(dates) 
  moveObjList <- lapply(seq_along(uniquedates), function(y) {
    w <- which(dates == uniquedates[y])
    x[w]
  })
  ms <- move::moveStack(moveObjList)
  # However the timezone in the timestamps slot is dropped
  # when applying move::moveStack. Need to redefine the timezone...
  attr(ms@timestamps, "tzone") <- attr(ms@data$timestamp, "tzone")
  return(ms)
})

# Coerce MoveStack objects to Tracks objects
Tracks_X1 <- as(vulture_moveStackList[[1]], "Tracks")
Tracks_X2 <- as(vulture_moveStackList[[2]], "Tracks")
Tracks_X3 <- as(vulture_moveStackList[[3]], "Tracks")

# Create TracksCollection
vulture_TrC <- TracksCollection(list(Tracks_X1 = Tracks_X1,
                                     Tracks_X2 = Tracks_X2,
                                     Tracks_X3 = Tracks_X3))


###################################################
### code chunk number 34: count_sp_vulture_TracksX1_count_1
###################################################
spG_X1_dim25 <- createSpatialArealObjFromPoints(
  as(Tracks_X1, "SpatialPointsDataFrame"), 
  desDim = 25, out = "SpatialGrid")
class(spG_X1_dim25)[1]
count_Tracks_X1 <- count(Tracks_X1, spG_X1_dim25)
class(count_Tracks_X1)[1]
str(count_Tracks_X1@data)


###################################################
### code chunk number 35: count_sp_vulture_TracksX1_fig_1
###################################################
mx <- max(count_Tracks_X1@data$ntraj, na.rm = TRUE)
mycol <- gray(((mx + 2):0/(mx + 2))[2:(mx+1)])
labelat = 1:mx
labeltext = as.character(1:mx)
mykey <- list(labels=list(at = labelat, labels = labeltext, cex = 1.1), space="top")
spLines <- list("sp.lines", as(Tracks_X1, "SpatialLines"))

sp::spplot(count_Tracks_X1, colorkey = mykey, cuts = mx - 1, col.regions = mycol,
       scales = list(draw = TRUE, tck = c(1,0), cex = 1), sp.layout = list(spLines))


###################################################
### code chunk number 36: count_sp_vulture_TrCbyID_count_1
###################################################
spG_X1X2_dim15 <- createSpatialArealObjFromPoints(
  as(vulture_TrC[1:2], "SpatialPointsDataFrame"), 
  desDim = 15, out = "SpatialGrid")
count_vultureTrC_byID <- count(vulture_TrC, spG_X1X2_dim15, byID = TRUE)
class(count_vultureTrC_byID)[1]
str(count_vultureTrC_byID@data)


###################################################
### code chunk number 37: count_sp_vulture_TrC_fig_1
###################################################
mx <- max(count_vultureTrC_byID@data, na.rm = TRUE)
mycol <- gray(((mx + 2):0/(mx + 2))[2:(mx+1)])
labelat = 1:mx
labeltext = as.character(1:mx)
mykey <- list(labels=list(at = labelat, labels = labeltext, cex = 1.1))

sp::spplot(count_vultureTrC_byID[1:2], cuts = mx - 1, col.regions = mycol, cex = 1, 
       scales = list(draw = TRUE, tck = c(1,0), cex = 1), colorkey = mykey,
       names.attr = c("ntraj_Tracks_X1 (Gyps africanus)", 
                      "ntraj_Tracks_X2 (Torgos tracheliotus)"))


###################################################
### code chunk number 38: count_stf_vulture_TracksX_count_1
###################################################
spPix_X3_dim15 <- createSpatialArealObjFromPoints(
  as(Tracks_X3, "SpatialPointsDataFrame"), 
  desDim = 15, out = "SpatialPixels")
stf_spPx_X3_4t <- STF(spPix_X3_dim15,
                      time = Tracks_X3[c(1,4,7,10)]@tracksData$tmin,
                      endTime = Tracks_X3[c(3,6,9,12)]@tracksData$tmax)
count_Tracks_X3_stf <- count(Tracks_X3, stf_spPx_X3_4t)


###################################################
### code chunk number 39: count_stf_vulture_X3_fig_1
###################################################
obj <- count_Tracks_X3_stf

mx <- max(obj@data$ntraj, na.rm = TRUE)
mycol <- gray(((mx + 2):0/(mx + 2))[2:(mx+1)])
labelat = 1:mx
labeltext = as.character(1:mx)
mykey <- list(labels=list(at = labelat, labels = labeltext, cex = 1.1), 
              space="right", height = 0.3)

# stplot --> cuts different from spplot!
spacetime::stplot(obj, main = NULL, colorkey = mykey, cuts = mx, col.regions = mycol,
       scales = list(draw = TRUE, tck = c(1,1), cex = 1, rot = 45),
       names.attr = c("2008-05-02", "2008-05-05", "2008-05-08", "2008-05-11"))


###################################################
### code chunk number 40: agg_overview
###################################################
showMethods(aggregate, classes = c("Track", "Tracks", "TracksCollection"))


###################################################
### code chunk number 41: agg_stf_vulture_TracksX1_agg_1
###################################################
# Create SpatialPixels covering the extent of the Tracks_X1 object
spPix_X1_dim15 <- createSpatialArealObjFromPoints(
  as(Tracks_X1, "SpatialPointsDataFrame"), 
  desDim = 15, out = "SpatialPixels")

# Create a STF object with four time intervals
stf_px_X1_4t <- STF(spPix_X1_dim15,
                    time = Tracks_X1[c(1,4,7,10)]@tracksData$tmin,
                    endTime = Tracks_X1[c(3,6,9,12)]@tracksData$tmax)

# Aggregate the attribute 'height_raw' using function 'min'
agg_X1_stf_minHght <- aggregate(x = Tracks_X1, by = stf_px_X1_4t, FUN = min,
                                na.rm = TRUE, use.data = "height_raw")

# Class of returned object
class(agg_X1_stf_minHght)[1]

# Some example rows from the data slot of the resulting STFDF object
agg_X1_stf_minHght@data[474:477, ]


###################################################
### code chunk number 42: agg_stf_vulture_Tracks_X1_fig_1
###################################################

obj <- agg_X1_stf_minHght

mx <- 5 # nr of intervals ... equals number of thisat - 1
#mycol_w2b <- gray(((mx + 2):0/(mx + 2))[2:(mx+1)])
mycol_b2w <- gray((0:(mx + 2)/(mx + 2))[2:(mx+1)])

#maxdata <- max(obj@data$height_raw, na.rm=T)
#maxScaleValue <- ceiling(maxdata/100) * 100

#thisat = c(0, 50, 100, 200, 400, 800, maxScaleValue)
thisat = c(0, 50, 100, 250, 500, 1000)

labelat = c(0, 100, 250, 500, 1000)#1:mx

# spplot or stplot
spacetime::stplot(obj[ , , 1], # select sp, time, attributes
       col.regions = mycol_b2w, # key bar color
       colorkey = list(labels=list(at = labelat, cex = 1.1), space="bottom",
                       height = 1.0), # thisat labels
       at = thisat, # thisat cut values
       scales = list(draw = TRUE, tck = c(1,0), cex = 1, rot = 45) # x and y scales
       #names.attr = c("2008-05-02", "2008-05-05", "2008-05-08", "2008-05-11"), # panel names
       #, main = "Minimal altitude of a vulture individual" # plot title
       , main = NULL
       )


###################################################
### code chunk number 43: agg_stf_vulture_TrC_agg_2
###################################################
# Identify the index of the attribute ground_speed
w_spd <- which(names(vulture_TrC[2][2]@data) == "ground_speed")

# To illustrate the creation of missing attributes the attribute
# ground_speed is deleted from the data of the second vulture
for (i in 2:length(vulture_TrC@tracksCollection[[2]]@tracks)) {
  vulture_TrC@tracksCollection[[2]]@tracks[[i]]@data <- 
    vulture_TrC@tracksCollection[[2]]@tracks[[i]]@data[ , -w_spd]
}

# Attribute still available in e.g. second Track...?
"ground_speed" %in% names(vulture_TrC[2][2]@data)

# Weighted aggregation of the TracksCollection separated by individuals
agg_vTrC_wMeanSpd <- aggregate(x = vulture_TrC, by = stf_px_X1_4t,
                               FUN = weighted.mean, na.rm = TRUE, 
                               use.data = "ground_speed",
                               weight.points = "byTime", 
                               weight.tracks = "byTime", byID = TRUE)

# Overview of individual ground_speed
summary(agg_vTrC_wMeanSpd@data[["Tracks_X1.ground_speed"]])
summary(agg_vTrC_wMeanSpd@data[["Tracks_X2.ground_speed"]])
summary(agg_vTrC_wMeanSpd@data[["Tracks_X3.ground_speed"]])


###################################################
### code chunk number 44: agg_stf_vulture_TrC_fig_2
###################################################

obj <- agg_vTrC_wMeanSpd

#par_sic <- par()
#mfcol=c(nrows, ncols) 
#par("mfcol")

### 1. Plot

#mx_Speed <- max(obj@data$ground_speed, na.rm = TRUE)
#mx <- ceiling(thismx/100)

attrNames <- names(agg_vTrC_wMeanSpd@data)
w <- which(attrNames == "Tracks_X1.ground_speed")
#           | attrNames == "Tracks_X2.ground_speed"
#           | attrNames == "Tracks_X3.ground_speed")

mx <- 5 # nr of intervals ... equals number of thisat - 1
#mycol_w2b <- gray(((mx + 2):0/(mx + 2))[2:(mx+1)])
mycol_b2w <- gray((0:(mx + 2)/(mx + 2))[2:(mx+1)])

#maxdata <- max(obj@data$height_raw, na.rm=T)
#maxScaleValue <- ceiling(maxdata/100) * 100

#thisat = c(0, 50, 100, 200, 400, 800, maxScaleValue)
thisat = c(0, 5, 10, 15, 20, 25)

#labelat = c(0, 100, 250, 500, 1000)#1:mx
labelat = c(0, 5, 10, 15, 20, 25)

# spplot or stplot
spacetime::stplot(obj[ , , w[1], drop = FALSE], # select sp, time, attributes
       col.regions = mycol_b2w, # key bar color
       colorkey = list(labels=list(at = labelat, cex = 1.1), space="bottom",
                       height = 1.0), # thisat labels
       at = thisat, # thisat cut values
       scales = list(draw = TRUE, tck = c(1,0), cex = 1, rot = 45) # x and y scales
       #names.attr = c("2008-05-02", "2008-05-05", "2008-05-08", "2008-05-11"), # panel names
       #,main = "Weighted average speed of a vulture individual"  #"height_raw median" # plot title
       , main = NULL
       )

# # !!!!!!
# ### 2. Plot nlocs
# 
# w_attr <- which(names(agg_vTrC_wMeanSpd@data) == "Tracks_X1.approx_distance")
# #w_attr <- which(names(agg_vTrC_wMeanSpd@data) == "Tracks_X1.nlocs")
# 
# #mx_Speed <- max(obj@data$ground_speed, na.rm = TRUE)
# 
# mx <- 6 # nr of intervals ... equals number of thisat - 1
# #mycol_w2b <- gray(((mx + 2):0/(mx + 2))[2:(mx+1)])
# mycol_b2w <- gray((0:(mx + 2)/(mx + 2))[2:(mx+1)])
# 
# #maxdata <- max(obj@data$height_raw, na.rm=T)
# #maxScaleValue <- ceiling(maxdata/100) * 100
# 
# thisat = c(0, 1000, 10000, 20000, 30000, 50000, 200000)
# #thisat = c(0, 1, 10, 20, 50, 100, 300, 600)
# 
# labelat = c(0, 1000, 10000, 20000, 30000, 50000, 200000)
# #labelat = c(1, 10, 20, 50, 100, 300, 600)
# 
# 
# # spplot or stplot
# stplot(obj[ , , w_attr, drop = FALSE], # select sp, time, attributes
#        col.regions = mycol_b2w, # key bar color
#        colorkey = list(labels=list(at = labelat, cex = 1.1), space="left",
#                        height = 1.0), # thisat labels
#        at = thisat, # thisat cut values
#        scales = list(draw = TRUE, tck = c(1,0), cex = 1, rot = 45), # x and y scales
#        #names.attr = c("2008-05-02", "2008-05-05", "2008-05-08", "2008-05-11"), # panel names
#        main = NULL  #"height_raw median" # plot title
#        )
# 
# #par("mfrow")
# par() <- par_sic
# par("mfcol")


###################################################
### code chunk number 45: agg_sp_pigeon_preparation_1
###################################################
# Load the small subsets of the example data sets
data(pigeon_R_moveSt_sub, pigeon_S_moveSt_sub)

# Validate the class
is(pigeon_R_moveSt_sub, "MoveStack") || is(pigeon_S_moveSt_sub, "MoveStack")

# Coerce MoveStacks to Tracks objects and validate the returned object
pigeon_R_Tr <- as.Tracks(pigeon_R_moveSt_sub)
pigeon_S_Tr <- as.Tracks(pigeon_S_moveSt_sub)
is(pigeon_R_Tr, "Tracks") || is(pigeon_S_Tr, "Tracks")

# Inspect the high frequent and synchronious sampling rate
options(digits.secs=2)
index(pigeon_R_Tr[1]@time[1:5])
options(digits.secs=0)
length(pigeon_R_Tr[1]@time)
identical(index(pigeon_R_Tr[1]@time), index(pigeon_S_Tr[1]@time))

# Create a TracksCollection with one Track object od each individual
pigeons_TrColl <- TracksCollection(
  list(Tracks(list(pigeon_R_Tr[1])), Tracks(list(pigeon_S_Tr[1]))))

# Inspect the tracksCollectionData
pigeons_TrColl@tracksCollectionData[ , -c(6, 7)]


###################################################
### code chunk number 46: agg_sp_pigeon_TrColl_1
###################################################
# Create SpatialPixels covering the extent of the TracksCollection
spPix_pTrC_dim4 <- createSpatialArealObjFromPoints(
  as(pigeons_TrColl, "SpatialPointsDataFrame"), 
  desDim = 4, out = "SpatialPixels")

# Dimensions of the SpatialPixels
spPix_pTrC_dim4@grid@cells.dim

# Aggregate the attribute 'ground_speed' using function 'weighted.mean'
agg_pTrC_sp_wMeanSpd <- aggregate(x = pigeons_TrColl, by = spPix_pTrC_dim4,
                                  FUN = weighted.mean, na.rm = TRUE,
                                  use.data = "ground_speed",
                                  weight.points = "equal", 
                                  weight.tracks = "byTime", byID = FALSE)

# Class of returned object
class(agg_pTrC_sp_wMeanSpd)[1]

# Dimensions of the returned STFDF object
dim(agg_pTrC_sp_wMeanSpd)

# Summary and example rows of the data slot of the resulting STFDF object
summary(agg_pTrC_sp_wMeanSpd@data[ , -c(3, 4)])
agg_pTrC_sp_wMeanSpd@data[c(209:212, 217:219), -4]


###################################################
### code chunk number 47: agg_sp_pigeon_TrColl_fig_1
###################################################
# obj <- agg_vTrC_wMeanSpd
# #par_sic <- par()
# #mfcol=c(nrows, ncols) 
# #par("mfcol")
# #mx_Speed <- max(obj@data$ground_speed, na.rm = TRUE)
# #mx <- ceiling(thismx/100)
# attrNames <- names(agg_vTrC_wMeanSpd@data)
# w <- which(attrNames == "Tracks_X1.ground_speed")
# #           | attrNames == "Tracks_X2.ground_speed"
# #           | attrNames == "Tracks_X3.ground_speed")
# mx <- 5 # nr of intervals ... equals number of thisat - 1
# #mycol_w2b <- gray(((mx + 2):0/(mx + 2))[2:(mx+1)])
# mycol_b2w <- gray((0:(mx + 2)/(mx + 2))[2:(mx+1)])
# #maxdata <- max(obj@data$height_raw, na.rm=T)
# #maxScaleValue <- ceiling(maxdata/100) * 100
# #thisat = c(0, 50, 100, 200, 400, 800, maxScaleValue)
# thisat = c(0, 5, 10, 15, 20, 25)
# #labelat = c(0, 100, 250, 500, 1000)#1:mx
# labelat = c(0, 5, 10, 15, 20, 25)
# # spplot or stplot
# stplot(obj[ , , w[1], drop = FALSE], # select sp, time, attributes
#        col.regions = mycol_b2w, # key bar color
#        colorkey = list(labels=list(at = labelat, cex = 1.1), space="bottom",
#                        height = 1.0), # thisat labels
#        at = thisat, # thisat cut values
#        scales = list(draw = TRUE, tck = c(1,0), cex = 1, rot = 45), # x and y scales
#        #names.attr = c("2008-05-02", "2008-05-05", "2008-05-08", "2008-05-11"), # panel names
#        main = "Weighted average speed of a vulture individual"  #"height_raw median" # plot title
#        )
spacetime::stplot(agg_pTrC_sp_wMeanSpd[ , , 1], mode = "tp",
       ylab = "speed" #cex = 2,
       #scales = list(cex = 2),
       #,main = "Time series of weighted average speed of two pigeons"
       , main = NULL
       )


###################################################
### code chunk number 48: agg_stf_pureSpatial_prep_1
###################################################
# Load data
data(pigeon_R_moveStack, pigeon_S_moveStack)

# Coerce to Tracks
pigeon_R_Trcs <- as.Tracks(pigeon_R_moveStack)
pigeon_S_Trcs <- as.Tracks(pigeon_S_moveStack)

# Create a TracksCollection
pigeons_TrC <- TracksCollection(list(Tracks(list(pigeon_R_Trcs[2])),
                                     Tracks(list(pigeon_S_Trcs[2]))))

# Create SpatialPixels covering the extent of the TracksCollection
spPix_pTrC_dim15 <- createSpatialArealObjFromPoints(
  as(pigeons_TrC, "SpatialPointsDataFrame"), 
  desDim = 15, out = "SpatialPixels")

# Create a STF object with one overall time interval
# Note: currently time zone problem in tracksCollectionData with V. 0.1-1
#stf_px_pTrC_1t <- STF(spPix_pTrC_dim15,
#                      time = pigeons_TrC@tracksCollectionData$tmin[1],
#                      endTime = pigeons_TrC@tracksCollectionData$tmax[1])
stf_px_pTrC_1t <- STF(spPix_pTrC_dim15,
                      time = pigeon_R_Trcs@tracksData$tmin[2],
                      endTime = pigeon_R_Trcs@tracksData$tmax[2])

# Aggregate the attribute 'height_raw' with FUN = weighted.mean
agg_pTrC_wMeanSpd <- aggregate(x = pigeons_TrC, by = stf_px_pTrC_1t, 
                               FUN = weighted.mean, na.rm = TRUE,
                               use.data = "ground_speed",
                               simplify = TRUE, weight.points = "equal", 
                               weight.tracks = "byTime", byID = FALSE)

class(agg_pTrC_wMeanSpd)[1]

summary(agg_pTrC_wMeanSpd@data[c(1, 2, 5)])


###################################################
### code chunk number 49: agg_stf_pureSpatial_fig_1
###################################################
#mx <- max(count_Tracks_X1@data$ntraj, na.rm = TRUE)
mx = 6
mycol <- gray(((mx + 2):0/(mx + 2))[2:(mx+1)])
#labelat = c(14, 15, 16, 17, 18, 19, 20, 21)
#labeltext = c(14, 15, 16, 17, 18, 19, 20, 21) #
#mykey <- list(labels=list(at = labelat, labels = labeltext, cex = 1.1), space="top")
mykey <- list(labels = list(cex=1),space="top")
sp::spplot(agg_pTrC_wMeanSpd["ground_speed"], 
       colorkey = mykey, 
       #cuts = mx - 1, 
       cuts = 5, 
       col.regions = mycol,
       #main = "Weighted average speed of two pigeons",
       scales = list(draw = TRUE, tck = c(1,0), cex = 1))


###################################################
### code chunk number 50: appendix_vulture_movestack_prep_1
###################################################
# library(trajaggr)
data(vulture_moveStack)
class(vulture_moveStack)[1]
vulture_TrC <- as(vulture_moveStack, "TracksCollection")
dim(vulture_TrC)
dim(vulture_TrC[1])


###################################################
### code chunk number 51: appendix_vulture_movestack_prep_2
###################################################
# Create a list of adequate MoveStack objects
# library(move)
vulture_moveObjList <- move::split(vulture_moveStack)
vulture_moveStackList <- lapply(vulture_moveObjList, function(x) {
  dates <- as.Date(x@timestamps)
  uniquedates <- unique(dates) 
  moveObjList <- lapply(seq_along(uniquedates), function(y) {
    w <- which(dates == uniquedates[y])
    x[w]
  })
  ms <- move::moveStack(moveObjList)
  # However the timezone in the timestamps slot is dropped
  # when applying move::moveStack. Need to redefine the timezone...
  attr(ms@timestamps, "tzone") <- attr(ms@data$timestamp, "tzone")
  return(ms)
})

# Coerce MoveStack objects to Tracks objects
Tracks_X1 <- as(vulture_moveStackList[[1]], "Tracks")
Tracks_X2 <- as(vulture_moveStackList[[2]], "Tracks")
Tracks_X3 <- as(vulture_moveStackList[[3]], "Tracks")

# Create TracksCollection
vulture_TrC <- TracksCollection(list(Tracks_X1 = Tracks_X1,
                                     Tracks_X2 = Tracks_X2,
                                     Tracks_X3 = Tracks_X3))

dim(vulture_TrC)
names(vulture_TrC@tracksCollection)
dim(vulture_TrC[1])
names(vulture_TrC[1]@tracks)


###################################################
### code chunk number 52: valid_coercion_move_part_1
###################################################
### Coercion of MoveStack to Tracks
# library(trajaggr)
data(vulture_moveStack)
class(vulture_moveStack)[1]
v_Tracks <- as(vulture_moveStack, "Tracks")
class(v_Tracks)[1]

# Compare as-method and generic method
v_Tracks_gen <- as.Tracks(vulture_moveStack)
identical(v_Tracks, v_Tracks_gen)

# Compare some selected data (first and third Track)
rowNames <- lapply(v_Tracks@tracks, function(x) row.names(x@data))
identical(vulture_moveStack@data[rowNames[[1]], ],
          v_Tracks@tracks[[1]]@data[rowNames[[1]], ])
identical(vulture_moveStack@data[rowNames[[3]], ],
          v_Tracks@tracks[[3]]@data[rowNames[[3]], ])
# Compare some selected coords (first Track)
nrows <- lapply(v_Tracks@tracks, function(x) nrow(x@data))
identical(vulture_moveStack@coords[1:nrows[[1]], ],
          v_Tracks@tracks[[1]]@sp@coords)
# Compare some selected timestamps (second Track)
identical(as.numeric( # due to ignore attribute tclass from Track time
  vulture_moveStack@timestamps[(nrows[[1]] + 1):(nrows[[1]]+nrows[[2]])]),
          as.numeric(index(v_Tracks@tracks[[2]]@time)))
# Compare tracksData and idData (ignoring row.names)
idDataNames <- names(vulture_moveStack@idData)
identical(data.frame(vulture_moveStack@idData, row.names = NULL),
          data.frame(v_Tracks@tracksData[ , idDataNames], row.names = NULL))

### Coercion of MoveStack to TracksCollection
v_TrColl <- as.TracksCollection(vulture_moveStack)
class(v_TrColl)[1]

# Compare some selected data (Track of second Tracks object)
rowNames <- row.names(v_TrColl@tracksCollection[[2]]@tracks[[1]]@data)
identical(vulture_moveStack@data[rowNames, ],
          v_TrColl@tracksCollection[[2]]@tracks[[1]]@data[rowNames, ])

# Compare some selected coords (Track of first Tracks object)
identical(vulture_moveStack@coords[1:nrows[[1]], ],
          v_TrColl@tracksCollection[[1]]@tracks[[1]]@sp@coords)

# Compare some selected timestamps (Track of first Tracks object)
identical(as.numeric(vulture_moveStack@timestamps[1:nrows[[1]]]),
          as.numeric( # due to ignore attribute tclass from Track time
            index(v_TrColl@tracksCollection[[1]]@tracks[[1]]@time)))

### Coercion of MoveBurst to Tracks
# Create a MoveBurst object from the first individuals' first day track
# First subset the Move object to the first tracked day
v_X1_Move <- vulture_moveStack[[1]]
day1 <- which(as.Date(v_X1_Move@timestamps) == 
                as.Date(v_X1_Move@timestamps[1]))
v_X1_1_Move <- v_X1_Move[day1]

# Create MoveBurst object with bursts specifying the type of 
# locomotion (on.ground or flying) based on vultures' speed
behav <- rep("on.ground", length(day1))
behav[which(v_X1_Move@data$ground_speed[day1] > 5)] <- "flying"
v_X1_1_mb <- move::burst(v_X1_1_Move, f = behav[1:length(behav) - 1])

# Coerce MoveBurst to Tracks ...
v_X1_1_mbTracks <- as.Tracks(v_X1_1_mb)
class(v_X1_1_mbTracks)[1]


###################################################
### code chunk number 53: valid_coercion_tomove_part_1
###################################################
### Coercion of Tracks to MoveStack
# library(trajaggr)
class(v_Tracks)[1]
# library(move)
v_moveSt <- as.MoveStack(v_Tracks)
class(v_moveSt)[1]

# Compare data, coords and time of original and re-coerced MoveStack
vars <- names(vulture_moveStack@data)
identical(vulture_moveStack@data[ , vars], v_moveSt@data[ , vars])
identical(vulture_moveStack@coords, v_moveSt@coords)
identical(vulture_moveStack@timestamps, v_moveSt@timestamps) 

### Coercion of TracksCollection to MoveStack
class(v_TrColl)[1]
v_moveSt <- as.MoveStack(v_TrColl)
class(v_moveSt)[1]

# Compare data, coords and time of original and re-coerced MoveStack
identical(vulture_moveStack@data[ , vars], v_moveSt@data[ , vars])
identical(vulture_moveStack@coords, v_moveSt@coords)
identical(vulture_moveStack@timestamps, v_moveSt@timestamps) 

# Compare idData (ignoring row.names)
idDataNames <- names(vulture_moveStack@idData)
identical(data.frame(vulture_moveStack@idData, row.names = NULL),
          data.frame(v_moveSt@idData[ , idDataNames], row.names = NULL))

### Coerce Tracks to MoveBurst
class(v_X1_1_mbTracks)[1]
v_X1_1_newMB <- as.MoveBurst(v_X1_1_mbTracks)
class(v_X1_1_newMB)[1]
# Different row.names in data.frame but identical data, e.g. height
identical(v_X1_1_mb@data$height_raw, v_X1_1_newMB@data$height_raw)
# as well as identical coords ...
identical(v_X1_1_mb@coords, v_X1_1_newMB@coords)
# ... and identical time
identical(as.numeric(v_X1_1_mb@timestamps), # as. numeric to ignore attributes
          as.numeric(v_X1_1_newMB@timestamps))


###################################################
### code chunk number 54: valid_coercion_ltraj_part_1
###################################################
# library(trajaggr)
data(wildboars_4Ind_ltraj)
class(wildboars_4Ind_ltraj[1])

# Coerce ltraj track of first individual (first burst) to Track object
wb_1_Track <- as(wildboars_4Ind_ltraj[1], "Track")
class(wb_1_Track)[1]

# Compare some selected data incl. coords and time
identical(wildboars_4Ind_ltraj[[1]]$x, wb_1_Track@sp@coords[ , 1])
identical(as.numeric(wildboars_4Ind_ltraj[[1]]$date), 
          as.numeric(index(wb_1_Track@time)))
identical(wildboars_4Ind_ltraj[[1]]$date, wb_1_Track@endTime) # time=endTime
identical(wildboars_4Ind_ltraj[[1]]$R2n, wb_1_Track@data$R2n)
identical(head(wildboars_4Ind_ltraj[[1]]$abs.angle, -1), 
          wb_1_Track@connections$abs.angle)

str(attr(wildboars_4Ind_ltraj[[1]], "infolocs"))
identical(attr(wildboars_4Ind_ltraj[[1]], "infolocs")[["pkey"]], 
          wb_1_Track@data$pkey)

# Compare as-method and generic method
# Note: Generic method just works with move package version >= 1.4
wb_1_Track_gen <- as.Track(wildboars_4Ind_ltraj[1])
identical(wb_1_Track, wb_1_Track_gen)

# Coercion of third and fourth burst which belong to the same individual
wb_Tracks <- as(wildboars_4Ind_ltraj[3:4], "Tracks")
class(wb_Tracks)[1]
dim(wb_Tracks)

# Coercion of whole ltraj object (4 ind., 5 bursts) to TracksCollection
wb_TracksColl <- as(wildboars_4Ind_ltraj, "TracksCollection")
wb_TracksColl@tracksCollectionData


###################################################
### code chunk number 55: valid_coercion_toLtraj_part_1
###################################################
# Coercion of Track to ltraj
# library(trajaggr)
wb_1_ltraj <- as(wb_1_Track, "ltraj")
class(wb_1_ltraj)

# Compare new ltraj object with original ltraj object / burst
identical(wb_1_ltraj[[1]][1:length(wb_1_ltraj[[1]])], 
          wildboars_4Ind_ltraj[[1]][1:length(wildboars_4Ind_ltraj[[1]])]) 

# Compare as-method and generic method
# Note: Generic method just works with move package version >= 1.4
wb_1_ltraj_gen <- as.ltraj(wb_1_Track)
identical(wb_1_ltraj, wb_1_ltraj_gen)

# Coercion of Tracks to ltraj
wb_3and4_ltraj <- as(wb_Tracks, "ltraj")
class(wb_3and4_ltraj)

# Compare new ltraj object with original ltraj object / bursts
len <- length(wb_3and4_ltraj[[1]])
identical(wb_3and4_ltraj[[1]][1:len], wildboars_4Ind_ltraj[[3]][1:len])
identical(wb_3and4_ltraj[[2]][1:len], wildboars_4Ind_ltraj[[4]][1:len])

# Coercion of TracksCollection to ltraj
wb_ltraj <- as(wb_TracksColl, "ltraj")
class(wb_ltraj)

# Compare new ltraj object with original ltraj object / bursts
identical(wb_ltraj, wildboars_4Ind_ltraj)


