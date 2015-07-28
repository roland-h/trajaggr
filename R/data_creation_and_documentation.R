
# ###########################################################################
#
# (Example) Data creation and documentation
#
# ###########################################################################



# ###########################################################################
#
# move data
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Vulture example data
# ---------------------------------------------------------------------------

# vulture_moveStack 

# vulture data creation in /data
#devtools::use_data(vulture_moveStack, vulture_moveStack, overwrite = TRUE)

# vulture data documentation
#' \code{vulture_moveStack} - example data from movebank
#' 
#' Example data from movebank stored in an object of 
#' class \code{MoveStack}, representing three tracked vulture individuals.
#' 
#' The vultures data comes from a study about search efficiency of vultures foraging on spatio-temporally unpredictable carcasses in the Etosha National Park in Namibia.
#' 
#' The \code{MoveStack} object \code{vulture_moveStack} represents a subset of the original
#' data set downloaded from movebank and contains trajectory data of three vulture individuals, 
#' named X1 (Gyps africanus), X2 (Torgos tracheliotus) and X3 (Gyps africanus). 
#' 
#' The permission to use the data was obtained from the owners of the data (Contact person: Orr Spiegel).
#' 
#' For further details please see the vignette.
#' 
#' @name vulture_moveStack
#' 
#' @usage data(vulture_moveStack)
#' 
#' @format An object of class \code{MoveStack}
#' 
#' @source 
#' Spiegel O, Getz WM, Nathan R (2013) Factors influencing foraging search efficiency: Why do scarce lappet-faced vultures outperform ubiquitous white-backed vultures? The American Naturalist 181(5), E102-115.
#' 
#' Spiegel O, Getz WM, Nathan R (2014) Data from: Factors influencing foraging search efficiency: Why do scarce lappet-faced vultures outperform ubiquitous white-backed vultures? (V2). Movebank Data Repository. doi:10.5441/001/1.mf903197
#' 
#' Wikelski, M., and Kays, R. 2014. Movebank: archive, analysis and sharing of animal movement data. World Wide Web electronic publication. http://www.movebank.org, accessed on 2014-11-16. 
#' 
#' @examples
#' ## load example data
#' ## data(vulture_moveStack, package = "trajaggr")
#' data(vulture_moveStack) # loads the data
#' class(vulture_moveStack) # class of object
#' # names of contained Move objects
#' levels(slot(vulture_moveStack, "trackId"))
#' length(vulture_moveStack) # number of locations
#' names(vulture_moveStack) # attribute names
#' head(vulture_moveStack) # first rows of data slot
#' # coerce to TracksCollection
#' vulture_TrC <- as.TracksCollection(vulture_moveStack)
#' class(vulture_TrC)
#' # for further details run
#' # ?vulture_moveStack
"vulture_moveStack"



# ---------------------------------------------------------------------------
# pigeon example data
# ---------------------------------------------------------------------------


# pigeon data creation in /data
#devtools::use_data(pigeon_R_moveStack, pigeon_R_moveStack, overwrite = TRUE)
#devtools::use_data(pigeon_S_moveStack, pigeon_S_moveStack, overwrite = TRUE)
#devtools::use_data(pigeon_R_moveSt_sub, pigeon_R_moveSt_sub, overwrite = TRUE)
#devtools::use_data(pigeon_S_moveSt_sub, pigeon_S_moveSt_sub, overwrite = TRUE)

# pigeon data documentation

#' \code{pigeon_R_moveStack} - example data from movebank
#' 
#' Example data from movebank stored in an object of 
#' class \code{MoveStack}. The object contains two trajectories of a pigeon individual.
#' 
#' The pigeons data come from a project, that studied the leadership-based flock structures of homing
#' pigeons (Columba livia). In particular the repeatability of leadership-based flock structures was studied
#' within a flight and across multiple flights conducted with the same animals.
#' 
#' The \code{MoveStack} object contains two trajectories of a pigeon individual.
#' It is a subset from the original data set. It contains 480 trajectory points sampled over a period
#' of 2 minutes. The sampling rate is regular with four samples per second.
#' 
#' The permission to use the data was obtained from the owners of the data (Contact person: Carlos David Santos).
#' 
#' For further details please see the vignette.
#' 
#' @name pigeon_R_moveStack
#' 
#' @usage data(pigeon_R_moveStack)
#' 
#' @format An object of class \code{MoveStack}
#' 
#' @source 
#' Santos CD, Neupert S, Lipp H-P, Wikelski M, Dechmann D (2014) Temporal and contextual consistency of leadership in homing pigeon flocks. PLOS ONE 9(7): e102771. 
#' 
#' Santos CD, Neupert S, Lipp H, Wikelski M, Dechmann D (2014) Data from: Temporal and contextual consistency of leadership in homing pigeon flocks. Movebank Data Repository. doi:10.5441/001/1.33159h1h
#' 
#' Wikelski, M., and Kays, R. 2014. Movebank: archive, analysis and sharing of animal movement data. World Wide Web electronic publication. http://www.movebank.org, accessed on 2014-11-16.
#' 

#' @examples
#' ## load example data
#' ## data(pigeon_R_moveStack, package = "trajaggr")
#' data(pigeon_R_moveStack)
#' class(pigeon_R_moveStack)
#' # names of contained trajectories objects
#' levels(slot(pigeon_R_moveStack, "trackId"))
#' head(pigeon_R_moveStack)
#' # coerce to Tracks
#' pigeon_R_Trcs <- as.Tracks(pigeon_R_moveStack)
#' class(pigeon_R_Trcs)
#' # for further details run
#' # ?pigeon_R_moveStack
"pigeon_R_moveStack"


#' \code{pigeon_S_moveStack} - example data from movebank
#' 
#' Example data from movebank containing one object of 
#' class \code{MoveStack}. The object contains two trajectories of a pigeon individual.
#' 
#' The pigeons data come from a project, that studied the leadership-based flock structures of homing
#' pigeons (Columba livia). In particular the repeatability of leadership-based flock structures was studied
#' within a flight and across multiple flights conducted with the same animals.
#' 
#' The \code{MoveStack} object contains two trajectories of a pigeon individual.
#' It is a subset from the original data set. It contains 480 trajectory points sampled over a period
#' of 2 minutes. The sampling rate is regular with four samples per second.
#' 
#' The permission to use the data was obtained from the owners of the data (Contact person: Carlos David Santos).
#' 
#' For further details please see the vignette.
#' 
#' @name pigeon_S_moveStack
#' 
#' @usage data(pigeon_S_moveStack)
#' 
#' @format An object of class \code{MoveStack}
#' 
#' @source 
#' Santos CD, Neupert S, Lipp H-P, Wikelski M, Dechmann D (2014) Temporal and contextual consistency of leadership in homing pigeon flocks. PLOS ONE 9(7): e102771. 
#' 
#' Santos CD, Neupert S, Lipp H, Wikelski M, Dechmann D (2014) Data from: Temporal and contextual consistency of leadership in homing pigeon flocks. Movebank Data Repository. doi:10.5441/001/1.33159h1h
#' 
#' Wikelski, M., and Kays, R. 2014. Movebank: archive, analysis and sharing of animal movement data. World Wide Web electronic publication. http://www.movebank.org, accessed on 2014-11-16.
#' 
#' @examples
#' ## load example data
#' ## data(pigeon_S_moveStack, package = "trajaggr")
#' data(pigeon_S_moveStack)
#' class(pigeon_S_moveStack)
#' # names of contained Move objects
#' levels(slot(pigeon_S_moveStack, "trackId"))
#' head(pigeon_S_moveStack)
#' # coerce to Tracks
#' pigeon_S_Trcs <- as.Tracks(pigeon_S_moveStack)
#' class(pigeon_S_Trcs)
#' # for further details run
#' # ?pigeon_S_moveStack
"pigeon_S_moveStack"


#############################
#### Pigeon subsets of 15 sec

### pigeon R small subset

#' \code{pigeon_R_moveSt_sub} - small subset of the pigeons' example data from movebank
#' 
#' A small subset of the example data from movebank containing one object of 
#' class \code{MoveStack}. This subset is used for illustration in the vignette.
#' 
#' The subset contains a \code{MoveStack} object containing two \code{Move} objects representing 
#' pigeon trajectories of 15 seconds respectively 60 trajectory points. For further details 
#' run \code{?pigeon_R_moveStack}.
#' 
#' The permission to use the data was obtained from the owners of the data (Contact person: Carlos David Santos).
#' 
#' @name pigeon_R_moveSt_sub
#' 
#' @usage data(pigeon_R_moveSt_sub)
#' 
#' @format An object of class \code{MoveStack}
#' 
#' @source 
#' Santos CD, Neupert S, Lipp H-P, Wikelski M, Dechmann D (2014) Temporal and contextual consistency of leadership in homing pigeon flocks. PLOS ONE 9(7): e102771. 
#' 
#' Santos CD, Neupert S, Lipp H, Wikelski M, Dechmann D (2014) Data from: Temporal and contextual consistency of leadership in homing pigeon flocks. Movebank Data Repository. doi:10.5441/001/1.33159h1h
#' 
#' Wikelski, M., and Kays, R. 2014. Movebank: archive, analysis and sharing of animal movement data. World Wide Web electronic publication. http://www.movebank.org, accessed on 2014-11-16.
#' 

#' @examples
#' ## load example data
#' ## data(pigeon_R_moveSt_sub, package = "trajaggr")
#' data(pigeon_R_moveSt_sub)
#' class(pigeon_R_moveSt_sub)
#' # names of contained Move objects
#' levels(slot(pigeon_R_moveSt_sub, "trackId"))
#' head(slot(pigeon_R_moveSt_sub, "data"))
#' # for further details run
#' # ?pigeon_R_moveSt_sub
#' # ?pigeon_R_moveStack
"pigeon_R_moveSt_sub"


### pigeon S small subset

#' \code{pigeon_S_moveSt_sub} - small subset of the pigeons' example data from movebank
#' 
#' A small subset of the example data from movebank containing one object of 
#' class \code{MoveStack}. This subset is used for illustration in the vignette.
#' 
#' The subset contains a \code{MoveStack} object containing two \code{Move} objects representing 
#' pigeon trajectories of 15 seconds respectively 60 trajectory points. For further details 
#' run \code{?pigeon_S_moveStack}.
#' 
#' The permission to use the data was obtained from the owners of the data (Contact person: Carlos David Santos).
#' 
#' @name pigeon_S_moveSt_sub
#' 
#' @usage data(pigeon_S_moveSt_sub)
#' 
#' @format An object of class \code{MoveStack}
#' 
#' @source 
#' Santos CD, Neupert S, Lipp H-P, Wikelski M, Dechmann D (2014) Temporal and contextual consistency of leadership in homing pigeon flocks. PLOS ONE 9(7): e102771. 
#' 
#' Santos CD, Neupert S, Lipp H, Wikelski M, Dechmann D (2014) Data from: Temporal and contextual consistency of leadership in homing pigeon flocks. Movebank Data Repository. doi:10.5441/001/1.33159h1h
#' 
#' Wikelski, M., and Kays, R. 2014. Movebank: archive, analysis and sharing of animal movement data. World Wide Web electronic publication. http://www.movebank.org, accessed on 2014-11-16.
#' 

#' @examples
#' ## load example data
#' ## data(pigeon_S_moveSt_sub, package = "trajaggr")
#' data(pigeon_S_moveSt_sub)
#' class(pigeon_S_moveSt_sub)
#' # names of contained Move objects
#' levels(slot(pigeon_S_moveSt_sub, "trackId"))
#' head(slot(pigeon_S_moveSt_sub, "data"))
#' # for further details run
#' # ?pigeon_S_moveSt_sub
#' # ?pigeon_S_moveStack
"pigeon_S_moveSt_sub"

##########################


# ###########################################################################
#
# adehabitatLT / ltraj data
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Wild boars example data (Modified 'puechabonsp' data set from adehabitatLT)
# ---------------------------------------------------------------------------

# wildboars_4Ind_ltraj

# wild boars data creation in /data
#devtools::use_data(wildboars_4Ind_ltraj, wildboars_4Ind_ltraj, overwrite = TRUE)


# wild boars data documentation
#' \code{wildboars_4Ind_ltraj} - (adjusted) example data from the package \pkg{adehabititLT}
#' 
#' Five trajectories of four tracked wild boars. A modiefied data set from the package \pkg{adehabititLT}.
#'
#' This data set is a modified version of the trajectory data stored in the object 
#' \code{puechabonsp}, which is provided by the package \pkg{adehabititLT}. The data represent 
#' the results of the monitoring of 4 wild boars in 1993 at 
#' Puechabon (Mediterranean habitat, South of France) and thus the original data 
#' contains four trajectories. The data set is modified in a way, 
#' that the trajectory of one individual is split into two trajectories respectively bursts.
#' 
#' 
#' @name wildboars_4Ind_ltraj
#' 
#' @usage data(wildboars_4Ind_ltraj)
#' 
#' @format An object of class \code{ltraj}
#' 
#' @source 
#' 
#' Original data source:
#' 
#' Maillard, D. (1996). Occupation et utilisation de la garrigue et du vignoble mediterraneens par 
#' le Sanglier. Universite d'Aix-Marseille III: PhD thesis.
#' 
#' taken from \pkg{adehabititLT}:
#' 
#' Calenge, C. (2006) The package adehabitat for the R software: a tool for the analysis of space
#' and habitat use by animals. Ecological Modelling, 197, 516-519
#' 
#' 
#' 
#' @examples
#' ## load example data
#' ## data(wildboars_4Ind_ltraj, package = "trajaggr")
#' data(wildboars_4Ind_ltraj)
#' class(wildboars_4Ind_ltraj)
#' #if (requireNamespace("adehabitatLT", quietly = TRUE)) {
#'  adehabitatLT::summary.ltraj(wildboars_4Ind_ltraj) 
#' #}
#' # for further details run
#' # ?wildboars_4Ind_ltraj
"wildboars_4Ind_ltraj"
