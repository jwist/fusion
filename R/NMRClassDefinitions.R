#' An S4 class for NMRPeak1D
#'
#' @slot x Ordinate of the peak
#' @slot y height of the peak
#' @slot fwhm Full width at half maximum of peak
#' @slot shape (optional) A peak shape for the different peaks conforming the signal. It uses the parent shape if void

#' @return a dataElement
#' @export
#' @importFrom crayon %+%
setClass("NMRPeak1D",
         representation = representation(x = "numeric",
                                         y = "numeric",
                                         fwhm = "numeric",
                                         shape = "list",
                                         type = "character"),
         prototype(x = NA_real_,
                   y = NA_real_,
                   fwhm = NA_real_,
                   shape = list(),
                   type = "NMRPeak1D")
)


#' An S4 class for NMRSignal1D
#'
#' @slot peaks a list of NMRPeak1D
#' @slot nbAtoms Number of atoms associated with the signal
#' @slot integration Raw non-normalized integration of the signal
#' @slot chemicalShift (optional) The chemical shift of the signal. Not always its center
#' @slot multiplicity (optional) A compiled NMR multiplicity pattern i.e [s|d|t|q|s,...|dd,...]
#' @slot shiftRange (optional) Range of x-peaks variation. It is an absolute value. Should be positive
#' @slot heightRangePer (optional) Proportional range of y-peaks variation. Must be between 0 and 1
#' @slot widthFactor (optional) Width factor depending on the signal
#' @slot shape (optional) A peak shape for the different peaks conforming the signal. Internal components overrides this shape
#' @slot diaIDs (optional) A list of atom ids to which this signal is assigned.
#' @slot analyte (optional) The name/id of the analyte
#' @slot validated (optional) Validated by an expert?
#' @slot id (optional) Unique identifier
#' @return a dataElement
#' @export
#' @importFrom crayon %+%
setClass("NMRSignal1D",
         representation = representation(peaks = "list",
                                         nbAtoms = "numeric",
                                         integration = "numeric",
                                         chemicalShift = "numeric",
                                         multiplicity = "character",
                                         shiftRange = "numeric",
                                         heightRangePer = "numeric",
                                         widthFactor="numeric",
                                         shape = "list",
                                         diaIDs = "character",
                                         analyte = "character",
                                         validated = "ANY",
                                         type = "character",
                                         id = "character"),
         prototype(peaks = list(),
                   nbAtoms = 0,
                   integration = 0,
                   chemicalShift = NA_real_,
                   multiplicity = NA_character_,
                   shiftRange = NA_real_,
                   heightRangePer = NA_real_,
                   widthFactor=1,
                   shape = list(),
                   diaIDs = NA_character_,
                   analyte = NA_character_,
                   validated = 0,
                   type = "NMRSignal1D",
                   id = NA_character_),
         validity = function(object) {
           if (object@nbAtoms < 0) {
             stop(crayon::red("fusion:ClassNMRSignal1D >> nbAtoms must greather or equal than 0"))
           }
           if (object@integration < 0) {
             stop(crayon::red("fusion:ClassNMRSignal1D >> integration must greather or equal than 0"))
           }
           # Check that peaks are of type NMRPeak1D
           if (length(object@peaks) > 0) {
             if (!class(object@peaks[[1]])[[1]] == "NMRPeak1D") {
               stop(crayon::red("fusion:ClassNMRSignal1D >> peaks must be of type NMRPeak1D"))
             }
           }
           TRUE
         }
)

#' An S4 class for Analyte
#'
#' @slot signals a list of NMRSignal1D
#' @slot category A string specifying a category for the compound. ie.e: ['metabolite', 'substrate source']
#' @slot name A human readable name for the analyte
#' @slot inchiKey A string used to identify a molecule
#' @slot diaID A canonical molecule ID
#' @slot id (optional) Unique identifier
#' @return a dataElement
#' @export
#' @importFrom crayon %+%
setClass("Analyte",
         representation = representation(signals = "list",
                                         category = 'character',
                                         name = "character",
                                         inchiKey = "character",
                                         diaID = "character",
                                         type = "character",
                                         id = "character"),
         prototype(signals = list(),
                   category = NA_character_,
                   name = NA_character_,
                   inchiKey = NA_character_,
                   diaID = NA_character_,
                   type = "Analyte",
                   id = NA_character_),
         validity = function(object) {
           # Check that peaks are of type NMRSignal1D
           if (length(object@signals) > 0) {
             if (!class(object@signals[[1]])[[1]] == "NMRSignal1D") {
               stop(crayon::red("fusion:ClassAnalyte >> peaks must be of type NMRSignal1D"))
             }
           }
           TRUE
         }
)

#' An S4 class for NMRSignalModel
#'
#' @slot signals a list of NMRSignal1D
#' @slot from start point for ROI
#' @slot to end point for ROI
#' @slot ppm Array of x-values from spectrum
#' @slot experimental Array of y-values from spectrum
#' @slot fitted Array of y-values given by the optimization
#' @slot signalsOutput A list of signal inputs with the optimized parameters
#' @slot shape (optional) A peak shape for the different peaks conforming the signal. Internal components overrides this shape
#' @slot error A list of different errors. I'll explain later
#' @slot id (optional) Unique identifier
#' @return a dataElement
#' @export
#' @importFrom crayon %+%
setClass("NMRSignalModel",
         representation = representation(signalsInput = "list",
                                         from = "numeric",
                                         to = "numeric",
                                         ppm = "numeric",
                                         experimental = "numeric",
                                         fitted = "numeric",
                                         signalsOutput = "list",
                                         shape = "list",
                                         error = "list",
                                         type = "character",
                                         id = "character"),
         prototype(signalsInput = list(),
                   from = NA_real_,
                   to = NA_real_,
                   ppm = NA_real_,
                   experimental = NA_real_,
                   fitted = NA_real_,
                   signalsOutput = list(),
                   shape = list(),
                   error = list(),
                   type = "NMRSignalModel",
                   id = NA_character_),
         validity = function(object) {
           # Check that peaks are of type NMRSignal1D
           if (length(object@signalsInput) > 0) {
             if (!class(object@signalsInput[[1]])[[1]] == "NMRSignal1D") {
               stop(crayon::red("fusion:ClassAnalyte >> peaks must be of type NMRSignal1D"))
             }
           }
           TRUE
         }
)

setGeneric("toJSON", function(obj, control=NA) standardGeneric("toJSON")) 

setMethod("toJSON", signature(obj="NMRPeak1D", control="ANY"),
          function(obj, control=NA) {
            json <- "{"
            sep <- ""
            for(slotName in names(getSlots(is(obj)))) {
              value <- slot(obj, slotName)
              if (length(value) > 0 && all(!is.na(value))) {
                json <- paste0(json, sep, '"',slotName, '":', toJSON(value, control))
                sep <- ","
              }
            }
            
            return(paste0(json, "}"))
          }
)

setMethod("toJSON", signature(obj="NMRSignal1D", control="ANY"),
          function(obj, control=NA) {
            json <- "{"
            sep <- ""
            for(slotName in names(getSlots(is(obj)))) {
              value <- slot(obj, slotName)
              if (length(value) > 0 && all(!is.na(value))) {
                json <- paste0(json, sep, '"',slotName, '":', toJSON(value, control))
                sep <- ","
                
              }
            }
            return(paste0(json, "}"))
          }
)

setMethod("toJSON", signature(obj="Analyte", control="ANY"),
          function(obj, control=NA) {
            json <- "{"
            sep <- ""
            for(slotName in names(getSlots(is(obj)))) {
              value <- slot(obj, slotName)
              if (length(value) > 0 && all(!is.na(value))) {
                json <- paste0(json, sep, '"',slotName, '":', toJSON(value, control))
                sep <- ","
                
              }
            }
            return(paste0(json, "}"))
          }
)

setMethod("toJSON", signature(obj="NMRSignalModel", control="ANY"),
          function(obj, control=NA) {
            json <- "{"
            sep <- ""
            slotNames = names(getSlots(is(obj)))
            
            # A hack to avoid the xy being exported
            if ("no_xy" %in% names(control)) {
              if (control["no_xy"])
                slotNames <- slotNames[!(slotNames %in% c('experimental', 'ppm'))]
            }
            
            for(slotName in slotNames) {
              value <- slot(obj, slotName)
              if (length(value) > 0 && all(!is.na(value))) {
                json <- paste0(json, sep, '"',slotName, '":', toJSON(value, control))
                sep <- ","
                
              }
            }
            return(paste0(json, "}"))
          }
)

setMethod("toJSON", signature(obj="list", control="ANY"),
          function(obj, control=NA) {
            lnames <- names(obj)
            if (is.null(lnames)) {
              json <- "["
              sep <- ""
              for (i in 1:length(obj)) {
                tmp <- toJSON(obj[[i]], control)
                json <- paste0(json, sep, tmp)
                sep <- ","
              }
              return(paste0(json, "]"))
            } else {
              json <- "{"
              sep <- ""
              for (i in 1:length(obj)) {
                slotName <- lnames[i]
                if (slotName == "")
                  slotName = i
                json <- paste0(json, sep, '"',slotName, '":', toJSON(obj[[i]], control))
                sep <- ","
              }
              return(paste0(json, "}"))
            }
          }
)

setMethod("toJSON", signature(obj="vector", control="ANY"),
          function(obj, control=NA) {
            return(jsonlite::toJSON(obj, control))
          }
)

setMethod("toJSON", signature(obj="numeric", control="ANY"),
          function(obj, control=NA) {
            if (length(obj) > 1) {
              json <- "["
              sep <- ""
              for (i in 1:length(obj)) {
                json <- paste0(json, sep, toJSON(obj[[i]], control))
                sep <- ","
              }
              return(paste0(json, "]"))
            } else {
              if(is.na(obj)) {
                return("null")
              }
              if (is.infinite(obj)) {
                if (obj < 0) {
                  return("-2e52") 
                } else {
                  return("2e52") 
                }
              }
              return(as.character(obj))
            }
          }
)

setMethod("toJSON", signature(obj="logical", control="ANY"),
          function(obj, control=NA) {
            if (length(obj) > 1) {
              json <- "["
              sep <- ""
              for (i in 1:length(obj)) {
                json <- paste0(json, sep, toJSON(obj[[i]], control))
                sep <- ","
              }
              return(paste0(json, "]"))
            } else {
              if( is.na(obj)) {
                return("null")
              }
              if (obj == TRUE) {
                return("true")
              } else {
                return("false")
              }
            }
          }
)

setMethod("toJSON", signature(obj="character", control="ANY"),
          function(obj, control=NA) {
            if (length(obj) > 1) {
              json <- "["
              sep <- ""
              for (i in 1:length(obj)) {
                json <- paste0(json, sep, toJSON(obj[[i]], control))
                sep <- ","
              }
              return(paste0(json, "]"))
            } else {
              return(paste0('"', obj, '"'))
            }
          }
)

setMethod("toJSON", signature(obj="matrix", control="ANY"),
          function(obj, control=NA) {
            if (length(obj) > 1) {
              json <- "["
              sep <- ""
              for (i in 1:dim(obj)[[1]]) {
                json <- paste0(json, sep, toJSON(obj[i,], control))
                sep <- ","
              }
              return(paste0(json, "]"))
            } else {
              return(paste0('"', obj, '"'))
            }
          }
)

setGeneric("fromVector", function(input) standardGeneric("fromVector")) 

setMethod("fromVector", signature(input="ANY"),
          function(input) {
            listNames <- names(input)
            if (is.null(listNames)) {
              if (length(input)==1 && any(c("boolean", "character", "logical", "numeric") %in%  is(input))) {
                return(input)
              } else {
                return(unlist(lapply(input, function(row) {fromVector(row)})))
              }
            }else if ("type" %in% listNames) {
              output <- new(input[["type"]]);
              slotNames <- names(getSlots(is(output)))
              if (all(!is.na(slotNames))) {
                for (slotName in slotNames) {
                  if (slotName %in% listNames) {
                    slot(output, slotName) <- fromVector(input[[slotName]])
                  }
                }
              }
              return(output)
            } else {
              return(lapply(input, function(row) {fromVector(row)}))
            }
          }
)