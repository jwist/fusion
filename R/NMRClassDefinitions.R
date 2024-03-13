#' An S4 class for NMRPeak1D
#'
#' @slot x Ordinate of the peak
#' @slot y height of the peak
#' @slot fwhm Full width at half maximum of peak
#' @slot shape (optional) A peak shape for the different peaks conforming the signal. It uses the parent shape if void
#' @slot type The Class Name. Used for moving between S4 and JSON
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
#' @slot multiplicity (optional) A compiled NMR multiplicity pattern i.e: s|d|t|q|s,...|dd,...
#' @slot shiftRange (optional) Range of x-peaks variation. It is an absolute value. Should be positive
#' @slot heightRangePer (optional) Proportional range of y-peaks variation. Must be between 0 and 1
#' @slot widthFactor (optional) Width factor depending on the signal
#' @slot shape (optional) A peak shape for the different peaks conforming the signal. Internal components overrides this shape
#' @slot diaIDs (optional) A list of atom ids to which this signal is assigned.
#' @slot analyte (optional) The name/id of the analyte
#' @slot validated (optional) Validated by an expert?
#' @slot type The Class Name. Used for moving between S4 and JSON
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
#' @slot category A string specifying a category for the compound. ie.e: 'metabolite', 'substrate source'
#' @slot name A human readable name for the analyte
#' @slot inchiKey A string used to identify a molecule
#' @slot diaID A canonical molecule ID
#' @slot type The Class Name. Used for moving between S4 and JSON
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
#' @slot signalsInput a list of NMRSignal1D
#' @slot from start point for ROI
#' @slot to end point for ROI
#' @slot ppm Array of x-values from spectrum
#' @slot experimental Array of y-values from spectrum
#' @slot fitted Array of y-values given by the optimization
#' @slot signalsOutput A list of signal inputs with the optimized parameters
#' @slot shape (optional) A peak shape for the different peaks conforming the signal. Internal components overrides this shape
#' @slot error A list of different errors. I'll explain later
#' @slot type The Class Name. Used for moving between S4 and JSON
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

#' Method for creating a JSON file out of an object, that contains S4 objects of the types contained in
#' this file.
#'
#' @param obj A data object to be parsed (list, array or S4)
#' @param control A set of control parameters for the transformation. We use it to avoid export some S4 slots
#' @param con A connection to the output file
#' @return void
#'
#' @export
#'
setGeneric("toJSONFile", function(obj, control=NA, con="ANY") standardGeneric("toJSONFile"))

#' Method for creating a JSON file out of an NMRPeak1D
#'
#' @param obj A data object to be parsed (list, array or S4)
#' @param control A set of control parameters for the transformation. We use it to avoid export some S4 slots
#' @param con A connection to the output file
#' @return void
#'
#' @export
#'
setMethod("toJSONFile", signature(obj="NMRPeak1D", control="ANY", con="ANY"),
          function(obj, control=NA, con) {
            write("{", con, append = TRUE, sep="")
            sep <- ""
            for(slotName in names(getSlots(is(obj)))) {
              value <- slot(obj, slotName)
              if (length(value) > 0 && !all(is.na(value))) {
                write(paste0(sep, '"',slotName, '":'), con, append = TRUE, sep="")
                toJSONFile(value, control, con)
                sep <- ","
              }
            }
            write("}", con, append = TRUE, sep="")
          }
)

#' Method for creating a JSON file out of an NMRSignal1D
#'
#' @param obj A data object to be parsed (list, array or S4)
#' @param control A set of control parameters for the transformation. We use it to avoid export some S4 slots
#' @param con A connection to the output file
#' @return void
#'
#' @importFrom methods getSlots slot
#' @export
#'
setMethod("toJSONFile", signature(obj="NMRSignal1D", control="ANY", con="ANY"),
          function(obj, control=NA, con) {
            write("{", con, append = TRUE, sep="")
            sep <- ""
            for(slotName in names(getSlots(is(obj)))) {
              value <- slot(obj, slotName)
              if (length(value) > 0 && !all(is.na(value))) {
                write(paste0(sep, '"',slotName, '":'), con, append = TRUE, sep="")
                toJSONFile(value, control, con)
                sep <- ","
              }
            }
            write("}", con, append = TRUE, sep="")
          }
)

#' Method for creating a JSON file out of an Analyte
#'
#' @param obj A data object to be parsed (list, array or S4)
#' @param control A set of control parameters for the transformation. We use it to avoid export some S4 slots
#' @param con A connection to the output file
#' @return void
#'
#' @export
#'
setMethod("toJSONFile", signature(obj="Analyte", control="ANY", con="ANY"),
          function(obj, control=NA, con) {
            write("{", con, append = TRUE, sep="")
            sep <- ""
            for(slotName in names(getSlots(is(obj)))) {
              value <- slot(obj, slotName)
              if (length(value) > 0 && !all(is.na(value))) {
                write(paste0(sep, '"',slotName, '":'), con, append = TRUE, sep="")
                toJSONFile(value, control, con)
                sep <- ","
              }
            }
            write("}", con, append = TRUE, sep="")
          }
)

#' Method for creating a JSON file out of an NMRSignalModel
#'
#' @param obj A data object to be parsed (list, array or S4)
#' @param control A set of control parameters for the transformation. We use it to avoid export some S4 slots
#' @param con A connection to the output file
#' @return void
#'
#' @export
#'
setMethod("toJSONFile", signature(obj="NMRSignalModel", control="ANY", con="ANY"),
          function(obj, control=NA, con) {
            write("{", con, append = TRUE, sep="")
            sep <- ""
            slotNames = names(getSlots(is(obj)))

            # A hack to avoid the xy being exported
            if ("no_xy" %in% names(control)) {
              if (control["no_xy"])
                slotNames <- slotNames[!(slotNames %in% c('experimental', 'ppm', "fitted"))]
            }

            for(slotName in slotNames) {
              value <- slot(obj, slotName)
              if (length(value) > 0 && !all(is.na(value))) {
                write(paste0(sep, '"',slotName, '":'), con, append = TRUE, sep="")
                toJSONFile(value, control, con)
                sep <- ","
              }
            }
            write("}", con, append = TRUE, sep="")
          }
)

#' Method for creating a JSON file out of a list
#'
#' @param obj A data object to be parsed (list, array or S4)
#' @param control A set of control parameters for the transformation. We use it to avoid export some S4 slots
#' @param con A connection to the output file
#' @return void
#'
#' @export
#'
setMethod("toJSONFile", signature(obj="list", control="ANY", con="ANY"),
          function(obj, control=NA, con) {
            lnames <- names(obj)
            if (is.null(lnames)) {
              write("[", con, append = TRUE, sep="")
              sep <- ""
              for(element in obj) {
                #if(!all(is.na(element))) {
                  write(sep, con, append = TRUE, sep="")
                  toJSONFile(element, control, con)
                  sep <- ","
                #}
              }
              write("]", con, append = TRUE, sep="")
            } else {
              write("{", con, append = TRUE, sep="")
              sep <- ""
              i <- 0
              for (slotName in lnames) {
                if (slotName == "")
                  slotName = i
                #if(!all(is.na(obj[[slotName]]))) {
                  write(paste0(sep, '"',slotName, '":'), con, append = TRUE, sep="")
                  toJSONFile(obj[[slotName]], control, con)
                  sep <- ","
                #}
                i <- i + 1
              }
              write("}", con, append = TRUE, sep="")
            }
          }
)

#' Method for creating a JSON file out of a vector
#'
#' @param obj A data object to be parsed (list, array or S4)
#' @param control A set of control parameters for the transformation. We use it to avoid export some S4 slots
#' @param con A connection to the output file
#' @return void
#'
#' @export
#'
setMethod("toJSONFile", signature(obj="vector", control="ANY", con="ANY"),
          function(obj, control=NA, con) {
            write(jsonlite::toJSON(obj, control), con, append = TRUE, sep="")
          }
)

#' Method for creating a JSON file out of a numeric
#'
#' @param obj A data object to be parsed (list, array or S4)
#' @param control A set of control parameters for the transformation. We use it to avoid export some S4 slots
#' @param con A connection to the output file
#' @return void
#'
#' @export
#'
setMethod("toJSONFile", signature(obj="numeric", control="ANY", con="ANY"),
          function(obj, control=NA, con) {
            if (length(obj) > 1) {
              write("[", con, append = TRUE, sep="")
              sep <- ""
              for (element in obj) {
                #if(!all(is.na(element))) {
                  write(sep, con, append = TRUE, sep="")
                  toJSONFile(element, control, con)
                  sep <- ","
                #}
              }
              write("]", con, append = TRUE, sep="")
            } else {
              if(is.na(obj)) {
                write("null", con, append = TRUE, sep="")
              } else if (is.infinite(obj)) {
                if (obj < 0) {
                  write("-2e52", con, append = TRUE, sep="")
                } else {
                  write("2e52", con, append = TRUE, sep="")
                }
              } else {
                write(as.character(obj), con, append = TRUE, sep="")
              }
            }
          }
)

#' Method for creating a JSON file out of a logical
#'
#' @param obj A data object to be parsed (list, array or S4)
#' @param control A set of control parameters for the transformation. We use it to avoid export some S4 slots
#' @param con A connection to the output file
#' @return void
#'
#' @export
#'
setMethod("toJSONFile", signature(obj="logical", control="ANY", con="ANY"),
          function(obj, control=NA, con) {
            if (length(obj) > 1) {
              write("[", con, append = TRUE, sep="")
              sep <- ""
              for (element in obj) {
                #if(!all(is.na(element))) {
                  write(sep, con, append = TRUE, sep="")
                  toJSONFile(element, control, con)
                  sep <- ","
                #}
              }
              write("]", con, append = TRUE, sep="")
            } else {
              if( is.na(obj)) {
                write("null", con, append = TRUE, sep="")
              } else {
                if (obj == TRUE) {
                  write("true", con, append = TRUE, sep="")
                } else {
                  write("false", con, append = TRUE, sep="")
                }
              }
            }
          }
)

#' Method for creating a JSON file out of a character
#'
#' @param obj A data object to be parsed (list, array or S4)
#' @param control A set of control parameters for the transformation. We use it to avoid export some S4 slots
#' @param con A connection to the output file
#' @return void
#'
#' @export
#'
setMethod("toJSONFile", signature(obj="character", control="ANY", con="ANY"),
          function(obj, control=NA, con) {
            if (length(obj) > 1) {
              sep <- ""
              write("[", con, append = TRUE, sep="")
              for (element in obj) {
                  #if(!all(is.na(element))) {
                  write(sep, con, append = TRUE, sep="")
                  toJSONFile(element, control, con)
                  sep <- ","
                #}
              }
              write("]", con, append = TRUE, sep="")
            } else {
              write(paste0('"', obj, '"'), con, append = TRUE, sep="")
            }
          }
)         

#' Method for creating a JSON file out of a matrix
#'
#' @param obj A data object to be parsed (list, array or S4)
#' @param control A set of control parameters for the transformation. We use it to avoid export some S4 slots
#' @param con A connection to the output file
#' @return void
#'
#' @export
#'
setMethod("toJSONFile", signature(obj="matrix", control="ANY", con="ANY"),
          function(obj, control=NA, con) {
            if (length(obj) > 1) {
              sep <- ""
              write("[", con, append = TRUE, sep="")
              for (i in 1:dim(obj)[[1]]) {
                #if(!all(is.na(obj[[i]]))) {
                  write(sep, con, append = TRUE, sep="")
                  toJSONFile(obj[i,], control, con)
                  sep <- ","
                #}
              }
              write("]", con, append = TRUE, sep="")
            } else {
              write(paste0('"', obj, '"'), con, append = TRUE, sep="")
            }
          }
)

#' Create and simplify a JSON file out of a data object.
#'
#' @param data A data object to be parsed (list, array or S4)
#' @param fileName The name of the file for storing the result
#' @return void
#'
#' @export
#'
writeToJSON <- function(data, fileName) {
  file.create(fileName)
  fileConn<-file(fileName, "wb")
  toJSONFile(data, control=c(no_xy=TRUE), con=fileConn)
  close(fileConn)

  fileLines <-readLines(fileName, encoding="UTF-8")
  fileConn<-file(fileName,"wb")
  write(paste(fileLines, collapse = ""), fileConn, sep = "")
  close(fileConn)
}

#' Introspect a data object and transform any matching structure into the
#' corresponding S4 object
#'
#' @param input A data object to be parsed (list, array or S4)
#' @return an object
#'
#' @export
#'
setGeneric("fromVector", function(input) standardGeneric("fromVector"))

#' Introspect a data object and transform any matching structure into the
#' corresponding S4 object
#'
#' @param input A data object to be parsed (list, array or S4)
#' @return an object
#'
#' @importFrom methods getSlots slot<-
#' @export
#'
setMethod("fromVector", signature(input="ANY"),
          function(input) {
            if (is.null(input)) return(NA)
            listNames <- names(input)
            if (is.null(listNames)) {
              if (length(input)==1 && any(c("boolean", "character", "logical", "numeric") %in%  is(input))) {
                if (input == "NA") return(NA)
                return(input)
              } else {
                tmp <- lapply(input, function(row) {fromVector(row)})
                if (length(tmp) > 0 && length(tmp[[1]]) == 1) {
                  return(unlist(tmp))
                } else {
                  return (tmp)
                }
              }
            }else if ("type" %in% listNames) {
              output <- tryCatch(new(input[["type"]])
                                 ,error=function(e){
                                   lapply(input, function(row) fromVector(row))
                                   }
                                 )
              if(is.object(output)){
                slotNames <- names(getSlots(is(output)))
                #if (all(!is.na(slotNames))) {
                for (slotName in slotNames) {
                  if (slotName %in% listNames) {
                    slot(output, slotName) <- fromVector(input[[slotName]])
                  }
                }
                #}  
              }
              return(output)
            } else {
              return(lapply(input, function(row) {fromVector(row)}))
            }
          }
)
