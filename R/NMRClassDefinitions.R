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
                                         shape = "list"),
         prototype(x = NA_real_,
                   y = NA_real_,
                   fwhm = NA_real_,
                   shape = list())
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

#' @slot shape (optional) A peak shape for the different peaks conforming the signal. Internal components overrides this shape
#' @slot diaIDs (optional) A list of atom ids to which this signal is assigned.
#' 
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
                                         shape = "list",
                                         diaIDs = "character"),
         prototype(peaks = list(),
                   nbAtoms = 0,
                   integration = 0,
                   chemicalShift = NA_real_,
                   multiplicity = NA_character_,
                   shiftRange = NA_real_,
                   heightRangePer = NA_real_,
                   shape = list(),
                   diaIDs = NA_character_),
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
#' @return a dataElement
#' @export
#' @importFrom crayon %+%
setClass("Analyte",
         representation = representation(signals = "list",
                                         category = 'character',
                                         name = "character",
                                         inchiKey = "character",
                                         diaID = "character"),
         prototype(signals = list(),
                   category = NA_character_,
                   name = NA_character_,
                   inchiKey = NA_character_,
                   diaID = NA_character_),
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
#' @slot signalOutput A list of signal inputs with the optimized paramaters
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
                                         signalOutput = "list"),
         prototype(signalsInput = list(),
                   from = NA_real_,
                   to = NA_real_,
                   ppm = NA_real_,
                   experimental = NA_real_,
                   fitted = NA_real_,
                   signalOutput = list()
                   ),
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