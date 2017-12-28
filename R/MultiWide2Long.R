
#' A function that plots the surface of a bivariate normal distribution
#'
#' @param data A data frame that is wide format.
#' @param idvars The id variable(s) that identify multiple records from the same unit.
#' @param varlist A list of length equal to the number of variables to be reshaped from wide to long. The names of the elements will make up the
#'                names of the new long-format variables. Each element of the list is a character vector that contains the variable names in data
#'                that are to be reshaped. All elements must have character vectors of the same length
#' @param timevals A vector of numbers to use for time. Length must be equal to the size of the elements in varlist. Can be NA for no time.
#' @param timevar A character containing the name of the new time variable. Can be NA for default name or no name if timevar is NA.
#' @return A data frome in long format
#' @examples
#' summary(Indometh)
#' wide <- reshape(Indometh, v.names = "conc", idvar = "Subject",
#'                 timevar = "time", direction = "wide")
#' #add additional column so reshape has same size for two variables
#' wide$conc.9 = runif(n = 6, min = 0, max = .05)
#' wide
#'
#' long <- MultiWide2Long(data = wide, idvars = "Subject",
#'                        varlist =
#'                          list(ConcLE2 =
#'                                 c("conc.0.25",
#'                                   "conc.0.5",
#'                                   "conc.0.75",
#'                                   "conc.1",
#'                                   "conc.1.25",
#'                                   "conc.2"),
#'                               ConcGT2 =
#'                                 c("conc.3",
#'                                   "conc.4",
#'                                   "conc.5",
#'                                   "conc.6",
#'                                   "conc.8",
#'                                   "conc.9")),
#'                        timevals = 1:6, timevar = "time")
#' long
MultiWide2Long = function(data, idvars, varlist, timevals = 1:length(varlist[[1]]), timevar = "time"){

  newData = NULL
  for (var in 1:length(varlist)){
    temp = reshape(data = data[c(idvars, varlist[[var]])], idvar = idvars, varying = varlist[[var]],
                   v.names = names(varlist)[var], direction = "long")

    # for first reformat data frame to be correct type
    if (var == 1) {
      if (is.na(timevar)) {
        temp = temp[c(idvars, names(varlist)[var])]
      }

      newData = temp
    } else {

      newData = cbind(newData, temp[3])
    }

  }

  return(newData)

}
