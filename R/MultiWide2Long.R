#' A function that reshapes data from wide format to long format
#' when multiple variables are present.
#'
#' Used for reshaping data more extensively than the \code{reshape},
#' \code{melt}, or \code{reshape2} functions available. Can reshape multiple
#' wide-format variables into long format.
#'
#' @param data A data frame that is wide format.
#' @param idvars The id variable(s) that identify multiple records from the
#'   same unit.
#' @param varlist A list of length equal to the number of variables to be
#'   reshaped from wide to long. The names of the elements will make up the
#'   names of the new long-format variables. Each element of the list is a
#'   character vector that contains the variable names in data that are to
#'   be reshaped. All elements must have character vectors of the same length.
#' @param timevals A vector of numbers to use for time. Length must be equal
#'   to the size of the elements in varlist. Can be NA for no time.
#' @param timevar A character containing the name of the new time variable.
#'   Can be NA for default name or no name if timevar is NA.
#' @return A data frome in long format
#' @name MultiWide2Long
#' @export
MultiWide2Long = function(data, idvars, varlist, timevals = 1:length(varlist[[1]]), timevar = "time"){

  newData = NULL
  for (var in 1:length(varlist)){
    temp = stats::reshape(data = data[c(idvars, varlist[[var]])], idvar = idvars, varying = varlist[[var]],
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


