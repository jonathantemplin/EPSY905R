#' Function to check results of HW1 before submitting.
#'
#' Given the nine data frames that HW1 requires, checks each for accuracy and provides a score.
#'
#' @param Husbands A data frame from Question #1
#' @param Wives A data frame from Question #1
#' @param FamilyWide A data frame from Question #2
#' @param FamilyLong A data frame from Question #3
#' @param DescriptivesFW A data frame from Question #4
#' @param DescriptivesFL A data frame from Question #3
#' @param DescriptivesFLTime A data frame from Question #5
#' @param FamilyLong2 A data frame from Question #6
#' @param FamilyLong3 A data frame from Question #7
#'
#' @return The score for HW1 if this file was submitted for credit.
#' @name CheckHW1
#' @export
CheckHW1 = function(Husbands = NULL, Wives = NULL, FamilyWide = NULL, FamilyLong = NULL, DescriptivesFW = NULL, DescriptivesFL = NULL, DescriptivesFLTime = NULL, FamilyLong2 = NULL, FamilyLong3 = NULL){


  tryCatch(load(url("https://jonathantemplin.com/files/multivariate/mv18epsy905/HW1check.RData")),
           error = function(e) stop("Looks like you cannot connect to JT's website. Check your internet connection and try again."),
           warning = function(w) stop("Looks like you cannot connect to JT's website. Check your internet connection and try again."))

  score = 0
  errors = NULL
  if (identical(Husbands, f)){
    score = score + 1
  } else {
    errors = c(errors, "Husbands data frame is incorrect")
  }

  if (identical(Wives, j)){
    score = score + 1
  } else {
    errors = c(errors, "Wives data frame is incorrect")
  }

  if (identical(FamilyWide, b)){
    score = score + 1
  } else {
    errors = c(errors, "FamilyWide data frame is incorrect")
  }

  if (identical(FamilyLong, h)){
    score = score + 1
  } else {
    errors = c(errors, "FamilyLong data frame is incorrect")
  }

  if (identical(DescriptivesFW, z)){
    score = score + 1
  } else {
    errors = c(errors, "DescriptivesFW data frame is incorrect")
  }

  if (identical(DescriptivesFL, l)){
    score = score + 1
  } else {
    errors = c(errors, "DescriptivesFL data frame is incorrect")
  }

  if (identical(DescriptivesFLTime, a)){
    score = score + 1
  } else {
    errors = c(errors, "DescriptivesFLTime data frame is incorrect")
  }

  if (identical(FamilyLong2, p)){
    score = score + 1
  } else {
    errors = c(errors, "FamilyLong2 data frame is incorrect")
  }

  if (identical(FamilyLong3, o)){
    score = score + 1
  } else {
    errors = c(errors, "FamilyLong3 data frame is incorrect")
  }

  if (score == 9){
    print(paste("Congratulations! Your current score is:", score, "out of 9. Save this file and send it to JT immediately!"))
  } else {
    print(paste("Your current score is:", score, "out of 9."))
    print("You have errors with:")
    print(errors)
  }

  return(score)


}
