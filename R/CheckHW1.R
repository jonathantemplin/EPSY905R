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
  if (checkFrameContents(Husbands, f)){
    score = score + 1
  } else {
    errors = c(errors, "Husbands data frame is incorrect")
  }

  if (checkFrameContents(Wives, j)){
    score = score + 1
  } else {
    errors = c(errors, "Wives data frame is incorrect")
  }

  if (checkFrameContents(FamilyWide, b)){
    score = score + 1
  } else {
    errors = c(errors, "FamilyWide data frame is incorrect")
  }

  if (checkFrameContents(FamilyLong, h)){
    score = score + 1
  } else {
    errors = c(errors, "FamilyLong data frame is incorrect")
  }

  if (checkFrameContents(DescriptivesFW, z)){
    score = score + 1
  } else {
    errors = c(errors, "DescriptivesFW data frame is incorrect")
  }

  if (checkFrameContents(DescriptivesFL, l)){
    score = score + 1
  } else {
    errors = c(errors, "DescriptivesFL data frame is incorrect")
  }

  allCheck = unlist(lapply(X = 1:4, FUN = function(x){checkFrameContents(DescriptivesFLTime[[x]], a[[x]])}))
  if (all(allCheck)){
    score = score + 1
  } else {
    errors = c(errors, "DescriptivesFLTime list is incorrect")
  }

  if (checkFrameContents(FamilyLong2, p)){
    score = score + 1
  } else {
    errors = c(errors, "FamilyLong2 data frame is incorrect")
  }

  if (checkFrameContents(FamilyLong3, o)){
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

# checks each variable from df1 to see if it is copied in df2
checkFrameContents = function(df1, df2){

  if (!identical(dim(df1), dim(df2))){
    return(FALSE)
  }

  df1Cols = 1:ncol(df1)
  df2Cols = 1:ncol(df2)

  endWhile = FALSE
  while (!endWhile) {
    cur1 = df1Cols[1]

    foundMatch = FALSE
    for (col in 1:length(df2Cols)){

      cur2 = df2Cols[col]
      if (identical(df1[,cur1], df2[,cur2])){
        foundMatch = TRUE
        df1Cols = df1Cols[which(df1Cols != cur1)]
        df2Cols = df2Cols[which(df2Cols != cur2)]
        break
      }


    }

    if (!foundMatch) endWhile = TRUE
    if (length(df1Cols)==0 & length(df2Cols)==0) endWhile = TRUE
  }

  if (!foundMatch) {
    return(FALSE)
  } else {
    return(TRUE)
  }

}

