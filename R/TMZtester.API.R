#' Internal functions
#'
#' @param Cdl An handmade double list to determine selected conditions.
#' @param wd Work directory. Default setting is \code{\link[base:getwd]{getwd()}}.
#' @param ftype File type to be read. Defaust setting is ".csv".
#'
#' @import qpcR VBTree utils
#' @return A matrix summary table for all input files.
#' @export API4TMZ
#'
#' @examples
#' \dontrun{
#' variable1 <- c("factor11", "factor12", "factor13")
#' variable2 <- c("factor21", "factor22")
#' variable3 <- c("factor31", "factor32", "factor33", "factor34")
#' conditions <- list(variable1, variable2, variable3)
#' SummaryTable <- API4TMZ(conditions, "/Your_Working_Directory/")
#' SummaryTable
#' }
#' @keywords internal
API4TMZ <- function(Cdl, wd=getwd(), ftype=".csv"){
  if(all(is.character(unlist(Cdl)))==F){
    stop("input list must double list.", call. = FALSE)
  } else{
    # Cdl: a double list for conditions
    # wd: the working directory, default value is getwd()

    # make names
    name <- trvs(dl2vbt(Cdl))
    name <- unlist(name[,1])
    len_name <- length(name)

    # set working directory
    if(wd == getwd()){
      sfd <- ""
    } else {
      sfd <- wd
      wd <- getwd()
    }

    # initialize temp data
    temp_data <- read.csv(paste(wd, sfd, name[1], ftype, sep = ""), header = F)
    temp_data <- as.matrix(temp_data[29:dim(temp_data)[1],])
    temp_data <- temp_data[which(temp_data[,2]==" 1"), 7:8]
    data <- temp_data

    # make data summary table
    for(i in 1:len_name){
      temp_data <- read.csv(paste(wd, sfd, name[i],".csv", sep = ""), header = F)
      temp_data <- as.matrix(temp_data[29:dim(temp_data)[1],])
      temp_data <- temp_data[which(temp_data[,2]==" 1"), 7:8]
      temp_data <- apply(temp_data, 2, as.numeric)
      colnames(temp_data) <- c(paste("Strain", name[i], sep = "-"), paste("Stress", name[i], sep = "-"))
      data <- qpcR:::cbind.na(data, temp_data)
    }
    data <- data[,-c(1:2)] # Delete initial temp data
    data <- data[complete.cases(data),]
    data <- apply(data, 2, as.numeric)
  }
  return(data)
}


#' Internal functions
#'
#' @param makeidx Boolean to control the index column for summary table. Default setting is FALSE.
#' @param ... Arguments to be past to \code{\link[TPMplt:API4TMZ]{API4TMZ}}.
#'
#' @return A data frame summary table for all input files.
#' @export TMZdatainput
#'
#' @examples
#' \dontrun{
#' variable1 <- c("factor11", "factor12", "factor13")
#' variable2 <- c("factor21", "factor22")
#' variable3 <- c("factor31", "factor32", "factor33", "factor34")
#' conditions <- list(variable1, variable2, variable3)
#' SummaryTable <- TMZdatainput(Cdl=conditions, wd="/Your_Working_Directory/")
#' SummaryTable
#' }
#' @keywords internal
TMZdatainput <- function(makeidx=FALSE, ...){
  data <- API4TMZ(...)
  title <- colnames(data)
  write.csv(data, "temp.csv")
  result <- read.csv("temp.csv")
  file.remove("temp.csv")
  colnames(result)[-1] <- title
  colnames(result)[1] <- "idx"
  result <- as.data.frame(result)
  if(makeidx==FALSE){
    result <- result[,-1]
  }
  return(result)
}
