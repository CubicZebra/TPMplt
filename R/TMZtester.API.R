# Functions for inputing data from Thermec Master-Z apparatus

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
