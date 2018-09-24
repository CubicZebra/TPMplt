#' Auto output for strain rate vs. temperature table
#'
#' @description Automatically output the strain rate vs. temperature table, by a specified strain condition.
#' @param data Input data frame with the style of \code{\link[VBTree:VBTree-package]{VBTree}}.
#' @param eps A numeric value to specify strain condition.
#' @param lyT An integer to specify the layer for temperature attribute in the vector binary tree.
#' @param lySR An integer to specify the layer for strain rate attribute in the vector binary tree.
#' @param manual An integer vector with the length of 3 where the 1st denotes the layer for Stress and Strain, while
#' the 2nd and 3rd represent the levels for Strain and Stress, respectively. The default setting for this value is
#' NULL to call the function \code{\link[TPMplt:lyIDdetector]{lyIDdetector}} for automatically generating this vector.
#'
#' @return A list consist of a matrix table arranged by rows for strain rates while columns for temperatures, and a numeric
#' value as strain condition for this strain rate-temperature table.
#' @import VBTree
#' @export epsExtract
#' @seealso \code{\link[VBTree:VBTree-package]{VBTree}}, \code{\link[TPMplt:lyIDdetector]{lyIDdetector}}
#'
#' @examples
#' dl2vbt(chrvec2dl(colnames(TPMdata)))
#' epsExtract(TPMdata, eps = 0.7, lyT = 2, lySR = 3)
#' @keywords SR-T.table" "epsExtract" "lyIDdetector"
epsExtract <- function(data, eps, lyT, lySR, manual=NULL){
  # data: data.frame format
  # eps: epsilon of a specific strain
  # lyT: an integer to specify the layer of temperature in vector binary tree;
  # lySR: an integer to specify the layer of strain rate in vector binary tree;
  # manual: should be a vector with length 3 while the 1st integer indicate the layer of Stress-Strain,
  #         the 2nd and 3rd to identify the dimensions of Strain and Stress, respectively.

  # # test section
  # manual <- NULL
  # lyT <- 2
  # lySR <- 3

  # input data diagnose:
  if(!is.data.frame(data)){
    warning("input data will be convert to data.frame.", call. = FALSE)
    data <- as.data.frame(data)
  }

  # check for epsilon:
  if(!is.numeric(eps)|length(eps)!=1){
    stop("the arg eps should be a numer.", call. = FALSE)
  }

  # method of manual:
  if(is.null(manual)){
    SSptr <- lyIDdetector(data)
    lySS <- as.numeric(SSptr$layer)
    strainID <- as.numeric(SSptr$strainID)
    stressID <- as.numeric(SSptr$stressID)
  } else {
    lySS <- manual[1]
    strainID <- manual[2]
    stressID <- manual[3]
  }

  # make sub double lists to generate strain and stress vector binary trees
  checkref <- data.frame(table(unlist(strsplit(colnames(data), split = "-"))))
  subdl <- chrvec2dl(colnames(data))

  # ensure the fixed order
  stablets <- dl2ts(subdl)
  subdl <- ts2dl(stablets)

  rpt <- (length(subdl[[lyT]]) * length(subdl[[lySR]]))*2
  fullcond <- as.vector(checkref[checkref[,2]==rpt,1])

  skip_layer <- c(lySS, lyT, lySR) # the layers which should be maintained

  unfixlayer <- c()
  unfixlayerlevels <- c()
  fixlayer <- c()
  fixlayerlevels <- c()

  i <- 1
  for (i in 1:length(subdl)){
    if (any(i %in% skip_layer)){
      fixlayer <- append(fixlayer, i)
      fixlayerlevels <- append(fixlayerlevels, length(subdl[[i]]))
      next
    } else {
      subdl[[i]] <- subdl[[i]][subdl[[i]] %in% fullcond]
      unfixlayer <- append(unfixlayer, i)
    }
    if (length(subdl[[i]])==0){
      stop("insufficient data. please check the input.", call. = FALSE) # check if the data in other dimensions are sufficient
    } else {
      unfixlayerlevels <- append(unfixlayerlevels, length(subdl[[i]]))
    }
  }

  # make sub vector binary trees for strain and stress
  strain.subdl <- subdl
  strain.subdl[[lySS]] <- subdl[[lySS]][strainID]
  strain.subvbt <- dl2vbt(strain.subdl)

  stress.subdl <- subdl
  stress.subdl[[lySS]] <- subdl[[lySS]][stressID]
  stress.subvbt <- dl2vbt(stress.subdl)

  vbtfull <- dl2vbt(subdl)
  trvsfull <- trvs(vbtfull)

  # build data mapping for convenient export:
  strain.data <- datavisit(data, strain.subvbt)
  stress.data <- datavisit(data, stress.subvbt)

  trvs.time <- prod(unfixlayerlevels)

  dataints <- array(NA, dim = c(fixlayerlevels[fixlayer[lySR]], fixlayerlevels[fixlayer[lyT]], trvs.time))

  # data export dynamically
  rpt2 <- length(strain.data[,1])
  i <- 1
  for (i in 1:rpt2){
    fill <- stress.data[i,4][[1]][which.min(abs(strain.data[i,4][[1]]-eps))] # Amazing mapping!
    locationSR <- as.numeric(stress.data[i,3][[1]][fixlayer[lySR]])
    locationT <- as.numeric(stress.data[i,3][[1]][fixlayer[lyT]])
    dataints.idx <- c(locationSR, locationT, i)
    dataints[dataints.idx] <- fill
  }
  dataints <- apply(dataints, c(1,2), mean)

  # make names
  rownames(dataints) <- subdl[[fixlayer[lySR]]]
  colnames(dataints) <- subdl[[fixlayer[lyT]]]

  # reorder the matrix
  sortT <-as.character(sort(as.numeric(colnames(dataints))))
  dataints <- dataints[, sortT]
  sortSR <-as.character(sort(as.numeric(rownames(dataints))))
  dataints <- dataints[sortSR,]

  result <- dataints
  result <- list("SRT.table"=result, "epsilon"=eps)

  class(result) <- "SR-T.table"
  return(result)
}
