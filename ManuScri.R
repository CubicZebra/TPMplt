##### API4TMZ: API for Thermec Mater-Z tester, complete read the csv data #####

API4TMZ <- function(cd, wd=getwd()){
  if(all(is.character(unlist(cd)))==F){
    stop("input list must double list.", call. = FALSE)
  } else{
    # cd: a double list for conditions
    # wd: the working directory, default value is getwd()

    # make names
    name <- trvs(dl2vbt(cd))
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
    temp_data <- read.csv(paste(wd, sfd, name[1],".csv", sep = ""), header = F)
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

TMZdatainput <- function(cd, wd=getwd(), makeidx=FALSE){
  data <- API4TMZ(cd=cd, wd=wd)
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

T <- c("900", "950", "1000", "1050", "1100", "1150", "1200")
S <- c("0.001", "0.01", "0.1", "1")
D <- c("60%")
cd <- list(T, S, D)

data <- TMZdatainput(cd, "/Data/Org/")
rm(list = c("T", "S", "D"))

##### diminspec: return the variables dimension from data #####

diminspec <- function(data, patterns="[Ss][Tt][Rr]"){

  # input data diagnose:
  if(!is.data.frame(data)){
    warning("input data will be convert to data.frame.", call. = FALSE)
    data <- as.data.frame(data)
  }

  name_vec <- colnames(data)
  dl <- chrvec2dl(name_vec)
  layers <- length(dl)
  patterns <- patterns

  # get the coordinate of stress and strain through regular expression ("[Ss][Tt][Rr]")
  i <- 1
  for (i in 1:layers) {
    if (sum(as.vector(regexpr(patterns, dl[[i]]))) >= 0){
      break
    }
  }

  # get the order between stress and strain through characterized letters ("[Aa][Ii]")
  recheck <- length(dl[[i]])
  if (recheck!=2){
    stop("Please check the varaible name; which should only contain stress and strain.", call. = FALSE)
  }

  j <- 1
  for (j in 1:recheck) {
    if(sum(as.vector(regexpr("[Aa][Ii]", dl[[i]][j]))) >= 0){
      break
    }
  }

  if(j==1){
    k <- 2
  } else {
    k <- 1
  }
  result <- list("layer"=i, "strainID"=j, "stressID"=k)
  class(result) <- "pointer"
  return(result)
}

diminspec(data)

##### epsimprt: auto input stress-strain value by specific epsilon #####

epsinprt <- function(data, eps, lyT, lySR, manual=NULL){
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
    SSptr <- diminspec(data)
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
  result <- list("SRTChart"=result, "epsilon"=eps)

  class(result) <- "SR-T Chart"
  return(result)
}

epsiSRTchart <- epsinprt(data, eps = 0.7, lyT = 2, lySR = 3)

##### SRTprocess: processing the data from the data generated by epsinprt() #####

SRTprocess <- function(x, consfuncPRT=FALSE, lgbase=exp(1), rnd=2){

  # check the input data
  if(class(x)!="SR-T Chart"){
    stop("the input data should be SR-T Chart generated by the function epsinprt()", call. = FALSE)
  }

  # # test section
  # base <- exp(1)
  # x <- epsiSRTchart
  # eigenPRT=TRUE

  eps <- x$epsilon
  x <- x$SRTChart

  # T1 to T in function
  logbase <- lgbase

  dims <- dim(x)
  SR <- as.numeric(rownames(x))
  Tem1 <- 1/as.numeric(colnames(x))
  lgSR <- log(SR, base = logbase)

  i <- 1
  temp_vec <- c()
  for (i in 1:dims[2]) {
    b <- as.vector(log(x[,i], base = logbase))
    a <- lgSR
    slope <- as.numeric(lm(b~I(a^1))$coefficients[2])
    temp_vec <- append(temp_vec, slope)
  }

  m.SRSensitivity <- temp_vec # vector; since m ~ f(T)
  n1.StressInd <- 1/mean(m.SRSensitivity)
  #eta.DissiEfficiFactor <- (2*m.SRSensitivity)/(1+m.SRSensitivity) # vector; eta ~ f(T)

  i <- 1
  temp_vec <- c()
  for (i in 1:dims[2]) {
    b <- as.vector(x[,i])
    a <- lgSR
    slope <- as.numeric(lm(b~I(a^1))$coefficients[2])
    temp_vec <- append(temp_vec, slope)
  }

  beta.StressInd <- mean(1/temp_vec)
  alpha.MateCons <- beta.StressInd/n1.StressInd # unit: MPa^(-1)

  lgsinhx <- log(sinh(alpha.MateCons*x), base = logbase) # make the log(sinh[alpha*sigma]) values

  i <- 1
  temp_vec <- c()
  for (i in 1:dims[1]) {
    b <- as.vector(lgsinhx[i,])
    a <- Tem1
    slope <- as.numeric(lm(b~I(a^1))$coefficients[2])
    temp_vec <- append(temp_vec, slope)
  }

  K.MetaCons <- temp_vec

  i <- 1
  temp_vec <- c()
  for (i in 1:dims[2]) {
    b <- lgSR
    a <- as.vector(lgsinhx[,i])
    slope <- as.numeric(lm(b~I(a^1))$coefficients[2])
    temp_vec <- append(temp_vec, slope)
  }

  Hem1.MetaCons <- temp_vec

  R.Cons <- 8.314
  Q.Temp <- R.Cons *mean(K.MetaCons) *mean(Hem1.MetaCons)

  # traverse dims[1]*dims[2] times using the built-in order in R
  i <- 1
  j <- 1
  Zmat <- matrix(NA, nrow = dims[1], ncol = dims[2])
  for (i in 1:dims[1]) {
    for (j in 1:dims[2]) {
      Tcond <- as.numeric(colnames(lgsinhx)[j])
      SRcond <- as.numeric(rownames(lgsinhx)[i])
      temp <- exp(Q.Temp/(R.Cons*Tcond))
      Zmat[i,j] <- SRcond*temp
    }
  }

  Z <- as.numeric(as.vector(Zmat))
  lnZ <- log(Z, base = logbase)
  a1 <- as.numeric(lgsinhx)

  n.PowerValue <- as.numeric(lm(lnZ~I(a1^1))$coefficients[2])

  a2 <- as.numeric(as.vector((sinh(alpha.MateCons*x))^n.PowerValue))
  A.MetaCons <- as.numeric(lm(Z~I(a2^1))$coefficients[2])

  Q.ActivEnerg <- Q.Temp*(-0.001) # unit: kJ/mol

  if(consfuncPRT){
    print(paste("the constitutive equation in strain ", round(eps, rnd), " is: sigma=", "(1/", round(alpha.MateCons, rnd), ")ln{(Z/", A.MetaCons, ")^(1/", round(n.PowerValue, rnd),
                ")+[(Z/", A.MetaCons, ")^(2/", round(n.PowerValue, rnd), ")", "+1]^(1/2)}", sep =""))
  }

  # package all the coefficients
  MaterialCoefficients <- list("m.StrainRateSensitivity"=m.SRSensitivity, "n1.StressIndex"=n1.StressInd, "beta.StressIndex"=beta.StressInd,
                               "alpha.MaterialConstant"=alpha.MateCons, "Q.ActivatingEnergy"=Q.ActivEnerg, "n.PowerValue"=n.PowerValue,
                               "A.MaterialConstant"= A.MetaCons, "epsilon.strain"=eps, "base"=logbase)

  i <- 1
  fitMat <- matrix(NA, nrow = 3, ncol = dims[2])
  for (i in 1:dims[2]) {
    b <- as.vector(log(x[,i], base = logbase))
    a <- lgSR
    coeffivec <- as.vector(lm(b~I(a^1)+I(a^2)+I(a^3))$coefficients)
    fitMat[,i] <- c(coeffivec[2], coeffivec[3], coeffivec[4])
  }

  temptable <- matrix(NA, nrow = dims[1], ncol = dims[2])

  i <- 1
  j <- 1
  for (i in 1:dims[1]) { # max i = 4
    for (j in 1:dims[2]) { # max j = 7
      bcd <- fitMat[,j] # d3 vector
      lgstr <- log(as.numeric(rownames(lgsinhx)[i]), base = logbase)
      temptable[i,j] <- bcd[1]+2*bcd[2]*lgstr+3*bcd[3]*(lgstr^2)
    }
  }

  etatable <- as.data.frame((2*temptable)/(1-temptable))
  rownames(etatable) <- rownames(lgsinhx)
  colnames(etatable) <- colnames(lgsinhx)

  i <- 1
  temp_vec <- c()
  for (i in 1:dims[2]) {
    b <- temptable[,i]/(temptable[,i] + 1)
    a <- lgSR
    temptable[,i] <- as.numeric(lm(b~I(a^1))$coefficients[2]) + temptable[,i]
  }

  xitable <- temptable
  rownames(xitable) <- rownames(lgsinhx)
  colnames(xitable) <- colnames(lgsinhx)

  # package two tables
  rslttable <- list("SRTtable"=x, "etatable"=etatable, "xitable"=xitable)

  result <- list("MaterialCoefficients"=MaterialCoefficients, "tablelist"=rslttable)
  class(result) <- "TPMdata"
  return(result)
}

TPMdata <- SRTprocess(epsiSRTchart)

##### TPMplot: plot the processing map #####

etatidy <- function(x){

  # input data check
  if(class(x)!="TPMdata"){
    stop("the input data should be TPMdata generated by SRTprocess() function.", call. = FALSE)
  }

  lgbase <- x$MaterialCoefficients$base
  x <- x[2]$tablelist$etatable

  T <- as.numeric(colnames(x))
  SR <- as.numeric(rownames(x))
  lgSR <- log(SR, base = lgbase)
  rpt <- length(lgSR)*length(T)

  i <- 1
  j <- 1
  temp_vec1 <- c()
  temp_vec2 <- c()
  temp_vec3 <- c()
  for(i in 1:length(lgSR)){
    for (j in 1:length(T)) {
      temp_vec1 <- append(temp_vec1, T[j])
      temp_vec2 <- append(temp_vec2, lgSR[i])
      temp_vec3 <- append(temp_vec3, x[i,j])
    }
  }
  M <- cbind(temp_vec1, temp_vec2, temp_vec3)
  colnames(M) <- c("Temperature", "lgStrainRate", "Value")
  return(M)
}

xitidy <- function(x){

  # input data check
  if(class(x)!="TPMdata"){
    stop("the input data should be TPMdata generated by SRTprocess() function.", call. = FALSE)
  }

  lgbase <- x$MaterialCoefficients$base
  x <- x[2]$tablelist$xitable

  T <- as.numeric(colnames(x))
  SR <- as.numeric(rownames(x))
  lgSR <- log(SR, base = lgbase)
  rpt <- length(lgSR)*length(T)

  i <- 1
  j <- 1
  temp_vec1 <- c()
  temp_vec2 <- c()
  temp_vec3 <- c()
  for(i in 1:length(lgSR)){
    for (j in 1:length(T)) {
      temp_vec1 <- append(temp_vec1, T[j])
      temp_vec2 <- append(temp_vec2, lgSR[i])
      temp_vec3 <- append(temp_vec3, x[i,j])
    }
  }
  M <- cbind(temp_vec1, temp_vec2, temp_vec3)
  colnames(M) <- c("Temperature", "lgStrainRate", "Value")
  return(M)
}

etaM <- etatidy(TPMdata)
xiM <- xitidy(TPMdata)

MakeGrid <- function(x, seqby=80){

  # input data check
  if(class(x)!="TPMdata"){
    stop("the input data should be TPMdata generated by SRTprocess() function.", call. = FALSE)
  }

  M <- etatidy(x)

  GridT <- seq(min(M[,1]), max(M[,1]), by=(max(M[,1]-min(M[,1])))/seqby)
  GridlgSR <- seq(min(M[,2]), max(M[,2]), by=(max(M[,2]-min(M[,2])))/seqby)
  predictor <- expand.grid(T=GridT, lgSR=GridlgSR)

  len <- length(predictor[,1])
  value <- rep(1, len)
  result <- cbind(predictor, value)

  return(result)
}

predictor <- MakeGrid(TPMdata)

# prediction by Bayesian Methods
BayesianPlot <- function(x, notelocation, seqby=80, stochastep=FALSE){

  sqby <- seqby
  BPconfid <- stochastep
  # function to get Bayesian posteriori
  BayesianModel <- function(x, seqby=seqby){
    # input data check
    if(class(x)!="TPMdata"){
      stop("the input data should be TPMdata generated by SRTprocess() function.", call. = FALSE)
    }
    etaM <- etatidy(x)
    xiM <- xitidy(x)

    etaM_BP <- MVNBayesian::MVN_BayesianPosteriori(etaM, pri_var = var(etaM)) # need modif for Wishart dis; book P.40
    xiM_BP <- MVNBayesian::MVN_BayesianPosteriori(xiM, pri_var = var(xiM)) # need modif for Wishart dis

    stp <- seqby
    predictor <- MakeGrid(x, stp)
    len <- length(predictor[,1])

    etavec <- c()
    xivec <- c()
    for (i in 1:len) {
      ptr <- as.vector(unlist(predictor[i,]))
      etavec <- append(etavec, MVNBayesian::MVN_FConditional(etaM_BP, 3, ptr)$mean)
      xivec <- append(xivec, MVNBayesian::MVN_FConditional(xiM_BP, 3, ptr)$mean)
    }

    etagroup <- rep("eta.BP", len)
    xigroup <- rep("xi.BP", len)

    etadata <- cbind(predictor[,1:2], etagroup, etavec)
    xidata <- cbind(predictor[,1:2], xigroup, xivec)

    colname <- c("T", "lgSR", "group", "value")
    colnames(etadata) <- colname
    colnames(xidata) <- colname

    if(BPconfid){
      confidtemp <- mvtnorm::rmvnorm(400, mean = etaM_BP$mean, sigma = etaM_BP$var)
      len2 <- length(confidtemp[,1])

      confiddata <- as.data.frame(matrix(NA, nrow = len2, ncol = 4))
      confiddata[,1:2] <- confidtemp[,1:2]
      confiddata[,4] <-confidtemp[,3]
      confiddata[,3] <- rep("contour", len2)
      colnames(confiddata) <- colname
      result <- as.data.frame(rbind(etadata, xidata, confiddata))
    } else {
      result <- as.data.frame(rbind(etadata, xidata))
    }
    return(result)
  }

  # output result of Bayesian inference for eta and xi
  BPplot <- function(x){
    # Plot Theme
    theme_zc <- theme(plot.title = element_text(size = 12.5, face = "bold"),
                      axis.text = element_text(size = 11), axis.title = element_text(size = 12, face = "bold"),
                      axis.text.y = element_text(margin = margin(0, 6, 0, 0, "pt")), axis.text.x = element_text(margin = margin(6, 0, 0, 0, "pt")),
                      axis.ticks = element_line(size = 1), axis.ticks.length = unit(-0.16, "cm"),
                      panel.background = element_rect(color = "black", size = 1))
    legendtheme_zc <- theme(legend.key.size = unit(0.18, "inches"), legend.key.height = unit(0.18, "inches"),
                            legend.text = element_text(face = "bold"), legend.title = element_text(face = "bold"))
    my_theme <- theme_zc + legendtheme_zc

    # Plot Process
    layer1 <- ggplot(subset(x, group=="xi.BP"), aes(T, lgSR, z = value))
    layer1 <- layer1 + geom_raster(aes(fill = value), interpolate = TRUE) + scale_fill_gradientn(name="xi.BP", colours=c("red", "#FFFFFFFF", "green"))
    layer2 <- layer1 + geom_contour(data = subset(x, group=="eta.BP"), aes(T, lgSR, z=value, colour = stat(level)), inherit.aes = TRUE)
    layer2 <- layer2 + my_theme + xlab("Temperature (Celsius)") + ylab("LogStrainRate (log(s^(-1)))") + labs(color="eta.BP")

    if(BPconfid==TRUE){
      layer3 <- layer2 + geom_density_2d(data = subset(x, group=="contour"), aes(T, lgSR), inherit.aes = TRUE, color="orange")
      return(layer3)
    } else {
      return(layer2)
    }
  }

  SR <- x$MaterialCoefficients$epsilon.strain
  locx <- notelocation[1]
  locy <- notelocation[2]

  BP <- BayesianModel(x, seqby = seqby)
  plotBP <- BPplot(BP) + annotate("text", x=locx, y=locy, label=paste("Strain: ", SR, sep = ""), colour = "black")
  return(plotBP)
}

epsiSRTchart <- epsinprt(data, eps = 0.4, lyT = 2, lySR = 3)
TPMdata <- SRTprocess(epsiSRTchart)
BPplot <- BayesianPlot(TPMdata, notelocation = c(930, -0.3))
BPplot

# e <- seq(0.1, 0.9, by = 0.02)
# times <- length(e)
# i <- 1
# for (i in 1:times) {
#   epsiSRTchart <- epsinprt(data, eps = e[i], lyT = 2, lySR = 3)
#   TPMdata <- SRTprocess(epsiSRTchart)
#   BPplot <- BayesianPlot(TPMdata)
#   print(BPplot)
#   Sys.sleep(5)
# }


# SVR Method
SVRPlot <- function(x, notelocation, seqby=80){

  sqby <- seqby
  # function to get Bayesian posteriori
  SVRModel <- function(x, seqby=sqby){
    # input data check
    if(class(x)!="TPMdata"){
      stop("the input data should be TPMdata generated by SRTprocess() function.", call. = FALSE)
    }

    etaM <- etatidy(x)
    xiM <- xitidy(x)

    stp <- seqby
    predictor <- MakeGrid(x, stp)
    len <- length(predictor[,1])
    len1 <- length(etaM[,1])

    etatable <- matrix(NA, nrow = len1, ncol = 4)
    xitable <- etatable
    etatable <- as.data.frame(etaM[,1:2])
    xitable <- as.data.frame(xiM[,1:2])

    vartable <- as.data.frame(predictor[,-3])
    etavalue <- as.vector(etaM[,3])
    xivalue <- as.vector(xiM[,3])

    modeleta <- e1071::svm(etatable, etavalue, kernel = "radial", type = "eps")
    modelxi <- e1071::svm(xitable, xivalue, kernel = "radial", type = "eps")

    predeta <- as.vector(predict(modeleta, vartable))
    predxi <- as.vector(predict(modelxi, vartable))

    etaresult <- cbind(vartable, rep("eta.SVR", len), predeta)
    xiresult <- cbind(vartable, rep("xi.SVR", len), predxi)

    colname <- c("T", "lgSR", "group", "value")
    colnames(etaresult) <- colname
    colnames(xiresult) <- colname

    result <- as.data.frame(rbind(etaresult, xiresult))
    return(result)
  }

  SVRplt <- function(x){
    # Plot Theme
    theme_zc <- theme(plot.title = element_text(size = 12.5, face = "bold"),
                      axis.text = element_text(size = 11), axis.title = element_text(size = 12, face = "bold"),
                      axis.text.y = element_text(margin = margin(0, 6, 0, 0, "pt")), axis.text.x = element_text(margin = margin(6, 0, 0, 0, "pt")),
                      axis.ticks = element_line(size = 1), axis.ticks.length = unit(-0.16, "cm"),
                      panel.background = element_rect(color = "black", size = 1))
    legendtheme_zc <- theme(legend.key.size = unit(0.18, "inches"), legend.key.height = unit(0.18, "inches"),
                            legend.text = element_text(face = "bold"), legend.title = element_text(face = "bold"))
    my_theme <- theme_zc + legendtheme_zc

    # Plot Process
    layer1 <- ggplot(subset(x, group=="xi.SVR"), aes(T, lgSR, z = value))
    layer1 <- layer1 + geom_raster(aes(fill = value), interpolate = TRUE) + scale_fill_gradientn(name="xi.SVR", colours=c("red", "#FFFFFFFF", "green"))
    layer2 <- layer1 + geom_contour(data = subset(x, group=="eta.SVR"), aes(T, lgSR, z=value, colour = stat(level)), inherit.aes = TRUE)
    layer2 <- layer2 + my_theme + xlab("Temperature (Celsius)") + ylab("LogStrainRate (log(s^(-1)))") + labs(color="eta.SVR")

    return(layer2)
  }

  SR <- x$MaterialCoefficients$epsilon.strain
  locx <- notelocation[1]
  locy <- notelocation[2]

  svrmod <- SVRModel(x)
  result <- SVRplt(svrmod) + annotate("text", x=locx, y=locy, label=paste("Strain: ", SR, sep = ""), colour = "black")
  return(result)
}

epsiSRTchart <- epsinprt(data, eps = 0.16, lyT = 2, lySR = 3)
TPMdata <- SRTprocess(epsiSRTchart)
tt1 <- SVRPlot(TPMdata, notelocation = c(930, -0.3))
tt1

# e <- seq(0.1, 0.9, by = 0.02)
# times <- length(e)
# i <- 1
# for (i in 1:times) {
#   epsiSRTchart <- epsinprt(data, eps = e[i], lyT = 2, lySR = 3)
#   TPMdata <- SRTprocess(epsiSRTchart)
#   SVRfig <- SVRPlot(TPMdata, notelocation = c(930, -0.3))
#   print(SVRfig)
#   Sys.sleep(5)
# }


