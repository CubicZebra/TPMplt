#' Basic fitting function for stress-strain curve.
#'
#' @param x Numeric vector as independent variable.
#' @param y Numeric vector as dependent variable.
#' @param sig Signif to control masking coefficients.
#'
#' @import stats
#' @return A list contains independent variable and fitted dependent variable over the maximum value.
#' @export basicPF
#'
#' @examples
#' x <- TPMdata[,1]
#' y <- TPMdata[,2]
#' m <- basicPF(x, y, 0.99)
#  print(m)
#'
#' @keywords internal
basicPF <- function(x, y, sig){

  if(length(x) != length(y)){
    stop("Unequal length between x and y inputted.", call. = FALSE)
  }

  x <- as.vector(x)
  y <- as.vector(y)

  rpt <- length(x)
  idx <- which.max(y)
  x1 <- x[idx:rpt]
  y1 <- y[idx:rpt]

  lmmod <- lm(y1~I(x1^1)+I(x1^2)+I(x1^3)+I(x1^4)+I(x1^5)+I(x1^6)+I(x1^7)) # stats
  slt <- as.vector(summary(lmmod)$coefficients[,4] < sig)
  coef_vec <- slt*as.vector(summary(lmmod)$coefficients[,1])

  for (i in idx:rpt) {
    xi <- x[i]
    xi_vec <- c(1, xi, xi^2, xi^3, xi^4, xi^5, xi^6, xi^7)
    fill <- as.numeric(coef_vec %*% xi_vec)
    y[i] <- fill
  }
  result <- list(x=x, y=y)
  return(result)
}



#' Fit all flow stress values in high strain conditions
#'
#' @param x A data frame with \code{\link[VBTree:VBTree-package]{VBTree}} style. Pay attention, all factors in column names
#' should be separated by "-" symbol, and factors for temperatures and strain rates should be saved in pure numeric style.
#' @param Manu An integer vector with the length of 3 where the 1st element denotes the layer for Stress and Strain,
#' the 2nd and 3rd elements represent the levels for Strain and Stress, respectively. The default setting is NULL, which
#' can call the function \code{\link[TPMplt:lyIDdetector]{lyIDdetector}} for automatical completion this vector.
#' @param SignifCtrl A numeric value from 0 to 1, to determine remaining coefficients for the function \code{\link[stats:lm]{lm}}.
#' The default value is 0.95.
#'
#' @import VBTree
#' @return A \code{\link[VBTree:VBTree-package]{VBTree}} style data frame with fitted values for flow stress in high strain conditions.
#' @export AllPF
#'
#' @examples
#' SSplots(TPMdata, 2, mfrow=c(2, 2))
#' x <- AllPF(TPMdata, SignifCtrl = 0.98)
#' SSplots(x, 2, mfrow=c(2, 2))
#' @keywords AllPF
AllPF <- function(x, Manu=NULL, SignifCtrl=0.95) {
  # test section
  # x <- TPMdata
  # Manu = NULL

  if (is.null(Manu)) {
    t <- lyIDdetector(x)
    ctrl_vec <- c(t[[1]], t[[2]], t[[3]]) # lySS, strainLV, stressLV
  } else {
    ctrl_vec <- Manu
  }

  m <- chrvec2dl(colnames(x))
  m <- arr2vbt(dl2arr(m))

  Strain_ptr <- rep(-1, length(m[[2]]))
  Stress_ptr <- Strain_ptr

  Strain_ptr[ctrl_vec[1]] <- ctrl_vec[2]
  Stress_ptr[ctrl_vec[1]] <- ctrl_vec[3]

  Strain_vec <- as.vector(vbt2arr(vbtsub(m, Strain_ptr)))
  Stress_vec <- as.vector(vbt2arr(vbtsub(m, Stress_ptr)))

  rpt <- length(Strain_vec)

  for (i in 1:rpt) {
    x1 <- x[,Strain_vec[i]]
    y1 <- x[,Stress_vec[i]]
    rplc <- basicPF(x1, y1, sig = SignifCtrl) # Modi
    x[,Stress_vec[i]] <- rplc[[2]]
  }

  result <- x
  return(result)
}



#' Intergal area function.
#'
#' @param x Numeric vector as independent variable.
#' @param y Numeric vector as dependent variable.
#'
#' @return A x-dependent function for intergal area.
#' @export Dvec
#'
#' @examples
#' x <- TPMdata[,1]
#' y <- TPMdata[,2]
#' Dvec(x, y)
#' @keywords internal
Dvec <- function(x, y){

  if(length(x) != length(y)){
    stop("Unequal length between x and y inputted.", call. = FALSE)
  }

  x <- as.vector(x)
  y <- as.vector(y)

  rpt <- length(x)

  temp_vec <- c()
  a <- 0
  for (i in 2:rpt) {
    a <- a + abs(x[i]-x[i-1]) * y[i]
    temp_vec <- append(temp_vec, a)
  }
  temp_vec <- c(0, temp_vec)

  result <- temp_vec
  return(result)
}



#' Temperature correction function
#'
#' @param x A data frame with \code{\link[VBTree:VBTree-package]{VBTree}} style. Pay attention, all factors in column names
#' should be separated by "-" symbol, and factors for temperatures and strain rates should be saved in pure numeric style.
#' @param ly_SR An integer to specify the layer for strain rate attribute in the vector binary tree.
#' @param ly_T An integer to specify the layer for temperature attribute in the vector binary tree.
#' @param C Numeric value for heat capacity constant of the material, the unit is J/(kg*K) in SI base.
#' @param rho Numeric value for density of the materials, the unit is kg/m^3 in SI base.
#' @param logbase A numeric value to specify the base of all logarithm calculations during building model. The default value
#' uses exp(1).
#' @param CorrCons A coefficient from 0 to 1 to specify thermal efficiency. Defualt value is NULL means this coefficient will
#' be selected dynamically.
#' @param CorrSR A coefficient corresponding to strain conditions to build a referential model for temperature correction.
#' Default value NULL means the program will detect the maximum raw stress values then select a proper strain condition. Referential
#' model will be built on the basis of that.
#' @param Manu An integer vector with the length of 3 where the 1st element denotes the layer for Stress and Strain,
#' the 2nd and 3rd elements represent the levels for Strain and Stress, respectively. The default setting is NULL, which
#' can call the function \code{\link[TPMplt:lyIDdetector]{lyIDdetector}} for automatical completion this vector.
#'
#' @import VBTree
#' @return A \code{\link[VBTree:VBTree-package]{VBTree}} style data frame with temperature-corrected values for flow stress
#' in high strain conditions.
#' @export TCorrect
#'
#' @examples
#' # Using the parameters of steel as example
#' x <- TCorrect(TPMdata, 3, 2, 510.7896, 8050, CorrCons = 0.9)
#'
#' SSplots(x, 2, mfrow=c(2, 2))
#' @keywords TCorrect AllPF
TCorrect <- function(x, ly_SR, ly_T, C, rho, logbase=exp(1), CorrCons=NULL, CorrSR=NULL, Manu=NULL){

  # # test section
  # x <- TPMdata
  # ly_SR <- 3
  # ly_T <- 2
  # C = 510.7896
  # rho = 8050
  # logbase <- exp(1)
  # CorrCons=0.9
  # CorrConsSR
  # Manu = NULL

  if (is.null(Manu)) {
    t <- lyIDdetector(x)
    ctrl_vec <- c(t[[1]], t[[2]], t[[3]]) # lySS, strainLV, stressLV
  } else {
    ctrl_vec <- Manu
  }

  raw_x <-x
  x <- AllPF(x)

  m <- chrvec2dl(colnames(x))
  m <- arr2vbt(dl2arr(m))
  mdl <- vbt2dl(m)

  Strain_ptr <- rep(-1, length(m[[2]]))
  Stress_ptr <- Strain_ptr

  Strain_ptr[ctrl_vec[1]] <- ctrl_vec[2]
  Stress_ptr[ctrl_vec[1]] <- ctrl_vec[3]

  Strain_vec <- as.vector(vbt2arr(vbtsub(m, Strain_ptr)))
  Stress_vec <- as.vector(vbt2arr(vbtsub(m, Stress_ptr)))

  if (is.null(CorrSR)){
    idx <- mean(as.vector(apply(raw_x[,Stress_vec], 2, which.max)))
    idx <- round(idx, 1)

    refSR <- mean(as.numeric(raw_x[,Strain_vec][idx,]))
    refSRT <- epsExtract(raw_x, refSR, lyT = ly_T, lySR=ly_SR)
  } else {
    refSRT <- epsExtract(raw_x, CorrSR, lyT = ly_T, lySR=ly_SR)
  }

  refDMM <- DMMprocess(refSRT, ConsFunc = TRUE, lgbase = logbase)
  bs <- refDMM[[1]][[5]]

  rpt <- length(Strain_vec)
  l <- length(x[,Stress_vec[1]])

  for (i in 1:rpt) {
    SRchr <- chrvec2dl(Strain_vec[i])[[ly_SR]]
    SR <- as.numeric(SRchr)

    Tchr <- chrvec2dl(Strain_vec[i])[[ly_T]]
    T <- as.numeric(Tchr) + 273.15

    lmid <- which(mdl[[ly_SR]] == SRchr)
    temp_lm <- refDMM.LM[[lmid]]

    if (is.null(CorrCons)){
      eta_e <- 0.316*log(SR, base = 10) + 0.95
    } else {
      eta_e <- CorrCons
    }

    x1 <- x[,Strain_vec[i]]
    y1 <- x[,Stress_vec[i]]
    D_vec <- Dvec(x1, y1) # length = l, 3939

    Delta_T <- D_vec*eta_e*1000000/(C*rho) # length = l, 3939; MPa to SI base Temperature

    lgSRp <- (log(SR, base = bs) + refDMM[[1]][[4]]*1000/(8.314*T) - log(refDMM[[1]][[1]], base = bs))/refDMM[[1]][[7]]
    SRp <- bs^as.numeric(lgSRp)
    Delta_sig <- abs(SRp - SR)

    Correct_Sig <- Delta_T * Delta_sig
    x[,Stress_vec[i]] <- y1 + Correct_Sig
  }

  result <- x
  return(result)
}
