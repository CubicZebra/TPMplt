library(VBTree)
library(TPMplt)
library(utils)

testfunc <- function(m){
  # test section
  lgbase <- exp(1)
  epstable <- epsExtract(TPMdata, m, 2, 3)
  x <- epstable
  eigenPRT=TRUE
  consfuncPRT=TRUE
  rnd=2
  InteractMode=TRUE
  legendcex = 0.65
  legendloc ="bottomright"

  eps <- x$epsilon
  x <- x$SRT.table

  # T1 to T in function
  logbase <- lgbase

  dims <- dim(x)
  SR <- as.numeric(rownames(x))
  Tem1 <- rev(1/(as.numeric(colnames(x)) + 273.15)) # Kelvin
  lgSR <- log(SR, base = logbase)
  clrvec <- RColorBrewer::brewer.pal(9, "Set1")

  i <- 1
  scatter_mat <- matrix(NA, nrow = dims[2], ncol = dims[1])
  scatter_mat1 <- scatter_mat
  lmlist <- list(list())[rep(1,dims[2])]
  lmlist1 <- lmlist
  for (i in 1:dims[2]) {
    scatter_mat[i,] <- as.numeric(log(x[,i], base = logbase))
    scatter_mat1[i,] <- as.numeric(x[,i])
    a <- lgSR
    lmlist[[i]] <- lm(scatter_mat[i,]~I(a^1))
    lmlist1[[i]] <- lm(scatter_mat1[i,]~I(a^1))
  }

  #Plot1 output section
  if(InteractMode == TRUE){
    cat("Show fitting plot of log_flow_stress vs. log_strain_rate?")
    ptr <- select.list(c("Yes", "No"))
    if(ptr == "Yes"){
      legendvec <- colnames(x)
      yscale <- c(min(unlist(scatter_mat)), max(unlist(scatter_mat)))
      xscale <- c(min(lgSR), max(lgSR))

      labx <- expression(paste("ln(",ring(epsilon), "/s"^-1, ")", sep = ""))
      laby <- expression(paste("ln(",sigma, "/MPa)", sep = ""))

      for(i in 1:dims[2]){
        plot(lgSR, scatter_mat[i,], type = "p", col = clrvec[i], pch=16, xlim = xscale, ylim = yscale, xlab = labx, ylab = laby)
        lines(x=lgSR, y=predict(lmlist[[i]]), col = clrvec[i], xlim = xscale, ylim = yscale, xlab = labx, ylab = laby)
        if (i == dims[2]){
          legend(legendloc,legend = legendvec, fill = clrvec[1:dims[2]], cex = legendcex, bg = "transparent", box.lty = 0)
          par(new=FALSE)
        } else {
          par(new=TRUE)
        }
      }
    }
    invisible(readline(prompt="Press [enter] to continue"))
  }

  #Plot2 output section
  if(InteractMode == TRUE){
    cat("Show fitting plot of flow_stress vs. log_strain_rate?")
    ptr <- select.list(c("Yes", "No"))
    if(ptr == "Yes"){
      legendvec <- colnames(x)
      yscale <- c(min(unlist(scatter_mat1)), max(unlist(scatter_mat1)))
      xscale <- c(min(lgSR), max(lgSR))

      labx <- expression(paste("ln(",ring(epsilon), "/s"^-1, ")", sep = ""))
      laby <- expression(paste(sigma, "/MPa", sep = ""))

      for(i in 1:dims[2]){
        plot(lgSR, scatter_mat1[i,], type = "p", col = clrvec[i], pch=16, xlim = xscale, ylim = yscale, xlab = labx, ylab = laby)
        lines(x=lgSR, y=predict(lmlist1[[i]]), col = clrvec[i], xlim = xscale, ylim = yscale, xlab = labx, ylab = laby)
        if (i == dims[2]){
          legend(legendloc,legend = legendvec, fill = clrvec[1:dims[2]], cex = legendcex, bg = "transparent", box.lty = 0)
          par(new=FALSE)
        } else {
          par(new=TRUE)
        }
      }
      invisible(readline(prompt="Press [enter] to continue"))
    }
  }

  temp_vec <- c()
  temp_vec1 <- temp_vec
  for (i in 1:dims[2]) {
    a <- as.numeric(lmlist[[i]][[1]][2]) # Extract slope
    a1 <- as.numeric(lmlist1[[i]][[1]][2])
    temp_vec <- append(temp_vec, a)
    temp_vec1 <- append(temp_vec1, a1)
  }

  n.StressInd <- mean(1/temp_vec)
  beta.StressInd <- mean(1/temp_vec1)
  alpha.StressInd <- beta.StressInd/n.StressInd # unit: MPa^(-1)

  sinhax <- sinh(alpha.StressInd*x)
  lgsinhax <- log(sinhax, base = logbase)

  scatter_mat <- matrix(NA, nrow = dims[2], ncol = dims[1])
  lmlist <- list(list())[rep(1,dims[2])]
  for (i in 1:dims[2]) {
    scatter_mat[i,] <- as.numeric(lgsinhax[,i])
    a <- lgSR
    lmlist[[i]] <- lm(scatter_mat[i,]~I(a^1))
  }

  #Plot3 output section
  if(InteractMode == TRUE){
    cat("Show fitting plot of log[sinh(alpha*flow_stress)] vs. log_strain_rate?")
    ptr <- select.list(c("Yes", "No"))
    if(ptr == "Yes"){
      legendvec <- colnames(x)
      yscale <- c(min(unlist(scatter_mat)), max(unlist(scatter_mat)))
      xscale <- c(min(lgSR), max(lgSR))

      labx <- expression(paste("ln(",ring(epsilon), "/s"^-1, ")", sep = ""))
      laby <- expression(paste("ln[sinh(", alpha, sigma, ")]", sep = ""))

      for(i in 1:dims[2]){
        plot(lgSR, scatter_mat[i,], type = "p", col = clrvec[i], pch=16, xlim = xscale, ylim = yscale, xlab = labx, ylab = laby)
        lines(x=lgSR, y=predict(lmlist[[i]]), col = clrvec[i], xlim = xscale, ylim = yscale, xlab = labx, ylab = laby)
        if (i == dims[2]){
          legend(legendloc,legend = legendvec, fill = clrvec[1:dims[2]], cex = legendcex, bg = "transparent", box.lty = 0)
          par(new=FALSE)
        } else {
          par(new=TRUE)
        }
      }
      invisible(readline(prompt="Press [enter] to continue"))
    }
  }

  temp_vec <- c()
  for (i in 1:dims[2]) {
    a <- as.numeric(lmlist[[i]][[1]][2])
    temp_vec <- append(temp_vec, a)
  }

  N.StressInd <- mean(1/temp_vec)

  scatter_mat <- matrix(NA, nrow = dims[1], ncol = dims[2])
  T_1 <- 1/(as.numeric(colnames(x)) + 273.15) # Convert to Kelvin
  lmlist <- list(list())[rep(1,dims[1])]
  for (i in 1:dims[1]) {
    scatter_mat[i,] <- as.numeric(lgsinhax[i,])
    a <- T_1
    lmlist[[i]] <- lm(scatter_mat[i,]~I(a^1))
  }

  #Plot4 output section
  if(InteractMode == TRUE){
    cat("Show fitting plot of log[sinh(alpha*flow_stress)] vs. 1/T(K^-1)?")
    ptr <- select.list(c("Yes", "No"))
    if(ptr == "Yes"){
      legendvec <- rownames(x)
      yscale <- c(min(unlist(scatter_mat)), max(unlist(scatter_mat)))
      xscale <- c(min(T_1), max(T_1))

      labx <- "1/T(K^-1)"
      laby <- expression(paste("ln[sinh(", alpha, sigma, ")]", sep = ""))

      for(i in 1:dims[1]){
        plot(T_1, scatter_mat[i,], type = "p", col = clrvec[i], pch=16, xlim = xscale, ylim = yscale, xlab = labx, ylab = laby)
        lines(x=T_1, y=predict(lmlist[[i]]), col = clrvec[i], xlim = xscale, ylim = yscale, xlab = labx, ylab = laby)
        if (i == dims[1]){
          legend(legendloc,legend = legendvec, fill = clrvec[1:dims[2]], cex = legendcex, bg = "transparent", box.lty = 0)
          par(new=FALSE)
        } else {
          par(new=TRUE)
        }
      }
      invisible(readline(prompt="Press [enter] to continue"))
    }
  }

  temp_vec <- c()
  for (i in 1:dims[1]) {
    a <- as.numeric(lmlist[[i]][[1]][2])
    temp_vec <- append(temp_vec, a)
  }

  K <- mean(temp_vec)
  Q <- K * N.StressInd * 8.314 # R Unit: J/(mol*K)

  print("eeee")
}

# testfunc(0.3)


# test section
lgbase <- exp(1)
epstable <- epsExtract(TPMdata, 0.7, 2, 3)
x1 <- epstable

x <- x1
x[[1]][,c(1:5)] <- x1[[1]][,c(3:7)]
x[[1]][1,1] <- 101.8
x[[1]][,6] <- x1[[1]][,1]
x[[1]][,7] <- c(8.464, 17.471, 53.144, 83.843)

eigenPRT=TRUE
consfuncPRT=TRUE
rnd=2
InteractMode=TRUE
legendcex = 0.65
legendloc = "bottomright"

eps <- x$epsilon
x <- x$SRT.table

# T1 to T in function
logbase <- lgbase

dims <- dim(x)
SR <- as.numeric(rownames(x))
Tem1 <- rev(1/(as.numeric(colnames(x)) + 273.15)) # Kelvin
lgSR <- log(SR, base = logbase)
clrvec <- RColorBrewer::brewer.pal(9, "Set1")

i <- 1
scatter_mat <- matrix(NA, nrow = dims[2], ncol = dims[1])
scatter_mat1 <- scatter_mat
lmlist <- list(list())[rep(1,dims[2])]
lmlist1 <- lmlist
for (i in 1:dims[2]) {
  scatter_mat[i,] <- as.numeric(log(x[,i], base = logbase))
  scatter_mat1[i,] <- as.numeric(x[,i])
  a <- lgSR
  lmlist[[i]] <- lm(scatter_mat[i,]~I(a^1))
  lmlist1[[i]] <- lm(scatter_mat1[i,]~I(a^1))
}

#Plot output section
if(FALSE){
  cat("Show fitting plot of log_flow_stress vs. log_strain_rate?")
  ptr <- select.list(c("Yes", "No"))
  if(ptr == "Yes"){
    legendvec <- colnames(x)
    yscale <- c(min(unlist(scatter_mat)), max(unlist(scatter_mat)))
    xscale <- c(min(lgSR), max(lgSR))

    labx <- expression(paste("ln(",ring(epsilon), "/s"^-1, ")", sep = ""))
    laby <- expression(paste("ln(",sigma, "/MPa)", sep = ""))

    for(i in 1:dims[2]){
      plot(lgSR, scatter_mat[i,], type = "p", col = clrvec[i], pch=16, xlim = xscale, ylim = yscale, xlab = labx, ylab = laby)
      lines(x=lgSR, y=predict(lmlist[[i]]), col = clrvec[i], xlim = xscale, ylim = yscale, xlab = labx, ylab = laby)
      if (i == dims[2]){
        legend(legendloc,legend = legendvec, fill = clrvec[1:dims[2]], cex = legendcex, bg = "transparent", box.lty = 0)
        par(new=FALSE)
      } else {
        par(new=TRUE)
      }
    }
  }
  invisible(readline(prompt="Press [enter] to continue"))
}

if(FALSE){
  cat("Show fitting plot of flow_stress vs. log_strain_rate?")
  ptr <- select.list(c("Yes", "No"))
  if(ptr == "Yes"){
    legendvec <- colnames(x)
    yscale <- c(min(unlist(scatter_mat1)), max(unlist(scatter_mat1)))
    xscale <- c(min(lgSR), max(lgSR))

    labx <- expression(paste("ln(",ring(epsilon), "/s"^-1, ")", sep = ""))
    laby <- expression(paste(sigma, "/MPa", sep = ""))

    for(i in 1:dims[2]){
      plot(lgSR, scatter_mat1[i,], type = "p", col = clrvec[i], pch=16, xlim = xscale, ylim = yscale, xlab = labx, ylab = laby)
      lines(x=lgSR, y=predict(lmlist1[[i]]), col = clrvec[i], xlim = xscale, ylim = yscale, xlab = labx, ylab = laby)
      if (i == dims[2]){
        legend(legendloc,legend = legendvec, fill = clrvec[1:dims[2]], cex = legendcex, bg = "transparent", box.lty = 0)
        par(new=FALSE)
      } else {
        par(new=TRUE)
      }
    }
    invisible(readline(prompt="Press [enter] to continue"))
  }
}

temp_vec <- c()
temp_vec1 <- temp_vec
for (i in 1:dims[2]) {
  a <- as.numeric(lmlist[[i]][[1]][2]) # Extract slope
  a1 <- as.numeric(lmlist1[[i]][[1]][2])
  temp_vec <- append(temp_vec, a)
  temp_vec1 <- append(temp_vec1, a1)
}

n.StressInd <- mean(1/temp_vec)
beta.StressInd <- mean(1/temp_vec1)
alpha.StressInd <- beta.StressInd/n.StressInd # unit: MPa^(-1)

sinhax <- sinh(alpha.StressInd*x)
lgsinhax <- log(sinhax, base = logbase)

scatter_mat <- matrix(NA, nrow = dims[2], ncol = dims[1])
lmlist <- list(list())[rep(1,dims[2])]
for (i in 1:dims[2]) {
  scatter_mat[i,] <- as.numeric(lgsinhax[,i])
  a <- lgSR
  lmlist[[i]] <- lm(scatter_mat[i,]~I(a^1))
}

if(FALSE){
  cat("Show fitting plot of log[sinh(alpha*flow_stress)] vs. log_strain_rate?")
  ptr <- select.list(c("Yes", "No"))
  if(ptr == "Yes"){
    legendvec <- colnames(x)
    yscale <- c(min(unlist(scatter_mat)), max(unlist(scatter_mat)))
    xscale <- c(min(lgSR), max(lgSR))

    labx <- expression(paste("ln(",ring(epsilon), "/s"^-1, ")", sep = ""))
    laby <- expression(paste("ln[sinh(", alpha, sigma, ")]", sep = ""))

    for(i in 1:dims[2]){
      plot(lgSR, scatter_mat[i,], type = "p", col = clrvec[i], pch=16, xlim = xscale, ylim = yscale, xlab = labx, ylab = laby)
      lines(x=lgSR, y=predict(lmlist[[i]]), col = clrvec[i], xlim = xscale, ylim = yscale, xlab = labx, ylab = laby)
      if (i == dims[2]){
        legend(legendloc,legend = legendvec, fill = clrvec[1:dims[2]], cex = legendcex, bg = "transparent", box.lty = 0)
        par(new=FALSE)
      } else {
        par(new=TRUE)
      }
    }
    invisible(readline(prompt="Press [enter] to continue"))
  }
}

temp_vec <- c()
for (i in 1:dims[2]) {
  a <- as.numeric(lmlist[[i]][[1]][2])
  temp_vec <- append(temp_vec, a)
}

N.StressInd <- mean(1/temp_vec)

scatter_mat <- matrix(NA, nrow = dims[1], ncol = dims[2])
T_1 <- 1/(as.numeric(colnames(x)) + 273.15) # Convert to Kelvin
lmlist <- list(list())[rep(1,dims[1])]
for (i in 1:dims[1]) {
  scatter_mat[i,] <- as.numeric(lgsinhax[i,])
  a <- T_1
  lmlist[[i]] <- lm(scatter_mat[i,]~I(a^1)) # Part of artificial
}

if(FALSE){
  cat("Show fitting plot of log[sinh(alpha*flow_stress)] vs. 1/T(K^-1)?")
  ptr <- select.list(c("Yes", "No"))
  if(ptr == "Yes"){
    legendvec <- rownames(x)
    yscale <- c(min(unlist(scatter_mat)), max(unlist(scatter_mat))) # Part_of_artificial
    xscale <- c(min(T_1), max(T_1)) # Part_of_artificial

    labx <- "1/T(K^-1)"
    laby <- expression(paste("ln[sinh(", alpha, sigma, ")]", sep = ""))

    for(i in 1:dims[1]){
      plot(T_1, scatter_mat[i,], type = "p", col = clrvec[i], pch=16, xlim = xscale, ylim = yscale, xlab = labx, ylab = laby) # Part_of_artificial
      lines(x=T_1, y=predict(lmlist[[i]]), col = clrvec[i], xlim = xscale, ylim = yscale, xlab = labx, ylab = laby) # Part_of_artificial
      if (i == dims[1]){
        legend(legendloc,legend = legendvec, fill = clrvec[1:dims[2]], cex = legendcex, bg = "transparent", box.lty = 0)
        par(new=FALSE)
      } else {
        par(new=TRUE)
      }
    }
    invisible(readline(prompt="Press [enter] to continue"))
  }
} #Check epsExtract()!

temp_vec <- c()
for (i in 1:dims[1]) {
  a <- as.numeric(lmlist[[i]][[1]][2])
  temp_vec <- append(temp_vec, a)
}

K <- mean(temp_vec)
Q <- K * N.StressInd * 8.314 # R Unit: J/(mol*K); Q Unit: J/mol.





i <- 1
temp_vec <- c()
for (i in 1:dims[1]) {
  b <- as.vector(lgsinhax[i,])
  a <- Tem1
  slope <- as.numeric(lm(b~I(a^1))$coefficients[2])
  temp_vec <- append(temp_vec, slope)
}

K.MetaCons <- temp_vec # remember rev the order of 1/T!

i <- 1
temp_vec <- c()
for (i in 1:dims[2]) {
  a <- lgSR
  b <- as.vector(lgsinhax[,i])
  slope <- as.numeric(lm(b~I(a^1))$coefficients[2])
  temp_vec <- append(temp_vec, slope)
}

Hem1.MetaCons <- temp_vec #!!!

R.Cons <- 8.314 #Unit: J/(mol*K)
#Q.Temp <- R.Cons *mean(K.MetaCons)/mean(Hem1.MetaCons)
Q.Temp <- mean(K.MetaCons %o% (1/Hem1.MetaCons))*R.Cons #Q~Q(K,1/H), mean after outer product should be more accurate

# traverse dims[1]*dims[2] times using the built-in order in R
i <- 1
j <- 1
Zmat <- matrix(NA, nrow = dims[1], ncol = dims[2])
for (i in 1:dims[1]) {
  for (j in 1:dims[2]) {
    Tcond <- as.numeric(colnames(lgsinhax)[j]) + 273.15
    SRcond <- as.numeric(rownames(lgsinhax)[i])
    temp <- exp(Q.Temp/(R.Cons*Tcond))
    Zmat[i,j] <- SRcond*te
  }
}

# lnZmat <- matrix(NA, nrow = dims[1], ncol = dims[2])
# for (i in 1:dims[1]) {
#   for (j in 1:dims[2]) {
#     Tcond <- as.numeric(colnames(lgsinhax)[j]) + 273.15
#     SRcond <- as.numeric(rownames(lgsinhax)[i])
#     temp <- log(SRcond, base = logbase) - Q.Temp/(R.Cons*Tcond)
#     lnZmat[i,j] <- temp
#   }
# }

Z <- as.numeric(as.vector(Zmat))
lnZ <- log(Z, base = logbase)
a1 <- as.numeric(as.vector(lgsinhax))

n.PowerValue <- as.numeric(lm(lnZ~I(a1^1))$coefficients[2])

a2 <- as.numeric(as.vector((sinhax)^n.PowerValue))
A.MetaCons <- as.numeric(lm(Z~I(a2^1))$coefficients[2])

Q.ActivEnerg <- Q.Temp*(0.001) # unit: kJ/mol

if(consfuncPRT){
  cat("Constitutive equation in", eps, "strain:\nEpsilon=A{[sinh(Alpha*Sigma)]^n}*exp[-Q/(RT)]\nWhere A =",
      A.MetaCons, "s^-1 = ", format(A.MetaCons, scientific = TRUE), "s^-1\n      Alpha =", alpha.MateCons,
      "MPa^-1 = ", format(alpha.MateCons, scientific = TRUE), "Pa^-1\n      Q =", Q.Temp, "J/mol = ", Q.ActivEnerg,
      "kJ/mol\n      R = ", 8.314,"J/(mol*K)\n      Epsilon is strain rate\n      Sigma is flow stress\n      T is Temperature (K)", "\n")
  #print(paste("the constitutive equation in strain ", round(eps, rnd), " is: sigma=", "(1/", alpha.MateCons, ")ln{(Z/", format(A.MetaCons, scientific = TRUE), ")^(1/", round(n.PowerValue, rnd), ")+[(Z/", format(A.MetaCons, scientific = TRUE), ")^(2/", round(n.PowerValue, rnd), ")", "+1]^(1/2)}", sep =""))
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
    lgstr <- lgSR[i] #log(as.numeric(lgSR[i]), base = logbase) #rownames(lgsinhax)[i]
    temptable[i,j] <- bcd[1]+2*bcd[2]*lgstr+3*bcd[3]*(lgstr^2)
  }
}

etatable <- as.data.frame((2*temptable)/(1+temptable))
rownames(etatable) <- rownames(lgsinhax)
colnames(etatable) <- colnames(lgsinhax)

i <- 1
temp_vec <- c()
for (i in 1:dims[2]) {
  b <- temptable[,i]/(temptable[,i] + 1)
  a <- lgSR
  temptable[,i] <- as.numeric(lm(b~I(a^1))$coefficients[2]) + temptable[,i]
}

xitable <- temptable
rownames(xitable) <- rownames(lgsinhax)
colnames(xitable) <- colnames(lgsinhax)

# package two tables
rslttable <- list("SRTtable"=x, "etatable"=etatable, "xitable"=xitable)

result <- list("MaterialCoefficients"=MaterialCoefficients, "tablelist"=rslttable)
