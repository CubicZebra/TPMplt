#' Plot 2d thermal process maps
#'
#' @description Plot a 2d thermal process maps: logarithm strain rate as y axis while celsius temperature as x axis. Contours denotes
#' the power dissipation efficiency factor, while the background with gradual colors represents rheological stability.
#' @param x Regression results from modeling functions such as \code{\link[TPMplt:SVRModel]{SVRModel}}.
#' @param xloc Location for annotatin in x axis. The default value is 0.09.
#' @param yloc Location for annotatin in y axis. The default value is 0.03.
#' @param lowclr Colour for low rheological stability region. The default setting is "red".
#' @param mdclr Colour between low and high rheological stability regions. The default setting uses "white".
#' @param highclr Colour for high rheological stability region. The default setting is "green".
#'
#' @import ggplot2
#' @return A 2d thermal process maps with logarithm strain rate as its y axis while celsius temperature as its x axis. Strain conditon
#' is showed in top-left in the figure. Power dissipation efficiency factor eta is denoted by blue gradual contours, and the rheological
#' stability are represented by gradual background.
#' @export TPM2dplt
#'
#' @examples
#' epstable <- epsExtract(TPMdata, 0.7, 2, 3)
#' DMM <- DMMprocess(epstable)
#' PLTbd <- SVRModel(DMM)
#' TPM2dplt(PLTbd)
#' @keywords "PLTbuilder" "TPM2dplt"
TPM2dplt <- function(x, xloc=0.09, yloc=0.03, lowclr="red", mdclr="white", highclr="green"){
  # input data check
  if(!any(class(x)=="PLTbuilder")){
    stop("input data must be a data frame with the attribute of PLTbuilder, returned from related functions", call. = FALSE)
  }

  # plot theme
  theme_zc <- theme(plot.title = element_text(size = 12.5, face = "bold"),
                    axis.text = element_text(size = 11), axis.title = element_text(size = 12, face = "bold"),
                    axis.text.y = element_text(margin = margin(0, 6, 0, 0, "pt")), axis.text.x = element_text(margin = margin(6, 0, 0, 0, "pt")),
                    axis.ticks = element_line(size = 1), axis.ticks.length = unit(-0.16, "cm"),
                    panel.background = element_rect(color = "black", size = 1))
  legendtheme_zc <- theme(legend.key.size = unit(0.18, "inches"), legend.key.height = unit(0.18, "inches"),
                          legend.text = element_text(face = "bold"), legend.title = element_text(face = "bold"))
  my_theme <- theme_zc + legendtheme_zc

  # make plot
  xily <- ggplot(subset(x[[1]], group=="xi"), aes(T, lgSR, z = value))
  xily <- xily + geom_raster(aes(fill = value), interpolate = TRUE) + scale_fill_gradientn(name="xi", colours=c(lowclr, mdclr, highclr))
  ath_etaly <- xily + geom_contour(data = subset(x[[1]], group=="eta"), aes(T, lgSR, z=value, colour = stat(level)), inherit.aes = TRUE)
  ath_etaly <- ath_etaly + my_theme + xlab("Temperature (Celsius)") + ylab("LogStrainRate (log(s^(-1)))") + labs(color="eta")

  # add annotation
  SR <- x[[2]]
  locx <- min(x[[1]][,1]) + (max(x[[1]][,1])-min(x[[1]][,1]))*xloc
  locy <- max(x[[1]][,2]) - (max(x[[1]][,2])-min(x[[1]][,2]))*yloc
  result <- ath_etaly + annotate("text", x=locx, y=locy, label=paste("Strain: ", SR, sep = ""), colour = "black")

  return(result)
}


#' Internal functions
#'
#' @param x An "PLTbuilder" object.
#' @param grp "eta" or "xi" to determine which group is extracted as the subset.
#'
#' @return A matrix for 3d surface plot.
#' @export surfacebld
#'
#' @examples
#' epstable <- epsExtract(TPMdata, 0.7, 2, 3)
#' DMM <- DMMprocess(epstable)
#' PLTbd <- SVRModel(DMM)
#' surfacebld(PLTbd, "eta")
#' @keywords internal
surfacebld <- function(x, grp=c("eta", "xi")){
  tt <- subset(x[[1]], group == grp)
  clnslt <- c(4,1,2)
  m <- cbind(tt[,clnslt[1]], tt[,clnslt[2]], tt[,clnslt[3]])
  len <- length(m[,1])
  levx <- as.numeric(levels(factor(m[,2])))
  levy <- as.numeric(levels(factor(m[,3])))
  cvrtm <- matrix(NA, nrow = length(levx), ncol = length(levy))

  i <- 1
  for (i in 1:len) {
    x_cor <- which(levx==m[i,2])
    y_cor <- which(abs(levy-m[i,3]) < 0.000002)
    cvrtm[x_cor, y_cor] <- m[i,1]
  }
  result <- list(result=cvrtm, levx=levx, levy=levy)
  return(result)
}


#' Internal function
#'
#' @param x An "PLTbuilder" object.
#' @param gain A positive integer to gain gradual colours. Default value is 100.
#' @param division subdivision numbers for x, y and z axises.
#' @param zeroplane Boolean value to control for adding the plane of z=0. Default setting is TRUE.
#'
#' @import rgl
#' @return A surface 3d plot.
#' @export basic3d
#'
#' @examples
#' epstable <- epsExtract(TPMdata, 0.7, 2, 3)
#' DMM <- DMMprocess(epstable)
#' PLTbd <- SVRModel(DMM)
#' PLT3dbd <- surfacebld(PLTbd, "eta")
#' basic3d(PLT3dbd)
#' @keywords internal
basic3d <- function(x, gain=100, division=5, zeroplane=TRUE){
  levx <- x[[2]]
  levy <- x[[3]]
  x <- x[[1]]
  eta.gained <- x*gain
  T.index <- (1:nrow(x))
  lgSR.index <- (1:ncol(x))

  zlim <- range(eta.gained)
  zlen <- zlim[2] - zlim[1] + 1
  colorlut <- cm.colors(zlen) # height color lookup table
  col <- colorlut[(eta.gained-zlim[1]+1)] # assign colors to heights for each point

  T.index1 <- T.index*zlim[2]/max(T.index)
  lgSR.index1 <- lgSR.index*zlim[2]/max(lgSR.index)


  surface3d(T.index1, lgSR.index1, x*gain, color=col, back="lines")
  grid3d(c("x", "y+", "z"), n =20)

  lgSR.name <- round(levy[1:ncol(x)], 2)
  lgSR.label <- as.integer(seq(1, length(lgSR.name), length = division))

  axis3d('y+',at = lgSR.index1[lgSR.label], lgSR.name[lgSR.label], cex = 1)
  axis3d('y',at = lgSR.index1[lgSR.label], lgSR.name[lgSR.label], cex = 1)
  eta.label <- seq(min(zlim), max(zlim), length=division)
  axis3d('z',pos=c(0, 0, NA), at=eta.label, round(eta.label/gain, 2), cex =1)

  T.label <- as.integer(seq(1, length(T.index), length = division))
  axes3d('x', at=T.index1[T.label], levx[T.label], cex = 1)

  if(zeroplane==TRUE){
    planes3d(a=0, b=0, c=1, d=0, alpha=0.6)
  }
}

#' Plot 3d thermal process maps
#'
#' @description Return a 3d thermal process result consisted of 3d surfaces for power dissipation efficiency eta and rheological stability
#' xi respectively.
#' @param x Regression results from modeling functions such as \code{\link[TPMplt:SVRModel]{SVRModel}}.
#' @param dvs A positive integer to set the divisions for all labels in two surface 3d plots. The default value is 5.
#'
#' @import rgl
#' @return Two 3d surface plots: the left one denotes power dissipation efficiency factor eta, while the right one is for rheological stability
#' xi. A zero plane for xi value is added in the right plots for determining unstable region.
#' @export TPM3dplt
#'
#' @examples
#' epstable <- epsExtract(TPMdata, 0.7, 2, 3)
#' DMM <- DMMprocess(epstable)
#' PLTbd <- SVRModel(DMM)
#' TPM3dplt(PLTbd)
#' @keywords "PLTbuilder" "TPM3dplt"
TPM3dplt <- function(x, dvs=5){
  # input data check
  if(!any(class(x)=="PLTbuilder")){
    stop("input data must be a data frame with the attribute of PLTbuilder, returned from related functions", call. = FALSE)
  }

  mfrow3d(1, 2, sharedMouse = TRUE)
  eta <- surfacebld(PLTbd, "eta")
  basic3d(eta, 100, division = dvs, zeroplane = FALSE)
  title.name <- "Surface3d: power dissipation efficiency"
  title.sub <- paste("(Strain=", PLTbd[[2]], ")", sep = "")
  title3d(main = title.name, sub = title.sub, xlab = "Temperature (Celsius)", ylab = "LogStrainRate (log(s^(-1)))", zlab = "eta")
  next3d()
  xi <- surfacebld(PLTbd, "xi")
  basic3d(xi, 100, division = dvs)
  title.name <- "Surface3d: rheological stability"
  title.sub <- paste("(Strain=", PLTbd[[2]], ")", sep = "")
  title3d(main = title.name, sub = title.sub, xlab = "Temperature (Celsius)", ylab = "LogStrainRate (log(s^(-1)))", zlab = "xi")
}
