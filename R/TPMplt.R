TPM2dplt <- function(x){
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
  xily <- xily + geom_raster(aes(fill = value), interpolate = TRUE) + scale_fill_gradientn(name="xi", colours=c("red", "#FFFFFFFF", "green"))
  ath_etaly <- xily + geom_contour(data = subset(x[[1]], group=="eta"), aes(T, lgSR, z=value, colour = stat(level)), inherit.aes = TRUE)
  ath_etaly <- ath_etaly + my_theme + xlab("Temperature (Celsius)") + ylab("LogStrainRate (log(s^(-1)))") + labs(color="eta")

  # add annotation
  SR <- x[[2]]
  locx <- min(x[[1]][,1]) + (max(x[[1]][,1])-min(x[[1]][,1]))*0.09
  locy <- max(x[[1]][,2]) - (max(x[[1]][,2])-min(x[[1]][,2]))*0.03
  result <- ath_etaly + annotate("text", x=locx, y=locy, label=paste("Strain: ", SR, sep = ""), colour = "black")

  return(result)
}

epstable <- epsExtract(TPMdata, 0.7, 2, 3)
DMM <- DMMprocess(epstable)
PLTbd <- SVRModel(DMM)
# TPM2dplt(PLTbd)

tt <- subset(PLTbd[[1]], group == "eta")
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

dim(cvrtm)

peak.height <- cvrtm*200
T.index <- (1:nrow(cvrtm));
lgSR.index <- (1:ncol(cvrtm));

zlim <- range(peak.height)
zlen <- zlim[2] - zlim[1] + 1
colorlut <- cm.colors(zlen) # height color lookup table
col <- colorlut[(peak.height-zlim[1]+1)] # assign colors to heights for each point
open3d()

T.index1 <- T.index*zlim[2]/max(T.index);
lgSR.index1 <- lgSR.index*zlim[2]/max(lgSR.index)

title.name <- paste("plot3d ", "cvrtm", sep = "");
surface3d(T.index1, lgSR.index1, peak.height, color=col, back="lines", main = title.name);
grid3d(c("x", "y+", "z"), n =20)

sample.name <- paste("col.", 1:ncol(cvrtm), sep="");
sample.label <- as.integer(seq(1, length(sample.name), length = 5));

axis3d('y+',at = lgSR.index1[sample.label], sample.name[sample.label], cex = 0.3);
axis3d('y',at = lgSR.index1[sample.label], sample.name[sample.label], cex = 0.3)
axis3d('z',pos=c(0, 0, NA))

ppm.label <- as.integer(seq(1, length(T.index), length = 10));
axes3d('x', at=c(T.index1[ppm.label], 0, 0), abs(round(T.index[ppm.label], 2)), cex = 0.3);

title3d(main = title.name, sub = "test", xlab = "ppm", ylab = "samples", zlab = "peak")
rgl.bringtotop();



