\name{TPMplt-package}
\alias{TPMplt-package}
\alias{TPMplt}
\docType{package}
\title{
\packageTitle{TPMplt}
}
\description{
\packageDescription{TPMplt}
}
\details{
Input data should be of the data frame with "VBTree" style (see \code{\link[VBTree:VBTree-package]{VBTree}}). Full calculation result
is returned from the function \code{\link[TPMplt:DMMprocess]{DMMprocess}}. Thermal processing-maps are generated based on this DMM
result. 2D plots are generated using \code{\link[ggplot2:ggplot2-package]{ggplot2}} while 3D plots are results from \code{\link[rgl:rgl-package]{rgl}}.
3D plots will separate power dissipation efficiency eta, from rheological stability coefficient xi, and generated two 3D surface plots
respectively.
}
\author{
\packageAuthor{TPMplt}

Maintainer: \packageMaintainer{TPMplt}
}
\references{
Prasad, YVRK, Gegel, HL, Doraivelu, SM, Malas, JC, Morgan, JT, Lark, KA & Barker, DR (1984). Modeling of dynamic material behavior in hot deformation: forging of Ti-6242. Metallurgical Transactions A, 15, 1883-1892.

Prasad, YVRK, Rao, KP & Sasidhar, S (2015). Hot working guide: a compendium of processing maps. ASM international
}
\keyword{package}
\seealso{
\code{\link[VBTree:VBTree-package]{VBTree}}, \code{\link[ggplot2:ggplot2-package]{ggplot2}}, \code{\link[rgl:rgl-package]{rgl}}.
}
\examples{
\dontrun{
# Check the factors in column names of input data:
# Note: Temperature in layer2, Strain Rate in layer3.
vbt <- dl2vbt(chrvec2dl(colnames(TPMdata)))
vbt

# Export Strain Rate-Temperature table based on
# given strain condition (epsilon):
epstable <- epsExtract(TPMdata, 0.7, 2, 3)

# Build dynamic materials model (DMM) from Strain
# Rate-Temperature table:
DMM <- DMMprocess(epstable)

# Choose regression method for plots:
PLTbd <- SVRModel(DMM)

# 2D processing-map through selected regression
# method:
TPM2dplt(PLTbd)

# 3D processing-map through selected regression
# method:
TPM3dplt(PLTbd)
}
}
