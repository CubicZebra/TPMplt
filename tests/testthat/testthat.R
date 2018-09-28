library(VBTree)
library(ggplot2)
library(rgl)
library(TPMplt)

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
