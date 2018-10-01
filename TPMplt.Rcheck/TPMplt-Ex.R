pkgname <- "TPMplt"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "TPMplt-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('TPMplt')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("API4TMZ")
### * API4TMZ

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: API4TMZ
### Title: Read multiple files exported from Thermec Master-Z tester
### Aliases: API4TMZ
### Keywords: APIfunction

### ** Examples

## Not run: 
##D variable1 <- c("factor11", "factor12", "factor13")
##D variable2 <- c("factor21", "factor22")
##D variable3 <- c("factor31", "factor32", "factor33", "factor34")
##D conditions <- list(variable1, variable2, variable3)
##D SummaryTable <- API4TMZ(conditions, "/Your_Directory_for_data/")
##D SummaryTable
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("API4TMZ", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Abstract-TPMplt")
### * Abstract-TPMplt

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Abstract-TPMplt
### Title: Tool-Kit for Dynamic Materials Model and Thermal Processing Maps
### Aliases: Abstract-TPMplt Abstract-TPMplt-package

### ** Examples

## Not run: 
##D # Check the factors in column names of input data:
##D # Note: Temperature in layer2, Strain Rate in layer3.
##D require(VBTree)
##D vbt <- dl2vbt(chrvec2dl(colnames(TPMdata)))
##D vbt
##D 
##D # Export Strain Rate-Temperature table based on
##D # given strain condition (epsilon):
##D epstable <- epsExtract(TPMdata, 0.7, 2, 3)
##D 
##D # Build dynamic materials model (DMM) from Strain
##D # Rate-Temperature table:
##D DMM <- DMMprocess(epstable)
##D 
##D # Choose regression method for plots:
##D PLTbd <- SVRModel(DMM)
##D 
##D # 2D processing-map through selected regression method:
##D TPM2dplt(PLTbd)
##D 
##D # 3D processing-map through selected regression method:
##D TPM3dplt(PLTbd)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Abstract-TPMplt", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("DMMprocess")
### * DMMprocess

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DMMprocess
### Title: Dynamic material modeling from strain rate temperature table
### Aliases: DMMprocess
### Keywords: DMMprocess DMMresult epsExtract

### ** Examples

require(VBTree)
dl2vbt(chrvec2dl(colnames(TPMdata)))
epstable <- epsExtract(TPMdata, 0.7, 2, 3)
DMM <- DMMprocess(epstable)
DMM



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DMMprocess", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("MakeGrid")
### * MakeGrid

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: MakeGrid
### Title: Make grid mesh for plots
### Aliases: MakeGrid
### Keywords: internal

### ** Examples

epstable <- epsExtract(TPMdata, 0.7, 2, 3)
DMM <- DMMprocess(epstable)
prdptr <- MakeGrid(DMM)
prdptr



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("MakeGrid", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("SSplots")
### * SSplots

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: SSplots
### Title: Automatic completion for Stress-Strain plots
### Aliases: SSplots
### Keywords: SSplots lyIDdetector

### ** Examples

## Not run: 
##D require(VBTree)
##D # Find locations for temperature and strain rate:
##D # temperature in layer2, strain rate in layer3;
##D # Strain in layer1 level1, Stress in layer1 level2.
##D dl2vbt(chrvec2dl(colnames(TPMdata)))
##D 
##D # Attention: Zoom your Plots panes large enough to ensure
##D # correct output!
##D 
##D # Plot multiple Stress-Strain curves, grouped by strain rate:
##D SSplots(TPMdata, 3, mfrow=c(3, 3))
##D 
##D # Plot multiple Stress-Strain curves, grouped by temperature:
##D SSplots(TPMdata, 2, mfrow=c(2, 2))
##D 
##D # Manual setting, for Stress-Stain plots:
##D SSplots(TPMdata, 2, manual=c(1, 1, 2), mfrow=c(2, 2))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("SSplots", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("SVRModel")
### * SVRModel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: SVRModel
### Title: Build support vector regression result
### Aliases: SVRModel
### Keywords: PLTbuilder SVRModel

### ** Examples

epstable <- epsExtract(TPMdata, 0.7, 2, 3)
DMM <- DMMprocess(epstable)
PLTbd <- SVRModel(DMM)
PLTbd



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("SVRModel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("TMZdatainput")
### * TMZdatainput

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: TMZdatainput
### Title: Read multiple files exported from Thermec Master-Z tester
### Aliases: TMZdatainput
### Keywords: APIfunction

### ** Examples

## Not run: 
##D variable1 <- c("factor11", "factor12", "factor13")
##D variable2 <- c("factor21", "factor22")
##D variable3 <- c("factor31", "factor32", "factor33", "factor34")
##D conditions <- list(variable1, variable2, variable3)
##D SummaryTable <- TMZdatainput(Cdl=conditions, wd="/Your_Directory_for_data/")
##D SummaryTable
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("TMZdatainput", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("TPM2dplt")
### * TPM2dplt

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: TPM2dplt
### Title: Plot 2d thermal process maps
### Aliases: TPM2dplt
### Keywords: PLTbuilder Processing-map

### ** Examples

epstable <- epsExtract(TPMdata, 0.7, 2, 3)
DMM <- DMMprocess(epstable)
PLTbd <- SVRModel(DMM)
TPM2dplt(PLTbd)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("TPM2dplt", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("TPM3dplt")
### * TPM3dplt

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: TPM3dplt
### Title: Plot 3d thermal processing-maps
### Aliases: TPM3dplt
### Keywords: PLTbuilder Processing-map

### ** Examples

epstable <- epsExtract(TPMdata, 0.7, 2, 3)
DMM <- DMMprocess(epstable)
PLTbd <- SVRModel(DMM)
TPM3dplt(PLTbd)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("TPM3dplt", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("basic3d")
### * basic3d

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: basic3d
### Title: 3D plots without labels
### Aliases: basic3d
### Keywords: internal

### ** Examples

epstable <- epsExtract(TPMdata, 0.7, 2, 3)
DMM <- DMMprocess(epstable)
PLTbd <- SVRModel(DMM)
PLT3dbd <- surfacebld(PLTbd, "eta")
basic3d(PLT3dbd, clrctrl="cm")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("basic3d", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("epsExtract")
### * epsExtract

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: epsExtract
### Title: Auto output for strain rate vs. temperature table
### Aliases: epsExtract
### Keywords: SR-T.table epsExtract lyIDdetector

### ** Examples

require(VBTree)
# Find locations for temperature and strain rate:
dl2vbt(chrvec2dl(colnames(TPMdata)))
epsExtract(TPMdata, eps = 0.7, lyT = 2, lySR = 3)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("epsExtract", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("etatidy")
### * etatidy

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: etatidy
### Title: Tidy eta table for further process
### Aliases: etatidy
### Keywords: internal

### ** Examples

epstable <- epsExtract(TPMdata, 0.7, 2, 3)
DMM <- DMMprocess(epstable)
etaM <- etatidy(DMM)
etaM



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("etatidy", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("lyIDdetector")
### * lyIDdetector

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: lyIDdetector
### Title: Detecting locations for Strain and Stress
### Aliases: lyIDdetector
### Keywords: epsExtract lyIDdetector

### ** Examples

require(VBTree)
chrvec2dl(colnames(TPMdata))
lyIDdetector(TPMdata)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("lyIDdetector", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("surfacebld")
### * surfacebld

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: surfacebld
### Title: Build matrix for 3D plots
### Aliases: surfacebld
### Keywords: internal

### ** Examples

epstable <- epsExtract(TPMdata, 0.7, 2, 3)
DMM <- DMMprocess(epstable)
PLTbd <- SVRModel(DMM)
surfacebld(PLTbd, "eta")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("surfacebld", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("xitidy")
### * xitidy

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: xitidy
### Title: Tidy xi table for further process
### Aliases: xitidy
### Keywords: internal

### ** Examples

epstable <- epsExtract(TPMdata, 0.7, 2, 3)
DMM <- DMMprocess(epstable)
xiM <- xitidy(DMM)
xiM



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("xitidy", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
