#' Tool-Kit for Dynamic Materials Model and Thermal Processing Maps
#'
#' @name Abstract-TPMplt
#' @docType package
#' @description Provides a simple approach for constructing dynamic materials modeling (DMM) suggested by Prasad and Gegel.
#' It can easily generate various processing-maps based on this model as well. The calculation result in this package contains
#' full materials constants, information about power dissipation efficiency factor, and rheological properties, can be exported
#' completely also, through which further analysis and customized plots will be applicable as well.
#' @details Input data should be of the data frame with "VBTree" style. Full calculation result returned from the function
#' DMMprocess builds the dynamic material model. 2D and 3D thermal processing-maps can be generated based on this model.
#' 2D plots are built using ggplot2 while 3D plots are constructed by rgl. Especially, 3D plots will separately generate two
#' 3D surfaces, for power dissipation efficiency eta, and rheological stability coefficient xi, respectively.
#' @author
#' Chen Zhang
#'
#' Maintainer: Chen Zhang <chen.zhang_06sept@foxmail.com>
#' @references
#' Prasad, YVRK, Gegel, HL, Doraivelu, SM, Malas, JC, Morgan, JT, Lark, KA & Barker, DR (1984). Modeling of dynamic material
#' behavior in hot deformation: forging of Ti-6242. Metallurgical Transactions A, 15, 1883-1892.
#'
#' Prasad, YVRK, Rao, KP & Sasidhar, S (2015). Hot working guide: a compendium of processing maps. ASM international
#' @seealso
#' \code{\link[VBTree:VBTree-package]{VBTree}}, \code{\link[ggplot2:ggplot2-package]{ggplot2}}, \code{\link[rgl:rgl-package]{rgl}}
#' @examples
#' \donttest{
#' # Check the factors in column names of input data:
#' # Note: Temperature in layer2, Strain Rate in layer3.
#' require(VBTree)
#' vbt <- dl2vbt(chrvec2dl(colnames(TPMdata)))
#' vbt
#'
#' # Export Strain Rate-Temperature table based on
#' # given strain condition (epsilon):
#' epstable <- epsExtract(TPMdata, 0.7, 2, 3)
#'
#' # Build dynamic materials model (DMM) from Strain
#' # Rate-Temperature table:
#' DMM <- DMMprocess(epstable)
#'
#' # Choose regression method for plots:
#' PLTbd <- SVRModel(DMM)
#'
#' # 2D processing-map through selected regression method:
#' TPM2dplt(PLTbd)
#'
#' # 3D processing-map through selected regression method:
#' TPM3dplt(PLTbd)
#' }
NULL
