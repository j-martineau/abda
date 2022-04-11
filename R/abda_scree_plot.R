#' @name abda_scree_data
#' @description Convert a processed data file (i.e., from
#'   \code{abda_process_data}) to a tibble for plotting the angle-based
#'   dimensionality scree plot.
#' @param x A named list resulting from \code{abda_process_data}.
#' @param data.title A character scalar labeling the data set for which the
#'   angle-based dimensionality scree plot is to be created for.
#' @param defs A tibble with different columns for different versions of
#'   theoretical category labels (for example, lowest level blueprint category,
#'   second level blueprint category, ...).
#' @param category The name of the column of \code{defs} to use as the
#'   theoretical category name.
#' @param single A logical scalar indicating whether to create a single plot
#'   panel (all angle changes in angles in a single plot panel) versus dual plot
#'   panels (i.e., within-category angle changes displayed in a separate panel
#'   from across-category angles).
#' @param offset The height of the separation between the upper panel
#'   (across-category angle changes) and the lower panel (within-category angle
#'   changes) as a multiple of the default. Ignored when \code{single = TRUE}.
#' @param max.dim Positive integer scalar giving the maximum level of dimensionality to plot.
#' @param shape The shape of the point to plot for each pair of items at each
#'   level of dimensionality. The same as \code{pch} in
#'   \code{\link[graphics]{points}}
#' @param size Numeric scalar size of points to plot. The same as \code{cex} in \code{\link[graphics]{points}}.
#' @param alpha Numeric scalar between 0 and 1 inclusive giving the proportion
#'   of complete opacity to apply to the plotted points (i.e., \code{0} is
#'   completely transparent, \code{1} is completely opaque).
#' @return A \code{ggplot2} object of class \code{'gg'}.
abda_scree_plot <- function(x, data.title, defs, category, single = T, offset = 1, max.dim = 25, shape = 20, size = 1, alpha = 1) {
  library(ggplot2)                                                               # load the ggplot2 library
  Rotate <- x$rotate                                                             # get the type of rotation used
  Offset <- ifelse(single, 0, offset * 15)                                       # offset is given in 15-degree units
  xMin   <- 1.5                                                                  # minimum x-value (2 is the lowest level of dimensionality)
  xMax   <- max.dim + 0.5                                                        # maximum x-value (1/2 above the maximum level of dimensionality to plot)
  cat("\n Applying the category labels")                                         # update the user
  Dims    <- as.integer(names(x$angles))                                         # get all levels of dimensionality for which data exist
  x       <- x$changes                                                           # extract the changes in angles as the plotting data
  N       <- nrow(x[[1]])                                                        # number of items
  VarLabs <- rownames(x[[1]])                                                    # variable labels
  RowLabs <- rownames(x[[1]])                                                    # row labels initialized as the same as variable labels
  CatLabs <- defs[[category]]                                                    # narrative category labels for the integer vector category indices
  for (i in 1:length(RowLabs)) {                                                 # for each row label
    RowLabs[[i]] <- CatLabs[which(defs$Variable == VarLabs[i])]                } # > replace the variable names with the variable definitions
  cat("\n Building a data.frame for plotting")                                   # update the user
  XX <- YY <- C1 <- C2 <- V1 <- V2 <- NULL                                       # initialize vectors for x, y, category 1, category 2, variable 1, and variable 2 values
  Cat.1 <- rep(CatLabs, each  = N)                                               # Category 1 is category labels each repeated N times
  Var.1 <- rep(VarLabs, each  = N)                                               # Variable 1 is variable labels each repeated N times
  Cat.2 <- rep(CatLabs, times = N)                                               # Category 2 is category labels repeated as a whole N times
  Var.2 <- rep(VarLabs, times = N)                                               # Variable 2 is variable labels repeated as a whole N times
  for (i in 1:length(x)) {                                                       # for each level of dimensionality
    XX <- c(XX, rep(Dims[i + 1], N * N))                                         # > X values are the level of dimensionality (repeat N * N times to account for every possible pair of items/variables)
    YY <- c(YY, as.vector(x[[i]]))                                               # > Y values are the change-in-angle values
    C1 <- c(C1, Cat.1)                                                           # > category 1 labels
    C2 <- c(C2, Cat.2)                                                           # > category 2 labels
    V1 <- c(V1, Var.1)                                                           # > variable 1 labels
    V2 <- c(V2, Var.2)                                                         } # > variable 2 labels
  cat("\n Sorting data on category name")                                        # update the user
  Data      <- tibble::tibble(x = XX, y = YY, Category.1 = C1, Category.2 = C2,  # tibble containing x-positions, y-positions,
                              Variable.1 = V1, Variable.2 = V2)  #   category labels, and variable labels
  Code      <- paste0(XX, " ¦ ", C1, " ¦ ", C2, " ¦ ", V1, " ¦ ", V2)            # create unique codes that
  Order     <- order(Code)                                                       # sort order is by order by x position, then category 1, then category 2, then variable 1, then variable 2
  Data      <- Data[Order, ]                                                     # order the data as by code
  VarLabs   <- unique(Data$Variable.1)                                           # unique variable labels
  BinWidth  <- 0.9                                                               # bin width leave 0.05 margin on each side of each bin
  VarWidth  <- BinWidth / N                                                      # the variable/item column width allotted is the bin width divided by the number of variables/times
  VarOffset <- VarWidth * ((1:N) - mean(1:N))                                    # offset of each variable within the dimensionality-specific bin
  cat("\n Calculating horizontal locations")                                     # update the user
  for (i in 1:length(VarLabs)) {                                                 # for each variable/item
    j         <- which(Data$Variable.1 == VarLabs[i])                            # > index the rows of data associated with variable/item
    Data$x[j] <- Data$x[j] + VarOffset[i]                                      } # > add the appropriate horizontal offset value
  C <- Data$Category.1                                                           # data row category labels
  V <- Data$Variable.1                                                           # data row variable labels
  X <- Data$x                                                                    # horizontal locations
  Y <- Data$y                                                                    # vertical locations
  if (!single) {                                                                 # if this is a dual-panel plot
    cat("\n Calculating mirrored y locations for lower panel")                   # > update the user
    Y    <- Y + Offset / 2                                                       # > add half the separation between panels to y-values
    i    <- C == Data$Category.2                                                 # > index rows where primary & secondary categories are the same
    Y[i] <- -Y[i]                                                              } # > reverse the sign for within-category angle changes
  Data <- tibble::tibble(x = X, y = Y, Category = C, Variable = V)               # simplified tibble with horizontal and vertical locations and category and variable labels
  Data <- Data[Data$x < max.dim + 0.5, ]                                         # remove any data rows for dimensionality greater than [max.dim]
  if (single) {                                                                  # if this is a single-pane scree plot
    yMin    <-   0 - 1                                                           # > minimum y value is -1 to give a margin below minimum angle change
    yMax    <- 180 + 1                                                           # > maximum y value is 181 to give a margin above maximum angle change
    yBrk    <- seq(0, 180, 15)                                                   # > place breaks at 15 degree intervals
    xMinPan <- xMin; xMaxPan <- xMax; yMinPan <- yMin; yMaxPan <- yMax           # > minimum and maximum x and y values
    yLab    <- seq(0, 180, 15)                                                 } # > y tick-mark labels
  else {                                                                         # otherwise
    yMin    <- -180 - 1 - Offset / 2                                             # > min y value is -181 - half of the separation between panels
    yMax    <-  180 + 1 + Offset / 2                                             # > max y value is  181 + half of the separation between panels
    yBrk    <- seq(yMin + 1, yMax - 1, 15)                                       # > y tick-mark locations run from min to max in increments of 15
    xMinPan <- c(xMin, xMin)                                                     # > x-min for panels are the same for both panels
    xMaxPan <- c(xMax, xMax)                                                     # > x-max for panels are the same for both panels
    yMinPan <- c(-1 + Offset / 2, yMin)                                          # > upper panel min y is -1 + half the separation between panels, lower is absolute min
    yMaxPan <- c(yMax,  1 - Offset / 2)                                          # > upper panel max y is absolute max, lower  is -1 - half the separation between panels
    yLab    <- seq(0, 180, 15)                                                   # > y-tick mark labels run from 0 to 180 in increments of 15
    yLab    <- c(rev(yLab), yLab)                                              } # > mirror the y-tick mark labels for the lower and upper panels
  yLab      <- paste0(yLab, "°")                                                 # paste a degree symbol after each y-tick mark label
  xLim      <- c(xMin, xMax)                                                     # horizontal value limits
  yLim      <- c(yMin, yMax)                                                     # vertical value limits
  xBrk      <- 2:max.dim                                                         # x-tick mark locations
  xLab      <- paste0(xBrk - 1, "→", xBrk)                                       # 'dim(n-1) → dim(n) as x-tick mark labels
  vLinesX   <-  0.5 + 1:max.dim                                                  # vertical reference line locations at 1.5 up to max.dim + 0.5 by increments of 1
  Title     <- paste0("Dimensionality Assessment of ", data.title)               # plot title
  Subtitle  <- paste0("absolute changes in angles between ", Rotate            , # plot subtitle
                      "-rotated factor loading vectors of feature pairs when " ,
                      "successively increasing modeled dimensionality by 1"    )
  TitleX    <- "\nIncrease in Dimensionality (from n→n+1)"                       # x-axis title
  TitleY    <- "Absolute Change in Angle (between feature vectors)\n"            # y-axis title
  Dual      <- "\n← Within Category            Across Category →\n"             # dual panel x-axis subtitle
  TitleY    <- paste0(TitleY, ifelse(single, "", Dual))                          # if single, not x-axis subtitle, otherwise append subtitle
  nCats     <- length(unique(Data$Category))                                     # number of categories
  Palette   <- rep(c("blue", "orange", "black", "magenta"), nCats)               # colorblind color palette with more than enough values
  Palette   <- Palette[1:nCats]                                                  # trim to the right number of values
  E         <- expansion(mult = 0)                                               # preallocate limits expansion parameter for tight axes
  G90       <- "grey90"; G85 <- "grey85"; B <- "black"                           # prespecify plot metacolors
  p <- ggplot(Data, aes(x = x, y = y, color = Category))                         # set up a plot template using the appropriate data, with the right aesthetics
  p <- p + xlab(TitleX)                                                          # add the x-axis label
  p <- p + ylab(TitleY)                                                          # add the y-axis label
  p <- p + coord_cartesian(xlim = xLim, ylim = yLim, expand = F)                 # add the axis limits and expansion setting
  p <- p + scale_x_continuous(breaks = xBrk, labels = xLab, expand = E)          # add the x-axis tick mark locations and labels without expansion
  p <- p + scale_y_continuous(breaks = yBrk, labels = yLab, expand = E)          # add the y-axis tick mark locations and labels without expansion
  p <- p + theme_light()                                                         # set the theme
  p <- p + theme(plot.background = element_rect(fill = G90, color = B))          # background is light grey with black border
  p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) # rotate x-axis text upward, left justified
  p <- p + theme(plot.subtitle = element_text(face = "italic"))                  # italicize the subtitle
  p <- p + theme(axis.ticks.x = element_blank())                                 # remove x-axis tick marks
  p <- p + theme(panel.border = element_blank())                                 # remove panel border(s)
  p <- p + theme(panel.grid.major.x = element_blank())                           # remove major x-axis gridlines
  p <- p + theme(panel.grid.minor.x = element_blank())                           # remove minor x-axis gridlines
  p <- p + ggtitle(label = Title, subtitle = Subtitle)                           # add plot title and subtitle
  p <- p + geom_vline(xintercept = vLinesX, color = G85)                         # add reference lines between vertical bins
  if (!single) {                                                                 # if this is a dual-panel plot
    p <- p + annotate(geom = "rect", xmin = xMin, xmax = xMax,                   # > draw the upper and lower panels as rects
                      ymin = -Offset / 2, ymax = Offset / 2,                     # >   at the right locations
                      color = "grey90", fill = "grey90")}                        # >   and with light grey fill and outline
  p <- p + geom_point(na.rm = T, shape = shape, size = size, alpha = alpha)      # add points to the graph
  p <- p + annotate(geom = "rect", xmin = xMinPan, xmax = xMaxPan,               # outline the panels with a 25 percent grey
                    ymin = yMinPan, ymax = yMaxPan, color = "grey75", fill = NA) #   rectangle and no fill
  p <- p + scale_color_manual(values = Palette)                                  # specify the palette
  p <- p + guides(color = guide_legend(override.aes = list(shape = 20,           # modify the legend to specify the shape
                                                           size = 2)))           #   and size to use for points in the legend
  cat("\n Printing the plot")                                                    # update the user
  print(p)                                                                       # print the plot to the plots window
  cat("\n PLEASE WAIT UP TO A MINUTE FOR THE PLOT TO RENDER")                    # notify the user of processing time needs
  p                                                                              # return the plot object
}
