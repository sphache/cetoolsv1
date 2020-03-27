#' Add customized ggplot theme
#'
#' @param base_size font size
#' @param base_family font family
#' @param base_line_size  line size
#' @examples
#' p1 <- p1 + mytheme.scatter(base_size = 11, base_family = "Arial", base_line_size = 0.1)
mytheme.scatter <- function(base_size=11,
                    base_family="Arial",
                    base_line_size=0.1){
    theme_bw(base_size = base_size,
           base_family = base_family,
           base_line_size = base_line_size
           ) %+replace%
    theme(panel.grid.major = element_line(colour = "black", size=base_line_size, linetype = "dotted"),
      panel.grid.minor = element_line(colour = "grey", size=base_line_size, linetype = "dotted"),
      panel.border = element_rect(fill=NA, colour = "black", size=0.2),
      #axis.title = element_text(color = rgb(105, 105, 105, maxColorValue = 255), size = rel(0.75)),
      #axis.text = element_text(color = rgb(105, 105, 105, maxColorValue = 255), size = rel(0.5))
      axis.title = element_text(color = "black", size = rel(0.75)),
      axis.text = element_text(color = "black", size = rel(0.5))
    )
}


#' Adjusts the size of a legend
#'
#' @param myplot a ggplot
#' @param pointSize size of the legend symptoms
#' @param textSize size of text
#' @param spaceLegend space between legend elements
#' @examples
#' p1 <- adjustLegendSize(p1, pointSize = 0.7, textSize = 10, spaceLegend = 0.7)
adjustLegendSize <- function(myPlot, pointSize=0.7, textSize=7, spaceLegend=0.7) {
    myPlot +
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
           color = guide_legend(override.aes = list(size = pointSize))) +
    theme(legend.title = element_text(size = textSize),
          legend.text  = element_text(size = textSize-1),
          legend.text  = element_text(size = textSize),
          legend.key.size = unit(spaceLegend, "lines"))
}

