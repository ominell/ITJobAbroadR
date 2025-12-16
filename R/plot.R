#' Plot Fuzzy VIKOR Results
#'
#' @description Visualizes the VIKOR results using a bubble chart.

#' @param data An object of class `fuzzy_vikor_res` returned by the `fuzzy_vikor()` function.
#' @return A ggplot object.
#' @import ggplot2
#' @import ggrepel
#' @export

plot.fuzzy_vikor <- function(data) {
  #labels
    plot(
      title = "Fuzzy VIKOR Compromise Analysis",
      x = "Countries (Defuzzified S)",
      y = "Work Satisfaction (Defuzzified R)",
    )
}
