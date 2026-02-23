library(dplyr)
library(ggplot2)
library(gridExtra)

#' EHSummarize_CategoryByTotal_ReturnsSingleTable
#'
#' Returns a bar chart for unaggregated data with a cetgory and a numeric value
#'
#' @param df - a dataframe with two columns, category and numeric value, in that order
#' @param font_size - change the font size from default 7
#' @param decreasingOrder - TRUE (default) puts the chart in decreasing order of value
#' @param rectfill - color for the background (slategray is the default)
#' @param xfill - color for the bars (Ivory is the default)
#' @param xtitle - choose a custom title
#'
#' @returns ggplot graph
#' @export
#'
#' @examples
#'

#' @exportClass EH_Summarize_CategoryByTotal_ReturnsSingleTable
EHSummarize_CategoryByTotal_ReturnsSingleTable <- function(df, font_size=7, decreasingOrder=TRUE, rectfill="slategray2", xfill = "ivory", xtitle = "")
{

  #EXPECTS A 2 COLUMN DF WITH CATEGORY AND AMOUNT IN THAT ORDER

  df3 <- as.data.frame(df) |>
    dplyr::rename(Category = 1, yValue=2)

  df2 <- df3 |>
    group_by(Category) |>
    summarise(Total = sum(yValue, na.rm = TRUE))

  ylimitmax= max(df2$Total) * 1.2

  if (min(df2$Total) <0) {
    ylimitmin = min(df2$Total)
  }
  else {
    ylimitmin = 0
  }

  if (decreasingOrder) {
    a1 <- ggplot(df2, aes(x=fct_reorder(Category, Total), y=Total)) }
  else {
    a1 <- ggplot(df2, aes(x=Category, y=Total)) }

  p <- a1 +
    coord_flip() +
    geom_col(color="black", fill=xfill, width=.3) +
    theme(legend.position="none") +
    ggtitle(xtitle) +
    theme(plot.margin = margin(0, 0, 2, 2, "cm"), title = element_text(size =(font_size), face="bold"), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(size = font_size), axis.text.y = element_text(size = font_size), axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank(),  panel.grid.major.y=element_line(color=rectfill), panel.background = element_rect(fill = rectfill, color="black", size = .4 )) +
    geom_text(aes(label = scales::comma(Total)), size=(4), fontface="bold", color="blue", hjust = -.2) +
    scale_y_continuous(labels = comma, limits = c(ylimitmin, ylimitmax))

  return (p)
}

