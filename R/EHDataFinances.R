library(dplyr)
library(ggplot2)
library(gridExtra)
library(roxygen2)

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
#'
#' @export
EHFinances_CategoryByTotal_ReturnsSingleTable <- function(df, font_size=7, decreasingOrder=TRUE, rectfill="slategray2", xfill = "ivory", xtitle = "")
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

EH_CleanCreditCards <- function(df, xsource) {
  df2 <- df |> dplyr::select(-`Post Date`) |>
    mutate(Source = xsource, SubCategory = "NA", ToDelete = 0) |>
    mutate(Amount=-1*Amount)
  return(df2)
}

#' @export
EHFinances_ImportCategories <- function()
{

  dfCategories <- read_csv("D:\\RStudio\\Finances\\Categories.csv")

  return(dfCategories)

}


EH_CleanBankAccounts <- function(df, xsource) {
  df2 <- df |>
    dplyr::rename(Memo=Status, `Transaction Date` = Date) |>
    mutate(Amount = as.character(ifelse(!is.na(Debit), Debit, Credit))) |>
    mutate(Type = as.character(ifelse(!is.na(Debit), "Debit", "Credit"))) |>
    mutate(Amount = as.numeric(parse_number(Amount))) |>
    dplyr::select(-Debit, -Credit) |>
    mutate(Category = "NA") |>
    mutate(Source = xsource, SubCategory = "NA", ToDelete = 0)

  return(df2)
}

#' @export
EHFinances_ImportRawAccountFiles <- function(Folder)
{

dfChase2785_raw <- read_csv(paste0("D:\\RStudio\\Finances\\AccountDownloads\\", Folder, "\\Chase2785.csv"))
dfChase4025_raw <- read_csv(paste0("D:\\RStudio\\Finances\\AccountDownloads\\", Folder, "\\Chase4025.csv"))
dfChase7825_raw <- read_csv(paste0("D:\\RStudio\\Finances\\AccountDownloads\\", Folder, "\\Chase7825.csv"))

dfChase2785 <- EH_CleanCreditCards(dfChase2785_raw, "cc2785")
dfChase4025 <- EH_CleanCreditCards(dfChase4025_raw, "cc4025")
dfChase7825 <- EH_CleanCreditCards(dfChase7825_raw, "cc7825")

dfCHK4987_raw <- read_csv(paste0("D:\\RStudio\\Finances\\AccountDownloads\\", Folder, "\\CHK_4987.csv"))
dfCHK7144_raw <- read_csv(paste0("D:\\RStudio\\Finances\\AccountDownloads\\", Folder, "\\CHK_7144.csv"))
dfCiti1547_raw <- read_csv(paste0("D:\\RStudio\\Finances\\AccountDownloads\\", Folder, "\\Citi_1547.csv"))

dfCHK4987 <- EH_CleanBankAccounts(dfCHK4987_raw, "ba4987")
dfCHK7144 <- EH_CleanBankAccounts(dfCHK7144_raw, "ba7144")
dfCiti1547 <- EH_CleanBankAccounts(dfCiti1547_raw,  "dc1547")

dfExpenses <- rbind(dfChase2785, dfChase4025, dfChase7825, dfCHK4987, dfCHK7144, dfCiti1547) |>
  mutate(`Transaction Date` = anydate(`Transaction Date`)) |>
  mutate(ID = row_number())

dfCategories <- EHFinances_ImportCategories()

dfExpenses2 <- dfExpenses |>
  rowwise() |>
  mutate(SupercedesTrip = dfCategories$SupercedesTrip[which(str_detect(Description, fixed(dfCategories$xKey)))[1]]
  ) |>
  ungroup() |>
  mutate(SupercedesTrip=if_else(is.na(SupercedesTrip), 0, SupercedesTrip)) |>
  mutate(Corrected=0)

liAccounts=list()
liAccounts[[1]] <- dfExpenses2

return (liAccounts)

}

#'

EHFinances_ImportAccountsToDelete <- function()
{

  dfCategories <- read_csv("D:\\RStudio\\Finances\\AccountsToDelete.csv")

  return(dfCategories)

}

#' @export
EHFinances_AssignAccountsToDelete <- function(dfExpenses)
{

  dfAccountsToDelete <- EHFinances_ImportAccountsToDelete()

  vAccounts <- dfAccountsToDelete$AccountToDelete

  for(i in 1:length(vAccounts)) {

    dfExpenses <- dfExpenses |>
      mutate(ToDelete = ifelse(str_detect(Description, vAccounts[i]), 1, ToDelete)) |>
      mutate(Category=if_else(ToDelete==1, "To Delete", Category)) |>
      mutate(SubCategory=if_else(ToDelete==1, "To Delete", SubCategory))
  }

  return(dfExpenses)

}

#' @export
EHFinances_AssignTrips <- function(dfExpenses, strStartDate, strEndDate, strTripName)
{

  dfExpenses2 <- dfExpenses |>
  mutate(zCategory = ifelse(between(`Transaction Date`, as.Date(strStartDate), as.Date(strEndDate)) & SupercedesTrip==0, "Travel", zCategory)) |>
  mutate(zSubCategory = ifelse(between(`Transaction Date`, as.Date(strStartDate), as.Date(strEndDate)) & SupercedesTrip==0, strTripName, zSubCategory))

  dfExpenses3 <- dfExpenses2 |>
  mutate(Category=if_else(zCategory=="Travel" & SupercedesTrip ==0, zCategory, Category)) |>
    mutate(Category=if_else(zCategory!="Travel" & zCategory!="NA", zCategory, Category)) |>
    mutate(SubCategory=if_else(zCategory=="Travel" & SupercedesTrip ==0, zSubCategory, SubCategory)) |>
    mutate(SubCategory=if_else(zCategory!="Travel" & zSubCategory!="NA", zSubCategory, SubCategory))

return(dfExpenses3)

}

#' @export
EHFinances_AssignCategoriesAndSubcategories <- function(dfExpenses) {

  dfCategories <- EHFinances_ImportCategories()

dfExpenses2 <- dfExpenses |>
  rowwise() |>
  mutate(zCategory=Category, zSubCategory=SubCategory) |>
  mutate(zCategory = dfCategories$xCategory[which(str_detect(Description, fixed(dfCategories$xKey)))[1]], zSubCategory = dfCategories$xSubCategory[which(str_detect(Description, fixed(dfCategories$xKey)))[1]]) |>
  ungroup() |>
  mutate(zCategory=if_else(is.na(zCategory), "NA", zCategory), zSubCategory=if_else(is.na(zSubCategory), "NA", zSubCategory))

return(dfExpenses2)
}

#' @export
EHFinances_AssignRuby <- function(dfExpenses)
{

  dfExpenses2 <- dfExpenses |>
    dplyr::mutate(zSubCategory = if_else(Source=="cc7825", Category, zSubCategory), zCategory = if_else(Source=="cc7825", "Ruby", zCategory))

  return(dfExpenses2)

}

