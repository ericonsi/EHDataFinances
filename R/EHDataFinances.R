library(dplyr)
library(ggplot2)
library(gridExtra)
library(roxygen2)
library(tidyverse)
library(anytime)
library(kableExtra)

#' EHSummarize_CategoryByTotal_ReturnsSingleTable5
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

EHFinances_RetrieveYearAndMonth <- function(Folder) {

  Folder <- as.character(Folder)
  yy <- substr(Folder, 1, 2)
  qMonth <- substr(Folder, 3, 4)
  qYear <- paste0("20", yy)

  li = list()

  li[[1]] <- as.numeric(qYear)
  li[[2]] <- as.numeric(qMonth)

  return (li)

}

EHFinances_TestIfDateIsInRange <- function(xDate, Folder) {

  x <- if_else(year(xDate)==EHFinances_RetrieveYearAndMonth(Folder)[[1]], month(xDate)==EHFinances_RetrieveYearAndMonth(Folder)[[2]], TRUE, FALSE)
  return (x)

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

#' @export
EHFinances_ImportAmazonOrders <- function(Folder)
{

  dfCategories <- read_csv("D:\\RStudio\\Finances\\AmazonOrders\\Retail.OrderHistory.1\\Retail.OrderHistory.1.csv") |>
  dplyr::select(`Order ID`, `Order Date`, `Total Owed`, `Payment Instrument Type`, `Order Status`, `Shipping Address`, `Product Name`, ASIN) |>
  dplyr::filter(EHFinances_TestIfDateIsInRange(xDate=`Order Date`, Folder))

  return(dfCategories)

}

EHFinances_ImportBudgetTargets <- function(Folder)
{

  dfTargets <- read_csv(paste0("D:\\RStudio\\Finances\\AccountDownloads\\", Folder, "\\BudgetTargets.csv"))

  return(dfTargets)

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
  mutate(`Transaction Date` = as.Date(`Transaction Date`, format = "%m/%d/%Y")) |>
  mutate(ID = row_number())

dfCategories <- EHFinances_ImportCategories()

dfExpenses2 <- dfExpenses |>
  rowwise() |>
  mutate(SupercedesTrip = dfCategories$SupercedesTrip[which(str_detect(Description, fixed(dfCategories$xKey)))[1]]
  ) |>
  ungroup()

dfExpenses3 <- dfExpenses2 |>
  mutate(SupercedesTrip=if_else(is.na(SupercedesTrip), 0, SupercedesTrip)) |>
  mutate(Corrected=0) |>
  #dplyr::filter(year(`Transaction Date`)==EHFinances_RetrieveYearAndMonth(Folder)[[1]], month(`Transaction Date`)==EHFinances_RetrieveYearAndMonth(Folder)[[2]])
  dplyr::filter(EHFinances_TestIfDateIsInRange(xDate=`Transaction Date`, Folder))

dfCategories <- EHFinances_ImportCategories()
liAccounts=list()

liAccounts[[1]] <- dfExpenses3

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
EHFinances_AssignFoodSubCategories <- function(dfExpenses) {

  dfExpenses2 <- dfExpenses |>
    mutate(AmountCategoryTmp = case_when(
      Amount <=10 ~ "Snack",
      Amount <= 20 ~ "Solo",
      Amount <+ 60 ~ "Date",
      Amount <+ 100 ~ "Family",
      TRUE       ~ "Group")) |>
    mutate(SubCategory = ifelse(Category=="Food & Drink", AmountCategoryTmp, SubCategory)) |>
    dplyr::select(-AmountCategoryTmp)

  return (dfExpenses2)
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

  dfExpenses3 <- EHFinances_AssignFoodSubCategories(dfExpenses2)

return(dfExpenses3)
}

#' @export
EHFinances_AssignRuby <- function(dfExpenses)
{

  dfExpenses2 <- dfExpenses |>
    dplyr::mutate(SubCategory = if_else(Source=="cc7825", Category, SubCategory), Category = if_else(Source=="cc7825", "Ruby", Category))

  return(dfExpenses2)

}

#' @export
EHFinances_ApplyBusinessRules <- function(dfExpenses) {

dfExpenses2 <- dfExpenses |>
  dplyr::mutate(Category = if_else(Category=="Groceries" & Amount < 20, "Food & Drink", Category)) |>
  dplyr::mutate(SubCategory=if_else(Category=="Groceries" & SubCategory=="NA", "Other", SubCategory)) |>
  dplyr::mutate(SubCategory = if_else(Category=="Gas", "Gas", SubCategory)) |>
  dplyr::mutate(Category = if_else(Category=="Gas", "Car", Category)) |>
  mutate(SubCategory = if_else(Category=="Food & Drink", "Uncategorized", SubCategory)) |>
  dplyr::mutate(Amount=round(Amount,0)) |>
  dplyr::mutate(Category = if_else(Category=="Professional Services", "Home", Category)) |>
  dplyr::mutate(SubCategory = if_else(Category=="Professional Services", "Admin", SubCategory)) |>
  dplyr::select(ID, Corrected, `Transaction Date`, Description, Category, SubCategory, Amount, Source, ToDelete, SupercedesTrip, Memo, Type)

return(dfExpenses2)

}

#' @export
EHFinances_WriteOrOpenOverrideFile <- function(dfExpenses, Folder, AlreadyWritten = TRUE) {

if(!AlreadyWritten)
{

  write_csv(dfExpenses, paste0("D:\\RStudio\\Finances\\AccountDownloads\\", Folder, "\\Overrides_", Folder, "p.csv"))
  dfOverrides <- read_csv(paste0("D:\\RStudio\\Finances\\AccountDownloads\\", Folder, "\\Overrides_", Folder, "p.csv"), na = c(""))

} else {

  dfOverrides <- read_csv(paste0("D:\\RStudio\\Finances\\AccountDownloads\\", Folder, "\\Overrides_", Folder, "r.csv"), na = c(""))
}

  return(dfOverrides)

}

#' @export
EHFinances_CreateShockAndExpenseDFs <- function(dfExpenses) {


  dfExpenses2 <- dfExpenses |>
    dplyr::filter(Category!="Ruby" & Category != "Renovation" & Category != "To Delete" & Category != "Income Taxes")
  dfRuby <- dfExpenses |>
    dplyr::filter(Category=="Ruby")
  dfRenovation <- dfExpenses |>
    dplyr::filter(Category=="Renovation")
  dfIncomeTaxes <- dfExpenses |>
    dplyr::filter(Category=="Income Taxes")

  liAccounts=list()
  liAccounts[[1]] <- dfExpenses2
  liAccounts[[2]] <- dfRuby
  liAccounts[[3]] <- dfRenovation
  liAccounts[[4]] <- dfIncomeTaxes

  return (liAccounts)

}

#' @export
EHFinances_FilterByCategory <- function(dfExpenses, xCategory) {

  dfCat <- dfExpenses |>
    dplyr::filter(Category==xCategory)

  return(dfCat)

}

#' @export
EHFinances_FilterBySubCategory <- function(dfExpenses, xSubCategory) {

  dfCat <- dfExpenses |>
    dplyr::filter(SubCategory==xSubCategory)

  return(dfCat)

}

EHFinances_ConvertAmazonPages <- function(Folder) {


  get_html_list <- function(Folder) {
    folder_path <- paste0("D:\\RStudio\\Finances\\AccountDownloads\\", Folder)
    list.files(folder_path, pattern = "^Details[0-9]+\\.html$", full.names = TRUE)
  }

  vPages <- get_html_list(Folder)

  dfTotal =  data.frame(matrix(ncol = 7, nrow = 0))
  colnames(dfTotal) <- c("Description", "Amount", "`Transaction Date`", "TotalAmount", "Ruby", "Category", "SubCategory")

  for(i in 1:length(vPages)) {
    Sys.sleep(2)
    htmlPage <- read_html(vPages[[i]])

    dfOrders <- htmlPage %>%
      html_nodes("div.a-fixed-left-grid-col.a-col-right") %>%
      map_df(function(x) {
        data.frame(
          Description = x %>% html_element('div[data-component="itemTitle"]') %>% html_text(trim = TRUE),
          Amount = x %>% html_node("span.a-offscreen") %>% html_text(trim = TRUE)
        )
      })

    dDate <- anydate(htmlPage |> html_node('div[data-component="orderDate"]') %>% html_text(trim = TRUE))
    dTotalAmount <- str_sub(htmlPage |> html_node('div[data-component="orderSummary"]') %>% html_text(trim = TRUE), -10)
    sRuby <- htmlPage |> html_node('div[data-component="orderSummary"]') %>% html_text(trim = TRUE)
    bRuby <- if_else(str_detect(sRuby, "Yale University") | str_detect(sRuby, "Ruby"), 1, 0)

    dfOrders2 <- dfOrders |>
      dplyr::filter(!is.na(Description) & !is.na(Amount) & Description!="") |>
      dplyr::mutate(Amount=as.numeric(parse_number(Amount))) |>
      mutate(`Transaction Date` = as.Date(dDate, format = "%m/%d/%Y")) |>
      mutate(TotalAmount=as.numeric(parse_number(dTotalAmount))) |>
      mutate(Ruby = bRuby) |>
      mutate(SubCategory = "NA") |>
      mutate(Category="Shopping")

    TotalToAdd <- (dfOrders2[1,4] - sum(dfOrders2$Amount))/nrow(dfOrders2)

    dfOrders3 <- dfOrders2 |>
      mutate(Amount=Amount+TotalToAdd)

    dfTotal <- rbind(dfOrders3, dfTotal)

    dfTotal2 <- dfTotal |>
      mutate(Description = paste("AMAZON:", Description)) |>
      dplyr::filter(EHFinances_TestIfDateIsInRange(xDate=`Transaction Date`, Folder)) |>
      dplyr::select(`Transaction Date`, Description, Amount, Ruby, Category, SubCategory)

  }

  return (dfTotal2)
}

#' @export
EHFinances_AssignShoppingCategories <- function(dfx) {

vShoppingSewing <- c("TARA FAUGHNAN", "MICHAELS", "SEW MODERN", "sewciety", "TAMI RAND", "FABRIC", "SILKS", "Fabric", "Quilt", "quilt")
vs <- str_c(vShoppingSewing, collapse = "|")

vShoppingHouse <- c("soap", " meal ", "SodaStream", "PAGE HARDWARE", "TARGET", "Etsy", "FRAME SHOP", "HOME DEPOT", "CONTAINER STORE", "FRAME SHOP", "Sheets", "sheets", "Pillow", "pillow", "Floor", "floor", "Candle", "candle", "Duvet", "duvet", "Kitchen", "Kitchen", "Bedroom", "bedroom", "Laundry", "laundry", "Bathroom", "bathroom", "Fridge", "fridge", "Toaster", "toaster", "Pantry", "pantry", "Furniture", "furniture", "bowl", "bowl", "etsy")
vh <- str_c(vShoppingHouse, collapse = "|")

vShoppingBooks <- c("BOOK", "Book", "book", "Kindle", "KINDLE", "kindle")
vbook <- str_c(vShoppingBooks, collapse = "|")

vShoppingClothes <- c("Leslie Oneill", "POSHMARK", "HUDSON", "LANDS END", "CLINTON CROSS", "DSW", "sukara", "MIZ MOOZ", "UNIQLO", "ZAPPOS", "ABERCROMBIE", "ZANNA", "EILEEN FISHER", "WINDSOR FASHIONS", "CLOSET", "Shirt", "shirt", "Pants", "pants", " Pant ", " pant ", " Pant,", " pant,", "Panty", "panty","Shoes", "shoes", "Socks", "socks", "Boot", "boot", "Wallet", "wallet", "Purse", "purse", "Glasses", "glasses", "Underwear", "underwear", "Underpants", "underpants", "Briefs", "briefs", "Belt", "belt", " hat ", " Hat ", "hat, ", "Hat, ", "Shorts", "shorts", "Costume", "costume", "Glove", "glove", "Sneakers", "sneakers", "Slipper", "slipper", " Watch ", " watch ", " Watch,", " watch,")
vc <- str_c(vShoppingClothes, collapse = "|")

vShoppingBirding <- c("AUDUBON", "BIRDS", "Bird", "bird")
vbird <- str_c(vShoppingBirding, collapse = "|")

vShoppingBoats <- c("RIVER CONN", "LIGHT CRAFT", "KOKATAT", "WEST MARINE", "MARINA", "Kayak", "kayak", "Drysuit", "drysuit")
vboat <- str_c(vShoppingBoats, collapse = "|")

vTravel <- c("Luggage", "luggage", "Travel", "travel", "Uber", "uber")
vtra <- str_c(vTravel, collapse = "|")

vElectronics <- c("Adapter", "adapter", "Phone", "phone", "iPad", "Monitor", "monitor", "Cable", "cable", "Cord", "cord", "Batteries", "batteries", "Hard Drive", "hard drive", "Camera", "camera", "Tablet", "tablet", "Compressed Air", "compressed air", "Microscope", "microscope")
vele <- str_c(vElectronics, collapse = "|")

vToiletries <- c(" curls ", "Biossance", "Repellent", "repellent", "Tooth", "tooth", "Floss", "floss", "Bandaid", "bandaid", "Bug", "bug", "Earplug", "earplug", "Dramamine", "dramamine", "Capsule", "capsule", "Supplement", "supplement", "Shampoo", "shampoo", "Conditioner", "conditioner")
vtoi <- str_c(vToiletries, collapse = "|")

vOffice <- c("Printer", "printer", "Ballpoint", "ballpoint", "Gel pen", "gel pen", "Gel Pen", "Label", "label",  "Markers", "markers", "Magnet", "magnet", "Glue", "glue", "Tape", "tape" )
voff <- str_c(vOffice, collapse = "|")

vOutdoors <- c("Hike", "hike", "Hiking", "hiking", "Bik", "bik", "Water Bottle", " Tent ", " tent ", " tent,", "weights", "Weights", "Skeleton", "skeleton", "camping", "Camping", "Massage", "massage", "wrist", "Wrist" )
vout <- str_c(vOutdoors, collapse = "|")

dfShopping2 <- dfx |>
  mutate(SubCategoryx = case_when(
    str_detect(Description, regex(vs, ignore_case = TRUE)) ~ "Sewing",
    str_detect(Description, regex(vh, ignore_case = TRUE)) ~ "House",
    str_detect(Description, regex(vbook, ignore_case = TRUE)) ~ "Books",
    str_detect(Description, regex(vc, ignore_case = TRUE)) ~ "Clothes",
    str_detect(Description, regex(vbird, ignore_case = TRUE)) ~ "Birding",
    str_detect(Description, regex(vboat, ignore_case = TRUE)) ~ "Boats",
    str_detect(Description, regex(vtra, ignore_case = TRUE)) ~ "Travel",
    str_detect(Description, regex(vele, ignore_case = TRUE)) ~ "Electronics",
    str_detect(Description, regex(vtoi, ignore_case = TRUE)) ~ "Toiletries",
    str_detect(Description, regex(voff, ignore_case = TRUE)) ~ "Office",
    str_detect(Description, regex(vout, ignore_case = TRUE)) ~ "Outdoors",
    TRUE ~ SubCategory)) |>
  mutate(SubCategoryx = if_else(is.na(SubCategoryx), "NA", SubCategoryx))

  dfShopping3 <- dfShopping2 |>
    mutate(SubCategory = if_else(Category=="Shopping", SubCategoryx, SubCategory))

return(dfShopping3)
}

#' @export
EHFinances_CreateDfForShoppingAnalysis <- function(dfExpenses, Folder) {

  dfAmazon <- EHFinances_ConvertAmazonPages(Folder) |>
    dplyr::filter(!is.na(Amount)) |>
    dplyr::select(`Transaction Date`, Description, Amount, Ruby, SubCategory, Category)

  dfShop<- dfExpenses |>
    dplyr::filter(Category=="Shopping") |>
    dplyr::filter(!str_detect(Description, regex("Amazon", ignore_case = TRUE))) |>
    dplyr::mutate(Ruby=0) |>
    dplyr::select(`Transaction Date`, Description, Amount, Ruby, SubCategory, Category)

  dfBoth <- bind_rows(dfShop, dfAmazon)

  dfBoth2 <- dfBoth |>
    mutate(xScale = case_when(
      Amount <= 0 ~ "1: Refund",
      Amount <= 50 ~ "2: Under 50",
      Amount <= 100 ~ "3: 51 - 100",
      Amount <= 250 ~ "4: 101 - 250",
      Amount <= 500 ~ "5: 251 - 500",
      Amount <= 100000 ~ "6: 501 +",
      TRUE ~ "7: Other")) |>
    mutate(`Transaction Date` = anydate(`Transaction Date`))

df3 <- EHFinances_AssignShoppingCategories(dfBoth2) |>
  dplyr::select(`Transaction Date`, Description, Amount, Ruby,  Category, SubCategory, xScale)

  return (df3)

}

#' @export
EHFinances_CreateYtdDfs <- function(Folder) {

nMonths <- as.numeric(substr(Folder, nchar(Folder) - 1, nchar(Folder)))
year <- as.character(substr(Folder, 1, 2))

dfq <- read_csv(paste0("D:\\RStudio\\Finances\\AccountDownloads\\", year, "01\\Overrides_", year, "01r.csv"), na = c(""))

if(nMonths>1) {
  for(i in 2:nMonths) {
    sMonth <- sprintf("%02d", i)

    dfq2 <- read_csv(paste0("D:\\RStudio\\Finances\\AccountDownloads\\", year, sMonth, "\\Overrides_", year, sMonth, "r.csv"), na = c(""))

    dfq <- rbind(dfq, dfq2)

  }
}

  return(EHFinances_CreateShockAndExpenseDFs(dfq))
}

EHFinances_BudgetAnalysisDF <- function(df, Folder, ytd=FALSE) {

  if(!ytd) {
  dfBudgetTargets <- EHFinances_ImportBudgetTargets(Folder)
  } else     {
  nMonths <- as.numeric(substr(Folder, nchar(Folder) - 1, nchar(Folder)))
  dfBudgetTargets <- read_csv(paste0("D:\\RStudio\\Finances\\AnnualBudgetTargets.csv")) |>
  mutate(Amount=Amount*nMonths)
  }

dfBudgetTargets2 <- dfBudgetTargets |>
  dplyr::arrange(Category) |>
  dplyr::rename(Amount_Budget=Amount)

dfExpensesReviewedCategories <- df |>
  group_by(Category) |>
  dplyr::summarize(Amount = sum(Amount)) |>
  dplyr::rename(Amount_Spent=Amount)

CategoriesWithZero <- as.data.frame(anti_join(dfBudgetTargets2, dfExpensesReviewedCategories, by = "Category")) |>
  mutate(Amount_Spent=0) |>
  dplyr::select(Category, Amount_Spent)

dfx1 <- rbind(dfExpensesReviewedCategories, CategoriesWithZero) |>
  dplyr::arrange(Category) |>
  dplyr::rename(Category_Spent = Category)

dfNew <- cbind(dfBudgetTargets2, dfx1)

dfNew2 <- dfNew |>
  mutate(Differential = Amount_Budget-Amount_Spent)  |>
  dplyr::select(Category, Amount_Budget, Amount_Spent, Differential)

return (dfNew2)

}


a <- EHFinances_BudgetAnalysisPlot <- function(df, Folder, ytd=FALSE) {

  if(!ytd) {
  xtitle <- as.character(Folder)
  } else {
  xtitle <- "YTD"
  }

df <- df |>
  dplyr::rename(spent=Amount_Spent, budget=Amount_Budget, category=Category)

df_plot <- df %>%
  mutate(
    status = ifelse(spent > budget, "Over budget", "Under budget")
  ) %>%
  arrange(budget) %>%
  mutate(category = factor(category, levels = category))

a <- ggplot(df_plot, aes(x = category)) +

  # Budget (background bar)
  geom_col(aes(y = budget),
           fill = "grey80",
           width = 0.7) +

  # Spent (foreground bar)
  geom_col(aes(y = spent, fill = status),
           width = 0.3) +
  ggtitle(paste0("Spending vs Budget, ", xtitle)) +
  coord_flip() +

  scale_fill_manual(values = c(
    "Under budget" = "#33a0a0",
    "Over budget" = "#c76030"
  )) +

  labs(
    x = "",
    y = "Amount",
    fill = ""
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank())

return (a)
}

#' @export
EHFinances_CreateBudgetAnalysisDFs <- function(df, df_ytd, Folder) {

  li = list()
  li[[1]] <- EHFinances_BudgetAnalysisDF(df, Folder, ytd=FALSE)
  li[[2]] <- EHFinances_BudgetAnalysisDF(df_ytd, Folder, ytd=TRUE)
  li[[3]] <- EHFinances_BudgetAnalysisPlot(li[[1]], Folder, ytd=FALSE)
  li[[4]] <- EHFinances_BudgetAnalysisPlot(li[[2]], Folder, ytd=TRUE)

  return(li)
}

#' @export
EHFinances_CreateTotalsTable <- function(dfExpenses, dfExpenses_ytd, dfRuby, dfRuby_ytd, dfRenovation, dfRenovation_ytd, dfIncomeTaxes, dfIncomeTaxes_ytd, Folder) {

  mat2 <- matrix(c(sum(dfExpenses$Amount), sum((dfRuby |> filter(SubCategory == "Tuition Etc."))$Amount), sum((dfRuby |> filter(SubCategory != "Tuition Etc."))$Amount), sum(dfRenovation$Amount),  sum(dfIncomeTaxes$Amount),
                   sum(dfExpenses_ytd$Amount), sum((dfRuby_ytd |> filter(SubCategory == "Tuition Etc."))$Amount), sum((dfRuby_ytd |> filter(SubCategory != "Tuition Etc."))$Amount), sum(dfRenovation_ytd$Amount),  sum(dfIncomeTaxes_ytd$Amount)), ncol=2, byrow=FALSE)


  colnames(mat2) <- c(paste0("Amount_", Folder), "Amount_YTD")
  rownames(mat2) <- c("Total Expenses:", "Total Ruby Tuition:", "Ruby Non Tuition:", "Renovation:", "Income Taxes:")

  dfMat <- as.data.frame(mat2)

  tab_custom <- kable(dfMat,  caption = "Monthly Budget Summary", format.args = list(big.mark = ","), digits = 0) |>
    kable_styling(full_width = FALSE, position = "center",
                  latex_options = c("striped", "hold_position"))

  return (tab_custom)

}

#' @export
EHFinances_CreateTopSpendingTable <- function(df, xCategory, Folder, ytd=FALSE, fontSize=9) {

if (ytd) {
  cap <- paste0("Top 15 Expenditures, YTD")
} else {
  cap <- paste0("Top 15 Expenditures, ", Folder)
}

a <- df %>%
  dplyr::filter(Category==xCategory) |>
  dplyr::select(`Transaction Date`, SubCategory, Description, Amount) |>
  arrange(desc(Amount)) %>%
  slice_head(n = 15) %>%
  mutate(Amount = dollar(Amount)) %>%
  kable(
    col.names = c("Transaction Date", "SubCategory", "Description", "Amount"),
    caption = cap,
    align = c("l", "l", "l", "r") |>
    column_spec(3, width = "10em")
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE,
    position = "center",
    font_size=fontSize
  )

return (a)
}

#' @export
EHFinances_CreateTimePlot <- function(df_ytd, dfBudget, xCategory) {

  EH_Turquoise = "#33a0a0"
  EH_Cream = "#FFFEE0"
  EH_Squash = "#c95b0c"
  EH_LabelColor = "#5d3f6a"

  Budget2 <- dfBudget |>
    dplyr::filter(Category==xCategory)

  Bu <- Budget2[1,2]


  df <- df_ytd |>
    dplyr::filter(Category==xCategory) |>
    group_by(month(`Transaction Date`)) |>
    dplyr::summarize(Expenditures=sum(Amount)) |>
    dplyr::rename(Month=1) |>
    mutate(Budget=Bu)

  a <- ggplot(df, aes(x = Month)) +
    geom_line(aes(y = Expenditures, color = "Expenditures"), size = 1) +
    geom_line(aes(y = Budget, color = "Budget"), size = 2) +
    geom_point(aes(y = Expenditures, color = EH_LabelColor), size = 3) +
    theme(panel.background = element_rect(fill = EH_Cream, colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_color_manual("", values = c("Expenditures" = EH_Turquoise, "Budget" = EH_Squash)) +
    scale_x_continuous(breaks = function(x) seq(ceiling(min(x)), floor(max(x)), by = 1)) +
    ggtitle("Expenditures Over Time") +
    scale_y_continuous(limits = c(0, NA))

  return(a)

}

#' @export
EHFinances_CategoryDetails <- function(dfExpensesReviewed, dfExpensesReviewed_YTD, dfBudget, xCategory, Folder) {

  a <- EHSummarize_CategoryByTotal_ReturnsSingleTable(dfExpensesReviewed |>  dplyr::filter(Category==xCategory) |> dplyr::select(SubCategory, Amount), xfill =EH_Squash, rectfill = EH_Cream, font_size = 9, xtitle = xCategory)
  b <- EHFinances_CreateTimePlot(dfExpensesReviewed_YTD, dfBudget, xCategory)

  grid.arrange(a,b,ncol=2)

  EHFinances_CreateTopSpendingTable(dfExpensesReviewed, xCategory, Folder, fontSize=8)

}

