library(dplyr)
library(ggplot2)
library(gridExtra)
library(roxygen2)
library(tidyverse)

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

#' @export
EHFinances_ImportAmazonOrders <- function(Folder)
{

  dfCategories <- read_csv("D:\\RStudio\\Finances\\AmazonOrders\\Retail.OrderHistory.1\\Retail.OrderHistory.1.csv") |>
  dplyr::select(`Order ID`, `Order Date`, `Total Owed`, `Payment Instrument Type`, `Order Status`, `Shipping Address`, `Product Name`, ASIN) |>
  dplyr::filter(year(`Order Date`)==EHFinances_RetrieveYearAndMonth(Folder)[[1]], month(`Order Date`)==EHFinances_RetrieveYearAndMonth(Folder)[[2]])

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

EHFinances_TestIfDataIsInRange <- function(xDate, Folder) {

  x <- ifelse(year(xDate)==EHFinances_RetrieveYearAndMonth(Folder)[[1]], month(xDate)==EHFinances_RetrieveYearAndMonth(Folder)[[2]], TRUE, FALSE)
  return (x)

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
  ungroup()

dfExpenses3 <- dfExpenses2 |>
  mutate(SupercedesTrip=if_else(is.na(SupercedesTrip), 0, SupercedesTrip)) |>
  mutate(Corrected=0) |>
  dplyr::filter(year(`Transaction Date`)==EHFinances_RetrieveYearAndMonth(Folder)[[1]], month(`Transaction Date`)==EHFinances_RetrieveYearAndMonth(Folder)[[2]])
  #dplyr::filter(EHFinances_TestIfDataIsInRange(xDate=`Transaction Date`, Folder))

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
  dplyr::select(ID, Corrected, `Transaction Date`, Description, Category, SubCategory, Amount, Source, ToDelete, SupercedesTrip, Memo, Type)

return(dfExpenses2)

}

#' @export
EHFinances_WriteOrOpenOverrideFile <- function(dfExpenses=data.frame(), Folder, xType="Open") {

if(xType=="Create")
{

  write_csv(dfExpenses, paste0("D:\\RStudio\\Finances\\Overrides_", Folder, ".csv"))

  return("Completed")

} else {

  dfOverrides <- read_csv(paste0("D:\\RStudio\\Finances\\Overrides_", Folder, ".csv"), na = c(""))

  return(dfOverrides)
  }
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

EHFinances_ConvertAmazonPages <- function(vPages, Folder) {

  dfTotal =  data.frame(matrix(ncol = 4, nrow = 0))
  colnames(dfTotal) <- c("Transaction_Date", "Memo", "Amount", "Description")

  for(i in 1:length(vPages)) {

    Sys.sleep(2)
    htmlPage <- read_html(vPages[[i]])
    dfOrders <- htmlPage %>%
      html_nodes("div.a-box-group") %>%
      map_df(function(x) {

        data.frame(
          Transaction_Date = x %>% html_node(".order-header__header-list-item") %>% html_text(trim = TRUE),
          Memo   = x %>% html_node(".yohtmlc-order-id") %>% html_text(trim = TRUE),
          Amount      = x %>% html_node(".a-column.a-span2") %>% html_text(trim = TRUE),
          Description       = x %>% html_node(".yohtmlc-product-title") %>% html_text(trim = TRUE)
        )
      })

    dfx <- dfOrders |>
      mutate(Description = paste("AMAZON:", Description)) |>
      mutate(Transaction_Date = mdy(date_str <- str_extract(Transaction_Date,
                                                            "(January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{1,2},\\s+\\d{4}"))) |>
      mutate(Memo = str_remove(Memo, "Order #")) |>
      mutate(Memo =  str_replace_all(Memo, " ", "")) |>
      mutate(Amount =  as.numeric(parse_number(Amount)))

    dfTotal <- rbind(dfx, dfTotal)

    dfTotal2 <- dfTotal |>
      dplyr::filter(year(Transaction_Date)==EHFinances_RetrieveYearAndMonth(Folder)[[1]], month(Transaction_Date)==EHFinances_RetrieveYearAndMonth(Folder)[[2]]) |>
      dplyr::rename(`Transaction Date` = Transaction_Date)
  }

  return (dfTotal2)
}

#' @export
EHFinances_AssignShoppingCategories <- function(dfx, vPages, Folder) {

vShoppingSewing <- c("TARA FAUGHNAN", "MICHAELS", "SEW MODERN", "sewciety", "TAMI RAND", "FABRIC", "SILKS", "Fabric", "Quilt", "quilt")
vs <- str_c(vShoppingSewing, collapse = "|")

vShoppingHouse <- c("PAGE HARDWARE", "TARGET", "Etsy", "FRAME SHOP", "HOME DEPOT", "CONTAINER STORE", "FRAME SHOP", "Sheets", "sheets", "Pillow", "pillow", "Floor", "floor", "Candle", "candle", "Duvet", "duvet", "Kitchen", "Kitchen", "Bedroom", "bedroom", "Laundry", "laundry", "Bathroom", "bathroom", "Fridge", "fridge", "Toaster", "toaster", "Pantry", "pantry", "Furniture", "furniture", "bowl", "bowl", "etsy")
vh <- str_c(vShoppingHouse, collapse = "|")

vShoppingBooks <- c("BOOK", "Book", "book", "Kindle", "KINDLE", "kindle")
vbook <- str_c(vShoppingBooks, collapse = "|")

vShoppingClothes <- c("POSHMARK", "HUDSON", "LANDS END", "CLINTON CROSS", "DSW", "sukara", "MIZ MOOZ", "UNIQLO", "ZAPPOS", "ABERCROMBIE", "ZANNA", "EILEEN FISHER", "WINDSOR FASHIONS", "CLOSET", "Shirt", "shirt", "Pants", "pants", " Pant ", " pant ", " Pant,", " pant,", "Panty", "panty","Shoes", "shoes", "Socks", "socks", "Boot", "boot", "Wallet", "wallet", "Purse", "purse", "Glasses", "glasses", "Underwear", "underwear", "Underpants", "underpants", "Briefs", "briefs", "Belt", "belt", " hat ", " Hat ", "hat, ", "Hat, ", "Shorts", "shorts", "Costume", "costume", "Glove", "glove", "Sneakers", "sneakers", "Slipper", "slipper", " Watch ", " watch ", " Watch,", " watch,")
vc <- str_c(vShoppingClothes, collapse = "|")

vShoppingBirding <- c("AUDUBON", "BIRDS", "Bird", "bird")
vbird <- str_c(vShoppingBirding, collapse = "|")

vShoppingBoats <- c("RIVER CONN", "LIGHT CRAFT", "KOKATAT", "WEST MARINE", "MARINA", "Kayak", "kayak", "Drysuit", "drysuit")
vboat <- str_c(vShoppingBoats, collapse = "|")

vTravel <- c("Luggage", "luggage", "Travel", "travel", "Uber", "uber")
vtra <- str_c(vTravel, collapse = "|")

vElectronics <- c("Adapter", "adapter", "Phone", "phone", "iPad", "Monitor", "monitor", "Cable", "cable", "Cord", "cord", "Batteries", "batteries", "Hard Drive", "hard drive", "Camera", "camera", "Tablet", "tablet", "Compressed Air", "compressed air", "Microscope", "microscope")
vele <- str_c(vElectronics, collapse = "|")

vToiletries <- c("Repellent", "repellent", "Tooth", "tooth", "Floss", "floss", "Bandaid", "bandaid", "Bug", "bug", "Earplug", "earplug", "Dramamine", "dramamine", "Capsule", "capsule", "Supplement", "supplement", "Shampoo", "shampoo", "Conditioner", "conditioner")
vtoi <- str_c(vToiletries, collapse = "|")

vOffice <- c("Printer", "printer", "Ballpoint", "ballpoint", "Gel pen", "gel pen", "Gel Pen", "Label", "label",  "Markers", "markers", "Magnet", "magnet", "Glue", "glue", "Tape", "tape" )
voff <- str_c(vOffice, collapse = "|")

vOutdoors <- c("Hike", "hike", "Hiking", "hiking", "Bik", "bik", "Water Bottle", " Tent ", " tent ", " tent,", "weights", "Weights", "Skeleton", "skeleton", "camping", "Camping", "Massage", "massage", "wrist", "Wrist" )
vout <- str_c(vOutdoors, collapse = "|")

dfShopping2 <- dfx |>
  mutate(SubCategory = case_when(
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
    TRUE ~ "NA"))

return(dfShopping2)
}

#' @export
EHFinances_CreateDfForShoppingAnalysis <- function(dfExpenses, vPages, Folder) {

  dfAmazon <- EHFinances_ConvertAmazonPages(vPages) |>
    dplyr::filter(!is.na(Amount)) |>
    dplyr::select(`Transaction Date`, Description, Amount, Memo)

  dfShop<- dfExpenses |>
    dplyr::filter(Category=="Shopping") |>
    dplyr::filter(!str_detect(Description, regex("Amazon", ignore_case = TRUE))) |>
    dplyr::select(`Transaction Date`, Description, Amount, Memo)

  dfBoth <- rbind(dfShop, dfAmazon)

  dfBoth2 <- dfBoth |>
    mutate(xScale = case_when(
      Amount <= 0 ~ "1: Refund",
      Amount <= 50 ~ "2: Under 50",
      Amount <= 100 ~ "3: 51 - 100",
      Amount <= 250 ~ "4: 101 - 250",
      Amount <= 500 ~ "5: 251 - 500",
      Amount <= 100000 ~ "6: 501 +",
      TRUE ~ "7: Other"))

df3 <- EHFinances_AssignShoppingCategories(dfBoth2, vPages, Folder)

  return (df3)

}


