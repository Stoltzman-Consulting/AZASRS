daily.test <- function(x, y, z = TRUE, write_to_excel = TRUE) {
  # x is a composite
  # y is a vector of sub composites or accounts that should sum to x
  # z is logical where TRUE == by.fund and FALSE == by.composite

  # dummy list for data frames
  check.list <- list()
  check.list[["ID"]] <- y

  if (z == TRUE) {
    suffix = ".by.portfolio.xlsx"
  } else {
    suffix = ".by.composite.xlsx"
  }

  # Loop for NAV Tests and return data
  for (i in c("e.mv", "b.mv", "cf")) {

    if (i == "e.mv") {
      type.name <- "EndingNAV"
    } else if (i == "b.mv") {
      type.name <- "BeginningNAV"
    } else if (i == "cf") {
      type.name <- "CashFlow"
    }

    composite.nav <- daily.data %>%
      filter(ID == x) %>%
      select(Date, ID, Composite = i)

    account.nav <- daily.data %>%
      filter(ID %in% unlist(y)) %>%
      select(Date, ID, i) %>%
      spread(key = ID, value = i) %>%
      replace(., is.na(.), 0) %>%
      mutate(Sum_by_ID = rowSums(.[, -1]))

    test.df <- account.nav %>%
      select(Date, Sum_by_ID) %>%
      left_join(composite.nav, by = "Date") %>%
      mutate(Delta = Composite - Sum_by_ID,
             Error = abs(Delta) > 250,
             Type = type.name) %>%
      select(Date, ID, Type, Composite, Sum_by_ID, Delta, Error) %>%
      filter(Date >= fytd.bgn)
    #mutate(Date = as.character(Date))

    check.list[[paste0(i, ".test.df")]] <- test.df

    # Return Check
    if (i == "e.mv") {
      e.mv.sum <- account.nav %>%
        select(Date, Sum_by_ID) %>%
        set_colnames(c("Date", paste0("ASRS_", type.name)))
    } else if (i == "b.mv") {
      b.mv.sum <- account.nav %>%
        select(Date, Sum_by_ID) %>%
        set_colnames(c("Date", paste0("ASRS_", type.name)))
    } else if (i == "cf") {
      cf.sum <- account.nav %>%
        select(Date, Sum_by_ID) %>%
        set_colnames(c("Date", paste0("ASRS_", type.name)))
    }
  }
  # Outside of the loop
  r.check <- daily.data %>%
    filter(ID == x) %>%
    select(Date, ID, r.StateStreet = r.day) %>%
    left_join(e.mv.sum, by = "Date") %>%
    left_join(b.mv.sum, by = "Date") %>%
    left_join(cf.sum, by = "Date") %>%
    mutate(r.ASRS = ASRS_EndingNAV / (ASRS_BeginningNAV + ASRS_CashFlow) - 1,
           Delta = r.StateStreet - r.ASRS,
           Error = abs(Delta) > .00001) %>%
    filter(Date >= fytd.bgn)

  check.list[["r.check"]] <- r.check

  if (write_to_excel == TRUE) {
    # Save Return Errors to same Excel Spreadsheet
    wb <- loadWorkbook(paste0(path.excel, x, suffix), create = TRUE)

    n.sheets <- length(getSheets(wb))
    if (n.sheets > 0) {
      for (i in 1:n.sheets) {
        clearSheet(wb, sheet = i)
      }
    }
    createSheet(wb, name = "IDs")
    writeWorksheet(wb, y, sheet = "IDs")

    createSheet(wb, name = "EndingNAV")
    writeWorksheet(wb, check.list[[2]], sheet = "EndingNAV")

    createSheet(wb, name = "BeginningNAV")
    writeWorksheet(wb, check.list[[3]], sheet = "BeginningNAV")

    createSheet(wb, name = "CashFlow")
    writeWorksheet(wb, check.list[[4]], sheet = "CashFlow")
    #setCellStyle(wb, sheet = "CashFlow", col= 1, row = 2, cellstyle = cs)

    createSheet(wb, name = "Return")
    writeWorksheet(wb, check.list[[5]], sheet = "Return")

    saveWorkbook(wb)
  }

  return(check.list)
}

# Daily Return Formula (beginning of day cash flow methodology):
# Daily Return = [Ending MV / (Beginning MV + Cash Flow)] - 1
calc.return <- function(mv.beg, mv.end, cf = 0){
  mv.end/(mv.beg + cf) - 1
}
