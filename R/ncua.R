# https://www.ncua.gov/analysis/Pages/call-report-data/quarterly-data.aspx
# Call Report Quarterly Data
# The Call Report Data files are compressed (ZIP) files and must be uncompressed using a program compatible WinZip®. The data files are in comma delimited text file format that suitable for importing into a database or spreadsheet.
# All data listed on this page is final data.
# Data Notes
# Effective with the September 2016 cycle, we removed the following tables from the Zip file: FS220E, FS220CUSO, Acct-DescCUSO.txt. This information is no longer collected from credit unions.
# Effective with the March 2014, cycle two new tables, FS220J and FS220K, were added to the Zip file. Information on FS220J and FS220K is available in the Account Descriptions table (AcctDesc.txt).
# Effective with the September 2013, cycle two new tables, Grants and TradeNames, were added to the Zip file. Information on the Grants table is available in the Acct_DescGrants.txt file and information on TradeNames table is available in the Acct_DescTradeNames.txt file.
# Effective with the June 2013 cycle, a new table, FS220E, was added to the Zip file. Information on the FS220E table accounts is available in the Account Descriptions table (AcctDesc.txt).


# https://www.ncua.gov/DataApps/Documents/QCR199403.zip
#
# https://www.ncua.gov/DataApps/Documents/QCR201412.zip
#
# https://www.ncua.gov/DataApps/Documents/QCR201503.zip
#
# https://www.ncua.gov/analysis/Pages/call-report-data/reports/5300-call-report-data-files/Call-Report-Data-2015-06.zip
#
# https://www.ncua.gov/analysis/Pages/call-report-data/reports/5300-call-report-data-files/call-report-data-2017-06.zip

library(data.table)
library(zoo)
library(stringr)

srcDataDir <- file.path(getwd(), '1.SourceData')
ncuaDir <- file.path(srcDataDir, 'ncua.gov')


download.5300cr <- function(url) {
    # download 5300 Call Report from NCUA.gov
    
    bn <- basename(url)
    qcr <- file.path(ncuaDir, bn)
    if(!dir.exists(ncuaDir)) dir.create(ncuaDir, recursive = T)
    download.file(url, destfile = qcr, mode = "wb")

    if(grepl("^Call-Report-Data-", bn)) {
        i <- str_length("Call-Report-Data-")
        yy <- str_sub(bn, i + 1, i + 4)
        mm <- str_sub(bn, i + 6, i + 7)
    } else if(bn == "5300Data0613Final.zip") {
        yy <- "2013"
        mm <- "06"
    } else {
        i <- str_length("QCR")
        yy <- str_sub(bn, i + 1, i + 4)
        mm <- str_sub(bn, i + 5, i + 6)
    }    
    crdir <- file.path(ncuaDir, str_c(yy, mm))
    unzip(qcr, exdir = crdir, junkpaths = T)
}

# Acct_Des.txt and FOICUDES.txt: accounts and table descripters
# FOICU.txt: CU identifers
# FS*.txt: account tables

url.5300cr <- function(x) {
    # x: a Date object
    # return the call report URL based reporting period x
    
    if(class(x) != "Date") stop("Date object required!")
    if(!month(x) %in% seq(3, 12, 3)) stop("Month must be on of 3, 6, 9 or 12!")
    if(x < "1994-03-31") stop("Call report data before 1994-03-31 unavaiable!")
    
    url1 <- "https://www.ncua.gov/DataApps/Documents/"
    url2 <- "https://www.ncua.gov/analysis/Pages/call-report-data/reports/5300-call-report-data-files/"
    
    yy <- year(x)
    mm <- str_pad(month(x), width = 2, "left", pad = "0")
    if (x <= "2015-03-31") {
        str_c(url1, "QCR",
              yy, mm,
              ".zip")
    } else {
        str_c(url2,
              "Call-Report-Data-",
              str_c(yy, mm, sep = "-"),
              ".zip")
    }
}


read.5300cr <- function(path) {
    if (!file.exists(file.path(path))) {
        x <- dirname(path)
        bn <- basename(x)
        yy <- str_sub(bn, 1, 4)
        mm <- str_sub(bn, 5, 6)
        ff <- as.Date(as.yearmon(str_c(yy, mm, sep = "-")), frac = 1)
        
        url.zip <- url.5300cr(ff)
        
        x <- download.5300cr(url.zip)
    }
    x <- fread(path, na.strings = c("", " "))
    x[, CYCLE_DATE := as.IDate(CYCLE_DATE, format = "%m/%d/%Y")]
    x[CYCLE_DATE < "1998-06-30", CYCLE_DATE := as.IDate(str_replace(CYCLE_DATE, "^00", "19"))]
}
