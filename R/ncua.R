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
    } else {
        i <- str_length("QCR")
        yy <- str_sub(bn, i + 1, i + 4)
        mm <- str_sub(bn, i + 5, i + 6)
    }    
    crdir <- file.path(ncuaDir, str_c(yy, mm))
    unzip(qcr, exdir = crdir)
}

# Acct_Des.txt and FOICUDES.txt: accounts and table descripters
# FOICU.txt: CU identifers
# FS*.txt: account tables


read.5300cr <- function(path) {
    if (!file.exists(file.path(path))) {
        x <- dirname(path)
        y <- basename(x)
        yy <- str_sub(y, 1, 4)
        mm <- str_sub(y, 5, 6)
        ff <- as.Date(as.yearmon(str_c(yy, mm, sep = "-")), frac = 1)
        
        url1 <- "https://www.ncua.gov/DataApps/Documents/"
        url2 <- "https://www.ncua.gov/analysis/Pages/call-report-data/reports/5300-call-report-data-files/"
        
        if (ff <= "2015-03-31") {
            url.zip <- str_c(url1, "QCR",
                             yy, mm,
                             ".zip")
        } else {
            url.zip <- str_c(url2,
                             "Call-Report-Data-",
                             str_c(yy, mm, sep = "-"),
                             ".zip")
        }
        x <- download.5300cr(url.zip)
    }
    fread(path)
}

fn <- file.path(ncuaDir, "201606", "FOICU.txt")
DT1 <- read.5300cr(fn)
fn <- file.path(ncuaDir, "201503", "FOICU.txt")
DT2 <- read.5300cr(fn)
