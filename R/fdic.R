# https://www5.fdic.gov/idasp/advSearch_warp_download_all.asp

# https://www5.fdic.gov/sdi/download_large_list_outside.asp

# https://www5.fdic.gov/sod/download/*.zip

# 3rd tab: Branch Office Deposits

# Summary of Deposits (sod)

# https://catalog.data.gov/dataset/fdic-summary-of-deposits-sod-download-file


# FFIEC Central Data Repository's Public Data Distribution web site
# https://cdr.ffiec.gov/public/

library(data.table)
library(zoo)
library(stringr)

srcDataDir <- file.path(getwd(), "1.SourceData")
fdicDir <- file.path(srcDataDir, "fdic.gov")


download.sod <- function(url) {
    # download Summary of Deposits from fdic.gov
    
    bn <- basename(url)
    sod  <- file.path(fdicDir, bn)
    if(!dir.exists(fdicDir)) dir.create(fdicDir, recursive = T)
    download.file(url, destfile = sod, mode = "wb")
    
    yy <- str_sub(bn, 5, 8)
    sodir <- file.path(fdicDir, "sod")
    unzip(sod,
          files = str_c("ALL_", yy, ".csv"),
          exdir = sodir, junkpaths = T)
}


url.sod <- function(x) {
    # x: a integer representing Year
    # return the Summary of Deposits URL based reporting period x
    
    if(x < 1994) stop("Summary of Deposits before 1994 unavaiable!")
    
    sod.url <- "https://www5.fdic.gov/sod/download/"
    
    str_c(sod.url,
          "ALL_",
          as.integer(x),
          ".zip")
}


read.sod <- function(path) {
    if (!file.exists(file.path(path))) {
        bn <- basename(path)
        yy <- str_sub(bn, 5, 8) %>% strtoi()

        url.zip <- url.sod(yy)
        
        x <- download.sod(url.zip)
    }
    x <- fread(path, na.strings = c("", " "))
}

