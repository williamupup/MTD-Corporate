setwd("C:/Users/Haonan/Desktop/MTD PDI Project/Data")
library(openxlsx)
rawsheet <- read.xlsx( "Monthly Warranty Report_wDetailed_SN_region_data-en-us.xlsx" , sheet = 2)

rawdata <- read.csv("importante.csv", header = T)

emptyrm <- function(x){
    rmlist <- 0
    for (i in 1:dim(x)[1]){
        if (is.na(x[i,7]) == TRUE) {
            rmlist <- c(rmlist, i)
        }
        
    }
    Temp <- x[-rmlist,]
    return(Temp)
}
rawdata <- emptyrm(rawdata)
##Remove all the empty rows. There raws were created when transforming the original data from .xlsx to .csv. One of the characteristic of
the empty row is that its 7st column is "NA"
