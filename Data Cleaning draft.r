setwd("~/Desktop/MTD PDI Project/Data")
library(openxlsx)
library(dplyr)
#rawsheet2 <- read.xlsx( "Monthly Warranty Report_wDetailed_SN_region_data-en-us.xlsx" , sheet = 2)

#THis function will eliminate the NA rows caused by the data set transformation from .xslx to .csv
refer <- read.xlsx("Model reference.xlsx", startRow = 2)
table(refer$Item.Platform)
aaa <- refer[grep("Utility Vehicle", refer$Item.Platform),]


rawdata <- read.csv("importante.csv", header = T, na.strings = c("","NA"))
empty_rm <- function(x){
    rmlist <- 0
    for (i in 1:dim(x)[1]){
        if (is.na(x[i,7]) == TRUE) {
            rmlist <- c(rmlist, i)
        }
        
    }
    Temp <- x[-rmlist,]
    return(Temp)
}
rawdata <- empty_rm(rawdata)

#This function will return the NA col index, I think the NAs derive from the same item different problems
na_list <-function(x) {
    nalist <- c()
    for (i in 1:dim(x)[2]){
        if (is.na(x[2,i]) == T){
            nalist <- c(nalist,i)
        }
    }
    return(nalist)
}
nalist <- na_list(rawdata)

#This loop will copy the upper row and paste it to the NAs in the certain cell
for (i in 2:dim(rawdata)[1]){
    if (is.na(rawdata[i,1]) == T) {
        rawdata[i,nalist] <- rawdata[i-1,nalist]
    }
}
remove(nalist)

#to numeric from factor
rawdata[,1] <- as.numeric(levels(rawdata[,1]))[rawdata[,1]]
rawdata[,3] <- as.numeric(levels(rawdata[,3]))[rawdata[,3]]
rawdata[,4] <- as.numeric(levels(rawdata[,4]))[rawdata[,4]]
rawdata[,38] <- as.numeric(levels(rawdata[,38]))[rawdata[,38]]

#to character from factor
rawdata[,c(2,5,9,10,11,12,23,24,25,26,27,29,30,31,37,39)] <- as.character(rawdata[,c(2,5,9,10,11,12,23,24,25,26,27,29,30,31,37,39)])
#->character -> numeric() : 1,3,4,38
#difference starts from Failure.Description

###################################################################################
#for (i in 1:dim(rawdata)[2]){
#         class(tmp[,i])
#    }
