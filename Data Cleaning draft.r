setwd("~/Desktop/MTD PDI Project/Data")
library(openxlsx)
library(dplyr)
library(ggplot2)
#rawsheet2 <- read.xlsx( "Monthly Warranty Report_wDetailed_SN_region_data-en-us.xlsx" , sheet = 2)

####################################################################################
#For the refer data set
refer <- read.xlsx("Model reference.xlsx", startRow = 2)
table(refer$Item.Platform)

UV <- refer[grep("Utility Vehicle", refer$Item.Platform),]
UVuni <- UV[!duplicated(UV$Item.Series),]
UVpos12 <- refer[grep("^37", refer$Item.Number),]
#All UVs are 37, but not all 37s are UV

Commercial <- refer[grep("Commercial", refer$Item.Platform),]
Comuni <- Commercial[!duplicated(Commercial$Item.Series),]  #Unique platform$series
aaa <- Commercial[Commercial$Item.Series == "Tank",]
unique(substr(Commercial[,1],5,5)) #How many unique 5th character in the Commercial data set
unique(substr(Commercial[Commercial$Item.Series == "Tank",1],5,5)) #5th character of Tank
View(Commercial[grep("53..2", Commercial$Item.Number),]) #When 5th of character is 2, return the item platform
#

RZTZF <- refer[grep("Residential Z", refer$Item.Platform),]
RZTZFuni <- RZTZF[!duplicated(RZTZF$Item.Series),]  
unique(substr(RZTZF[RZTZF$Item.Series == "RZT",1],5,5))
View(RZTZF[grep("17..C", RZTZF$Item.Number),])
View(RZTZF[grep("17..D", RZTZF$Item.Number),])
#

XT3 <- refer[grep("XT3", refer$Item.Series),]
View(RZTZF[grep("17..C", RZTZF$Item.Number),])
View(refer[grep("^14..3", refer$Item.Number),])
unique(substr(XT3[XT3$Item.Series == "XT3 GT",1],5,5))
#

View(refer[grep("^13", refer$Item.Number),])
#XT1 and XT2 is quite weird cuz don't have them. There're NX15 & NX9 for 13A & 139


###################################################################################
#For sales data set
sales <- read.xlsx("IR Ships 2012-2016 by Dealer and Platform.xlsx", sheet = 1)
dealer <- sales[!duplicated(sales$Customer.Number),c(2,3,5,6)]
dealer[grep("LES EQUIPEMENTS ADRIEN PHANEUF INC.", dealer$Customer.Name),] #To verify one dealer has multiple numbers

#Order the dealers by Shipped LC
PDIdealer <- sales[sales$SBU.Division == 73,]
NPDIdealer <- sales[sales$SBU.Division == 81,]
PDIorder <- with(PDIdealer, tapply(Shipped.LC, Customer.Name, sum))
PDIorder <- PDIorder[order(PDIorder, decreasing = T)]
NPDIorder <- with(NPDIdealer, tapply(Shipped.LC, Customer.Name, sum))
NPDIorder <- NPDIorder[order(NPDIorder, decreasing = T)]
remove(PDIdealer, NPDIdealer)
NPDIorder <- as.data.frame(NPDIorder)
PDIorder <- as.data.frame(PDIorder)

PDIorder[,2] <- PDIorder[,1]
names(PDIorder)[2] <- "PDIorder"
PDIorder[,1] <- rownames(PDIorder)
names(PDIorder)[1] <- "Dealername"
PDIorder <- rbind(PDIorder,c("Others", sum(PDIorder$PDIorder)-sum((PDIorder$PDIorder)[1:5])))


###################################################################################
#For Warranty data set
rawdata <- read.csv("importante.csv", header = T, na.strings = c("","NA"))

#THis function will eliminate the NA rows caused by the data set transformation from .xslx to .csv
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

#Add values for the column POS1,2, POS5, dealer, state
for (i in 2:dim(rawdata)[1]){
    if (is.na(rawdata[i,1]) == T) {
        rawdata[i,c(2,3,4,10,11)] <- rawdata[i-1,c(2,3,4,10,11)] #I might want to fill year col for the rf function
    }
}

#Remove 14 NA values from State
rawdata <- rawdata[complete.cases(rawdata$Repair.Dealer.State),]
# verify <- rawdata[is.na(rawdata$Repair.Dealer.State) == T, ]  #Varify if the state is NA, then the dealer is NA

#Remove Unknown rows from Model.Number, 1025 rows
rawdata <- rawdata[-grep("Unknown", rawdata$Model.Number),]

#Remove "NOMODEL" rows from Model.Number, 712 rows
rawdata <- rawdata[-grep("NOMODEL", rawdata$Model.Number),]


#Extract unreadable dealer name, 7rows
unread <- rawdata[-grep("", rawdata$Repair.Dealer.Name),]

#to numeric from factor
rawdata[,1] <- as.numeric(levels(rawdata[,1]))[rawdata[,1]]
#rawdata[,3] <- as.numeric(levels(rawdata[,3]))[rawdata[,3]]
#rawdata[,4] <- as.numeric(levels(rawdata[,4]))[rawdata[,4]]
rawdata[,38] <- as.numeric(levels(rawdata[,38]))[rawdata[,38]]
rawdata$Repair.Dealer.Name <- as.character(rawdata$Repair.Dealer.Name)

#to character from factor
rawdata[,c(2,5,9,10,11,12,23,24,25,26,27,29,30,31,37,39)] <- as.character(rawdata[,c(2,5,9,10,11,12,23,24,25,26,27,29,30,31,37,39)])
#->character -> numeric() : 1,3,4,38
#difference starts from Failure.Description


###################################################################################
#PDI DATA SET
PDI <- rawdata[grep("PDI", rawdata$Failure.Description),]


###################################################################################
#Draft
for (i in 1:dim(rawdata)[2]){
         class(tmp[,i])
    }

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



