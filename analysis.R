rosswar <- rf(dealer = "rosslyn", platform = "37", year = 2012:2016)
rossal <- sf(dealer = "rosslyn", platform = "UV")
rosswar$Total.Claim.Dollars <- as.character(rosswar$Total.Claim.Dollars)
rosswar$Total.Claim.Dollars <- gsub(",", "", rosswar$Total.Claim.Dollars)
rosswar$Total.Claim.Dollars <- as.numeric(rosswar$Total.Claim.Dollars)
ggplot(rosswar, aes(y = Total.Claim.Dollars, x = 1:dim(rosswar)[1])) + 
    geom_point() + 
    geom_hline(yintercept = 50, col = 2) + 
    labs(title = "Rosslyn Service Ltd, Platform UV", x = "Record", y = "Total Claim Dollars")


billwar <- rf(dealer = "billette", platform = "37")
billsal <- sf(dealer = "billette", platform = "UV")
billwar$Total.Claim.Dollars <- as.numeric(as.character(billwar$Total.Claim.Dollars))
ggplot(billwar, aes(y = Total.Claim.Dollars, x = 1:dim(billwar)[1])) + 
    geom_point() + 
    geom_hline(yintercept = 50, col = 2) + 
    labs(title = "Billette, Platform UV", x = "Record", y = "Total Claim Dollars")


#Find the part
partorder <- table(rawdata$Consolidated.ProdFailed.Part)[order(table(rawdata$Consolidated.ProdFailed.Part), decreasing = T)]
partorder <- as.data.frame(partorder)
partorder[,2] <- rownames(partorder)
names(partorder)[2] <- "partname"
part <- rawdata[rawdata$Consolidated.ProdFailed.Part == "725-1707",]
partPDI <- part[grep("PDI", part$Failure.Description),]
partNPDI <- part[-grep("PDI", part$Failure.Description),]

ggplot(partPDI, aes(y = Total.Failure.Cost, x = 1:dim(partPDI)[1])) + 
    geom_point(alpha = .5, color = 4) +
    geom_hline(yintercept = mean(partPDI$Total.Failure.Cost), col = 2)+
    labs(title = "Part 725-1707 in PDI", x = "Record", y = "Total Claim Dollars")

ggplot(partNPDI, aes(y = Total.Failure.Cost, x = 1:dim(partNPDI)[1])) + 
    geom_point(alpha = .25, color = 3) + 
    geom_hline(yintercept = mean(partNPDI$Total.Failure.Cost), col = 2)+
    labs(title = "Part 725-1707 Non-PDI", x = "Record", y = "Total Claim Dollars") + 
    coord_cartesian(ylim = 0:600)


##########################################################################################################
#PDI Cost comparason for UV
sort(unique(sales$Item.Platform))
salesUV <- sf(platform = "UV")
NPDIsalesUV <- salesUV[salesUV$SBU.Division == 81,]
PDIsalesUV <- salesUV[salesUV$SBU.Division == 73,]
range(NPDIsalesUV$Shipment.Fiscal.Year)  #2012:2016
sum(abs(NPDIsalesUV$Shipped.Units))     #421, there are negative values I don't know why
sum(abs(PDIsalesUV$Shipped.Units))      #41, there are negative values I don't know why
NPDIwarUV <- rf(platform = "37", year = 2012:2016, PDI = F)
PDIwarUV <- rf(platform = "37", year = 2012:2016, PDI = T)
sum(as.numeric(levels(NPDIwarUV$Total.Claim.Dollars))[NPDIwarUV$Total.Claim.Dollars], na.rm = T)   #118273.1
sum(as.numeric(levels(PDIwarUV$Total.Claim.Dollars))[PDIwarUV$Total.Claim.Dollars], na.rm = T) + 
    + (41 - nrow(PDIwarUV[PDIwarUV$Total.Failure.Cost == 50,])) * 50    #3532.29
118273.1/421
3532.29/41

###################################################################################################
#A Function to automaticlly generate necessary information
#1111111111111111111111111111111111111111111
info <- function(platform, pos,money){
    #Sales
    sales <- sf(platform = platform)
    NPDIsales <- sales[sales$SBU.Division == 81,]
    PDIsales <- sales[sales$SBU.Division == 73,]
    year <- range(NPDIsales$Shipment.Fiscal.Year)[1]:range(NPDIsales$Shipment.Fiscal.Year)[2]
    NPDIno <- sum(abs(NPDIsales$Shipped.Units))    
    PDIno <- sum(abs(PDIsales$Shipped.Units)) 
    
    #Warranty Claims
    NPDIwar <- rf(platform = pos, year = year, PDI = F)
    PDIwar <- rf(platform = pos, year = year, PDI = T)
    NPDItotalcost <- sum(as.numeric(levels(NPDIwar$Total.Claim.Dollars))[NPDIwar$Total.Claim.Dollars], na.rm = T)   
    PDItotalcost <- sum(as.numeric(levels(PDIwar$Total.Claim.Dollars))[PDIwar$Total.Claim.Dollars], na.rm = T) + 
    + (PDIno - nrow(PDIwar[PDIwar$Total.Failure.Cost == money,])) * money
    #asd <- c(NPDItotalcost/NPDIno, PDItotalcost/PDIno)
    asd <- c(year[1], year[length(year)],NPDIno,PDIno,NPDItotalcost,PDItotalcost,NPDItotalcost/NPDIno, PDItotalcost/PDIno)
    return(asd)
}

infoii <- function(pos, money, name){
    NPDIwar <- rf(platform = pos, year = 2012:2016, PDI = F)
    PDIwar <- rf(platform = pos, year = 2012:2016, PDI = T)
    PDIno <- nrow(PDIwar[PDIwar$Total.Failure.Cost == money,])
    NPDItotalcost <- sum(as.numeric(levels(NPDIwar$Total.Claim.Dollars))[NPDIwar$Total.Claim.Dollars], na.rm = T)   
    PDItotalcost <- sum(as.numeric(levels(PDIwar$Total.Claim.Dollars))[PDIwar$Total.Claim.Dollars], na.rm = T) 
    asd <- c(name, PDIno, NPDItotalcost, PDItotalcost)
    return(asd)
}

#22222222222222222222222222222222222222222
info <- function(platform, pos,money){
    #Sales
    sales <- sf(platform = platform)
    NPDIsales <- sales[sales$SBU.Division == 81,]
    PDIsales <- sales[sales$SBU.Division == 73,]
    year <- range(NPDIsales$Shipment.Fiscal.Year)[1]:range(NPDIsales$Shipment.Fiscal.Year)[2]
    NPDIno <- sum(abs(NPDIsales$Shipped.Units))    
    PDIno <- sum(abs(PDIsales$Shipped.Units)) 
    
    #Warranty Claims
    NPDIwar <- rf(platform = pos, year = year, PDI = F)
    PDIwar <- rf(platform = pos, year = year, PDI = T)
    NPDItotalcost <- sum(NPDIwar$Total.Failure.Cost, na.rm = T)     
    PDItotalcost <- sum(PDIwar$Total.Failure.Cost, na.rm = T) + 
        + (PDIno - nrow(PDIwar[PDIwar$Total.Failure.Cost == money,])) * money
    #asd <- c(NPDItotalcost/NPDIno, PDItotalcost/PDIno)
    asd <- c(year[1], year[length(year)],NPDIno,PDIno,NPDItotalcost,PDItotalcost,NPDItotalcost/NPDIno, PDItotalcost/PDIno)
    return(asd)
}

infoii <- function(pos, money, name){
    NPDIwar <- rf(platform = pos, year = 2012:2016, PDI = F)
    PDIwar <- rf(platform = pos, year = 2012:2016, PDI = T)
    NPDIno <- length(unique(NPDIwar$Serial.Number))
    #PDIno <- nrow(PDIwar[PDIwar$Total.Failure.Cost == money,])
    PDIno <- length(unique(PDIwar$Serial.Number))
    NPDItotalcost <- sum(NPDIwar$Total.Failure.Cost, na.rm = T)   
    PDItotalcost <- sum(PDIwar$Total.Failure.Cost, na.rm = T)  
    asd <- c(name, NPDIno, PDIno, NPDItotalcost, PDItotalcost)
    return(asd)
}

#This function applies for UV, RZT, 2-Force, Commercial Z, 
######################################################################################################
#Still lake NPDI total cost
residentialzinfo <- info(platform = "residential", pos = "17C", money = 25)[c(3,5,7)]
residentialz <- infoii(pos = "17C", money = 25, name = "RZT")
residentialz <- rbind(residentialz, infoii(pos = "172", money = 25, name = "RZT"))
residentialz <- rbind(residentialz, infoii(pos = "173", money = 25, name = "Z-Force"))
residentialz <- rbind(residentialz, infoii(pos = "175", money = 25, name = "Z-Force"))
residentialz <- rbind(residentialz, infoii(pos = "17D", money = 25, name = "Z-Force"))
residentialz <- as.data.frame(residentialz)
colnames(residentialz) <- c("Type", "PDI Number", "NPDI Total Cost", "PDI Total Cost")

commercialzinfo <- info(platform = "commercial", pos = "532", money = 30)[c(3,5,7)]
commercialz <- infoii(pos = "53E", money = 30, name = "Pro Z")
commercialz <- rbind(commercialz,infoii(pos = "532", money = 30, name = "Tank") )
commercialz <- rbind(commercialz,infoii(pos = "535", money = 30, name = "Commercial Z") )
commercialz <- rbind(commercialz,infoii(pos = "53D", money = 30, name = "Commercial Z") )
commercialz <- as.data.frame(commercialz)
colnames(commercialz) <-  c("Type", "PDI Number", "NPDI Total Cost", "PDI Total Cost")

lawntractorinfo <- info(platform = "Lawn Tractor", pos = "131", money = 25)[c(3,5,7)]
lawntractor <- infoii(pos = "137", money = 25, name = "LT5")
lawntractor <- rbind(lawntractor, infoii(pos = "13A", money = 25, name = "NX15"))
lawntractor <- rbind(lawntractor, infoii(pos = "139", money = 25, name = "NX9"))
lawntractor <- rbind(lawntractor, infoii(pos = "131", money = 25, name = "RT99GT & Series 1500"))
lawntractor <- rbind(lawntractor, infoii(pos = "132", money = 25, name = "CR12 & Series 2000"))
lawntractor <- as.data.frame(lawntractor)
colnames(lawntractor) <-  c("Type", "PDI Number", "NPDI Total Cost", "PDI Total Cost")

