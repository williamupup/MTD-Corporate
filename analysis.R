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
