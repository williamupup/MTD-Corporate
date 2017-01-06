#1. Rawdata Function, under the condition that rawdata has already benn loaded
rf <- function(platform = NULL, dealer = "", year = NULL , state = "", PDI = ""){
    asd <- rawdata
    #platform !!!!!!!!!!!!!!!!!! when it's null asd  = null
    if (is.null(platform) == F) {
        pos12 <- substr(platform, 1,2)
        pos3 <- substr(platform,3,3)
        asd <- asd[asd$POS1.2 == pos12,]
        if (pos3 != "") {
            asd <- asd[asd$POS5 == pos3,]
        }
    }
    
    #dealer!!!!!!!!!!!!!!!!!!!! when dealer is null, asd will lose 233 rows, some dealer name has a <> in it so can't identify
    #stringarray <- strsplit(dealer,"")
    #condicion <- 
    asd <- asd[grep(dealer, asd$Repair.Dealer.Name, ignore.case = T),]
    
    #year
    if (is.null(year) == F){
        asd <- asd[asd$Production.Calendar.Year %in% year,]
    }
    
    #state
    asd <- asd[grep(state, asd$Repair.Dealer.State, ignore.case = T),]
    
    #PDI
    if (PDI == T) {
        asd <- asd[grep("[Pp][Dd][Ii]", asd$Failure.Description),]
    } else
    if (PDI == F) {
        asd <- asd[-grep("[Pp][Dd][Ii]", asd$Failure.Description),]
    } else
        asd <- asd
    return(asd)
    
}

#2. Sales Function, under the condition that sales data has alreadt been loaded
sf <- function(dealer = "", year = NULL, platform = ""){
    asd <- sales
    
    #dealer
    asd <- asd[grep(dealer, asd$Customer.Name, ignore.case = T),]
    
    #year
    if (is.null(year) == F){
        asd <- asd[asd$Shipment.Fiscal.Year == year,]
    }
    
    #platform
    asd <- asd[grep(platform, asd$Item.Platform, ignore.case = T),]
    
    return(asd)
}
