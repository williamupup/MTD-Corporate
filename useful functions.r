rf <- function(platform = NULL, dealer = "", year = NULL , state = "", PDI = F){
    #platform !!!!!!!!!!!!!!!!!! when it's null asd  = null
    if (is.null(platform) == F) {
        pos1 <- substr(platform, 1,2)
        pos3 <- substr(platform,3,3)
        asd <- rawdata[rawdata$POS1.2 == substr(platform,1,2) & rawdata$POS5 == substr(platform,3,3),]
    }
    
    #dealer!!!!!!!!!!!!!!!!!!!! when dealer is null, asd will lose 240 rows, some dealer name has a <> in it so can't identify
    #stringarray <- strsplit(dealer,"")
    #condicion <- 
    asd <- asd[grep(dealer, asd$Repair.Dealer.Name, ignore.case = T),]
    
    #year!!!!!!!!!!!!!!!!!still no default value
    if (is.null(year) == F){
        asd <- asd[asd$Failure.Calendar.Year == year,]
    }
    
    #state
    asd <- asd[grep(state, asd$Repair.Dealer.State, ignore.case = T),]
    
    #PDI
    if (PDI == T) {
        asd <- asd[grep("[Pp][Dd][Ii]", asd$Failure.Description),]
    }
    return(asd)
    
}
