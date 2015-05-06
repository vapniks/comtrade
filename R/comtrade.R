.packageName <- "comtrade"


##' Show country codes for comtrade.data reporter and partner params
##'
##' This function is copied directly from http://comtrade.un.org/data/Doc/api/ex/r and works as of 06/05/2015
##' @title comtrade.codes
##' @return A data.frame containing country names and associated reporter numbers.
##' @author Ben Veal
##' @export 
comtrade.codes <- function() {
    require(rjson)
    reporters <- fromJSON(file="http://comtrade.un.org/data/cache/partnerAreas.json")
    reporters <- as.data.frame(t(sapply(reporters$results,rbind)))
}


##' Download data from http://comtrade.un.org/data/
##' 
##' This function is copied from http://comtrade.un.org/data/Doc/api/ex/r with a few minor alterations,
##' and works as of 06/05/2015
##' To see a more detailed explanation of the parameters see: http://comtrade.un.org/data/doc/api/
##' @title comtrade.data 
##' @param r reporting area - either "all" or a numeric vector of reporting area codes, see \code{\link{comtrade.codes}}
##' @param p partner area - either "all" or a numeric vector of partner area codes, see \code{\link{comtrade.codes}}
##' @param ps time period - "now" (default), "recent", or "YYYY" or "YYYYMM" where YYYY=year and MM=month
##' @param maxrec maximum number of records to download (default = 50000 the maximum possible number)
##' @param type type of trade - "C"=commodities (default), "S"=services (not yet available)
##' @param freq frequency - "A"=annual (default), "M"=monthly
##' @param px classification - "HS"=harmonized system (default), "H0"=HS 1992, "H1"=HS 1996, "H2"=HS 2002, "H3"=HS 2007,
##' "H4"=HS 2012, "ST"=Standard International Trade Classification (SITC), "S1"=SITC revision 1, "S2"=SITC revision 2,
##' "S3"=SITC revision 3, "S4"=SITC revision 4, "BEC"=Broad Economic Categories.
##' @param rg trade regime/flow - "all" (default), "1" (imports), "2" (exports), "3" (re-Export), "4" (re-Import)
##' @param cc classification code - "AG1", "AG2" (default), "AG3", "AG4", "AG5", "AG6", "TOTAL" or "ALL"
##' @param validation whether or not to return validation information (showing download status, time, etc.)
##' @param url url for http api (you probably dont need to touch this)
##' @return A list containing a validation attribute (NULL for csv output), and a data attribute containing the data
##' @author Ben Veal
##' @export 
comtrade.data <- function(r,p,ps="now",maxrec=50000,type="C",freq="A",px="HS",
                          rg="all",cc="TOTAL",validation=FALSE,
                          url="http://comtrade.un.org/api/get?") {
    require(rjson)
    string<- paste0(url
                   ,"max=",maxrec,"&" #maximum no. of records returned
                   ,"type=",type,"&" #type of trade (c=commodities)
                   ,"freq=",freq,"&" #frequency
                   ,"px=",px,"&" #classification
                   ,"ps=",ps,"&" #time period
                   ,"r=",paste(r,collapse=","),"&" #reporting area
                   ,"p=",paste(p,collapse=","),"&" #partner country
                   ,"rg=",rg,"&" #trade flow
                   ,"cc=",cc,"&" #classification code
                   ,"fmt=",ifelse(validation,"json","csv")) #Format
    if(!validation) {
        raw.data<- read.csv(string,header=TRUE)
        return(list(validation=NULL, data=raw.data))
    } else {
        raw.data <- fromJSON(file=string)
        data <- raw.data$dataset
        validation <- unlist(raw.data$validation, recursive=TRUE)
        ndata <- NULL
        if(length(data) > 0) {
            var.names <- names(data[[1]])
            data <- as.data.frame(t(sapply(data,rbind)))
            ndata <- NULL
            for(i in 1:ncol(data)) {
                data[sapply(data[,i],is.null),i] <- NA
                ndata <- cbind(ndata, unlist(data[,i]))
            }
            ndata <- as.data.frame(ndata)
            colnames(ndata) <- var.names
        }
        return(list(validation=validation,data =ndata))
    }
}

