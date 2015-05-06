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
##' This function is copied directly from http://comtrade.un.org/data/Doc/api/ex/r and works as of 06/05/2015
##' To see a more detailed explanation of the parameters see: http://comtrade.un.org/data/doc/api/
##' @title comtrade.data 
##' @param url url for http api (you probably don't need to touch this)
##' @param maxrec maximum number of records to download (default = 50000 the maximum possible number)
##' @param type type of trade - "C" (default), "S" (not yet available) trade in services data
##' @param freq frequency - "A"=annual (default), "M"=monthly
##' @param px classification - "HS"=harmonized system (default), "H0"=HS 1992, "H1"=HS 1996, "H2"=HS 2002, "H3"=HS 2007,
##' "H4"=HS 2012, "ST"=Standard International Trade Classification (SITC), "S1"=SITC revision 1, "S2"=SITC revision 2,
##' "S3"=SITC revision 3, "S4"=SITC revision 4, "BEC"=Broad Economic Categories.
##' @param ps time period - "now" (default), "recent", or "YYYY" or "YYYYMM" where YYYY=year and MM=month
##' @param r reporting area - "all", see \code{\link{comtrade.codes}}
##' @param p partner area - "all", see \code{\link{comtrade.codes}}
##' @param rg trade regime/flow - "all" (default), "1" (imports), "2" (exports), "3" (re-Export), "4" (re-Import)
##' @param cc classification code - "AG1", "AG2" (default), "AG3", "AG4", "AG5", "AG6", "TOTAL" or "ALL"
##' @param fmt output format - "json" (default), or "csv". json output includes validation information.
##' @return A list containing a validation attribute (NULL for csv output), and a data attribute containing the data
##' @author Ben Veal
##' @export 
comtrade.data <- function(url="http://comtrade.un.org/api/get?",
                         maxrec=50000,type="C",freq="A",px="HS",ps="now",
                         r,p,rg="all",cc="TOTAL",fmt="json") {
    
    string<- paste(url
                  ,"max=",maxrec,"&" #maximum no. of records returned
                  ,"type=",type,"&" #type of trade (c=commodities)
                  ,"freq=",freq,"&" #frequency
                  ,"px=",px,"&" #classification
                  ,"ps=",ps,"&" #time period
                  ,"r=",r,"&" #reporting area
                  ,"p=",p,"&" #partner country
                  ,"rg=",rg,"&" #trade flow
                  ,"cc=",cc,"&" #classification code
                  ,"fmt=",fmt #Format
                  ,sep = "")
    if(fmt == "csv") {
        raw.data<- read.csv(string,header=TRUE)
        return(list(validation=NULL, data=raw.data))
    } else {
        if(fmt == "json" ) {
            raw.data<- fromJSON(file=string)
            data<- raw.data$dataset
            validation<- unlist(raw.data$validation, recursive=TRUE)
            ndata<- NULL
            if(length(data)> 0) {
                var.names<- names(data[[1]])
                data<- as.data.frame(t(sapply(data,rbind)))
                ndata<- NULL
                for(i in 1:ncol(data)){
                    data[sapply(data[,i],is.null),i]<- NA
                    ndata<- cbind(ndata, unlist(data[,i]))
                }
                ndata<- as.data.frame(ndata)
                colnames(ndata)<- var.names
            }
            return(list(validation=validation,data =ndata))
        }
    }
}

