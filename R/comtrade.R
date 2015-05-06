.packageName <- "comtrade"


##' Obtain country codes for comtrade.data reporter and partner params
##'
##' This function is copied from http://comtrade.un.org/data/Doc/api/ex/r with a few alteration,
##' and works as of 06/05/2015
##' @title comtrade.codes
##' @param names An optional list of country names to return the codes for. If this is left blank then all available
##' codes will be returned. You can include "World" in the this list.
##' @return A data.frame containing country names and associated reporter codes.
##' @author Ben Veal
##' @export 
comtrade.codes <- function(names=NULL) {
    require(rjson)
    require(countrycode)
    reporters <- fromJSON(file="http://comtrade.un.org/data/cache/partnerAreas.json")
    reporters <- as.data.frame(t(sapply(reporters$results,rbind)))
    rows <- !(reporters$V1 %in% c("all","All","ALL"))
    data <- data.frame(code=as.numeric(unlist(reporters$V1[rows])),name=unlist(reporters$V2[rows]))
    if(!is.null(names)) {
        stopifnot(is.character(names))
        world <- which(names %in% c("World","world","WORLD"))
        notworld <- setdiff(1:length(names),world)
        codes <- numeric(length(names))
        codes[world] <- 0
        codes[notworld] <- countrycode(names[notworld],"country.name","un")
        missing <- is.na(codes)
        if(any(missing))
            warning("No code found for: ",paste(names[which(missing)],collapse=","))
        return(data[data$code %in% codes,])
    } else {
        return(data)
    }
}


##' Download data from http://comtrade.un.org/data/
##'
##' For a more detailed explanation of the parameters see: http://comtrade.un.org/data/doc/api/
##' 
##' @title comtrade.data 
##' @param r reporting area - either "all" or a numeric vector of reporting area codes, see \code{\link{comtrade.codes}}
##' @param p partner area - either "all" or a numeric vector of partner area codes, see \code{\link{comtrade.codes}}
##' @param dates time periods - "now" (default), "recent" (5 most recent years/months), or a list of dates, strings or integers
##' indicating which years/months to include. The strings/integers should be in the form "YYYY" or "YYYYMM" where YYYY=year
##' and MM=month
##' @param freq frequency - "A"=annual (default), "M"=monthly
##' @param type type of trade - "C"=commodities (default), "S"=services (not yet available)
##' @param px classification - "HS"=harmonized system (default), "H0"=HS 1992, "H1"=HS 1996, "H2"=HS 2002, "H3"=HS 2007,
##' "H4"=HS 2012, "ST"=Standard International Trade Classification (SITC), "S1"=SITC revision 1, "S2"=SITC revision 2,
##' "S3"=SITC revision 3, "S4"=SITC revision 4, "BEC"=Broad Economic Categories.
##' @param rg trade regime/flow - "all" (default), "1" (imports), "2" (exports), "3" (re-Export), "4" (re-Import)
##' @param cc classification code - "AG1", "AG2" (default), "AG3", "AG4", "AG5", "AG6", "TOTAL" or "ALL"
##' @param maxrec maximum number of records to download (default = 50000 the maximum possible number)
##' @param validation whether or not to return validation information (showing download status, time, etc.). Default is FALSE.
##' @return A list containing a validation attribute (NULL for csv output), and a data attribute containing the data
##' @author Ben Veal
##' @export 
comtrade.data <- function(r,p,dates="now",freq="A",type="C",px="HS",
                          rg="all",cc="TOTAL",maxrec=50000,validation=FALSE) {
    require(rjson)
    if(class(dates) %in% c("integer","character")) {
        ps <- dates
    } else if("POSIXct" %in% class(dates)) {
        if(freq=="A")
            ps <- format(dates,"%Y")
        else
            ps <- format(dates,"%Y%m")
    } else stop("Invalid data type for 'dates' param")
    chunk <- function(x,n) split(x,rep(1:ceiling(length(x)/n),each=n)[1:length(x)])
    ## We can only submit 5 values at a time for the r, p and dates parameter,
    ## so work out different parameter combinations required
    params <- expand.grid(chunk(r,5),chunk(p,5),chunk(dates,5))
    results <- validations <- list()
    ## loop over all possible parameter combinations
    for(row in 1:nrow(params)) {
        thisresult <- .comtrade.download(r=paste(params[row,1][[1]],collapse=","),
                                         p=paste(params[row,2][[1]],collapse=","),
                                         ps=paste(params[row,3][[1]],collapse=","),
                                         freq=freq,type=type,px=px,rg=rg,cc=cc,maxrec=maxrec,
                                         fmt=ifelse(validation,"json","csv"),
                                         url="http://comtrade.un.org/api/get?")
        results <- c(results,list(thisresult$data))
        validations <- c(validations,list(thisresult$validation))
        ## pause for a bit before the next download
        if(nrow(params) > 1) Sys.sleep(1.1)
    }
    return(list(validation=validations,data=do.call(rbind,results)))
}


##' This function is copied from http://comtrade.un.org/data/Doc/api/ex/r with a few minor alterations,
##' and works as of 06/05/2015
##' For a more detailed explanation of the parameters see: http://comtrade.un.org/data/doc/api/
##' Note: you can use a maximum of 5 country codes at a time in for the 'r' and 'p' parameters, and a maximum of
##' 5 different dates at a time.
.comtrade.download <- function(r,p,ps="now",freq="A",type="C",px="HS",
                               rg="all",cc="TOTAL",maxrec=50000,fmt="json",
                               url="http://comtrade.un.org/api/get?") {
    string<- paste0(url
                   ,"max=",maxrec,"&" #maximum no. of records returned
                   ,"type=",type,"&" #type of trade (c=commodities)
                   ,"freq=",freq,"&" #frequency
                   ,"px=",px,"&" #classification
                   ,"ps=",ps,"&" #time period
                   ,"r=",r,"&" #reporting area
                   ,"p=",p,"&" #partner country
                   ,"rg=",rg,"&" #trade flow
                   ,"cc=",cc,"&" #classification code
                   ,"fmt=",fmt) #Format
    if(fmt=="csv") {
        raw.data<- read.csv(string,header=TRUE)
        return(raw.data)
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
        return(list(validation=validation,data=ndata))
    }
}
