#' doublePlot
#' 
#' @author Roland Hollos
#' @import ggplot2
#' @keywords internal
doublePlot <- function(baseData,colorVec,variables,yearLim=c(-Inf,Inf), plotStyle=dplotStyl){

    if(length(year)==1){
        year <- rep(year,2)
    }
   
    if(length(variables)!=2){
        stop("doublePlot can plot exactly two dataset")
    }
    
    scales <- function(x){
        scales=list()
        range1 <- (max(x[,1])-min(x[,1]))
        range2 <- (max(x[,2])-min(x[,2]))
        if(range1>=range2){
            scales[["plotVars"]] <- colnames(x)[c(1,2)]
            scales[["scaleRatio"]] <- max(x[,1])/max(x[,2])
            scales[["range"]] <- c(min(min(x[,1]),min(x[,2])),max(x[,1]))
                } else {
                    scales[["plotVars"]] <- colnames(x)[c(2,1)]
                    scales[["scaleRatio"]] <- max(x[,2])/max(x[,1])
                    scales[["range"]] <- c(min(min(x[,1]),min(x[,2])),max(x[,2]))
                }
        return(scales)         
    }
    
    scaleData <- baseData %>%
        select(UQ(quo(variables))) %>%
        scales
    
    baseData %>%
        select(date, year, month, day, yearDay, UQ(quo(variables))) %>%
        #filter(year >= UQ(quo(year[1])), year <=UQ(quo(year[2]))) %>%
        filter(year>=yearLim[1],year<=yearLim[2])%>%
    ggplot(aes_string("date",scaleData$plotVars[1],gropup="year"))+
    scale_y_continuous(limits = scaleData$range ,sec.axis = sec_axis(~ . * scaleData$scaleRatio,name=scaleData$plotVars[2]))+
        geom_line()+geom_line(aes(date,
                                 eval(parse(text=paste0(scaleData$plotVars[2],"*","scaleData$scaleRatio")))),color="blue")+
        plotStyle()

    
}
#'soilWaterC
#'
#' @keywords internal
#' @import latex2exp magrittr
soilWaterC <- function(baseData,years, yearDays, style=soilWaterPlotStyle, yLimits=NULL, language="hu"){
    soilWater<-grep("vwc",colnames(baseData),value = TRUE)
    
    sWBase <- baseData %>%
        select(year,yearDay,soilWater)
    sRange <- sWBase %>% select(soilWater) %>% (function (x) {c(min(x),max(x))})

    if(is.null(yLimits)){
        yLimits <- c(NA,NA)
    } else {
        if(is.character(yLimits)){
            yLimits <- sRange
        }
    }
    if(language=="hu"){
        
    }
    
    sWBase %>% filter(year==years,yearDay==yearDays) %>%
    tidyr::gather(.,key = vwcStamp, value = vwc,soilWater)%>%
    mutate(depth=-c(0.1,0.35,0.75,1.5,3.5,7.5)) %>%
        ggplot(aes(depth,vwc))+
        labs(x="mélység [cm]")+
  #  scale_x_continuous(position = "right")+
    scale_y_continuous(position = "top",
                       sec.axis = dup_axis(),
                       limits=yLimits
                       )+
    geom_vline(xintercept = -c(0.2,0.5,1,2,5), col="bisque3")+
    scale_x_continuous(labels=c("2","5","10", "20","50","100"),breaks = -c(0.2,0.5,1,2,5,10),expand = c(0,0.095))+
    coord_flip()+
      geom_line(size=1, color="brown") +
     # geom_point(size=2, shape=19, colour="black", alpha=1)+
    ylab(TeX("Talajrégetek víztartalma \\[m^3/m^3\\]"))+ style()

}
