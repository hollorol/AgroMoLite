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

soilWaterC <- function(baseData,years, yearDays, style=soilWaterPlotStyle){
    soilWater<-grep("vwc",colnames(baseData),value = TRUE)    
    baseData %>%
    select(year,yearDay,soilWater) %>%
    filter(year==years,yearDay==yearDays) %>%
    gather(.,key = vwcStamp, value = vwc,soilWater)%>%
    mutate(depth=-10*(1:6)+5) %>%
    ggplot(aes(depth,vwc))+
  #  scale_x_continuous(position = "right")+
    scale_y_continuous(position = "top",
                       sec.axis = dup_axis(),
                       breaks = 
                       )+
    geom_vline(xintercept = -c(10,20,30,40,50), col="bisque3")+
    scale_x_continuous(labels=c("0-2 cm","2-5 cm","5-10 cm", "10-20 cm","20-50 cm","50-100 cm"),breaks = -c(5,15,25,35,45,55))+
    coord_flip()+
      geom_line(size=1.5, color="brown") +
      geom_point(size=3, shape=19, colour="black", alpha=1)+
    ylab(TeX("Soil water content \\[m^3/m^3\\]"))+ style()

}
