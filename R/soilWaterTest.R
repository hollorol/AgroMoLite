#' soilWaterTest
#'
#' @param iniFile The name of the inifile
#' @param epcFile The name of the epcfile
#' @export

soilWaterTest <- function(iniFile, epcFile=NULL, year, yearDay,savePlot=NULL){
    settings <- setupMuso()
    numberOfYears <- settings$numYears
    startYear <- settings$startYear
    YearDay <- yearDay
    Year <- year
    baseData <- calibMuso(settings,silent = TRUE) %>%
        as.data.frame() %>%
        tibble::rownames_to_column("date") %>%
        mutate(date2=date,date=as.Date(date,"%d.%m.%Y"),yearDay=rep(1:365,numberOfYears)) %>%
        separate(date2,c("day","month","year"),sep="\\.")
    if(!is.null(savePlot)){
        ggplot2::ggsave(savePlot,soilWaterC(baseData, years=Year,yearDays=YearDay))
    } else
        soilWaterC(baseData, years=Year,yearDays=YearDay)
}
