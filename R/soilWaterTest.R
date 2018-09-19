#' soilWaterTest
#'
#' @param iniFile The name of the inifile
#' @param epcFile The name of the epcfile
#' @import dplyr, shiny
#' @export

soilWaterTest <- function(year, yearDay, iniFile, epcFile=NULL,savePlot=NULL, skipSpinup=TRUE, interactive=TRUE){

    settings <- RBBGCMuso::setupMuso()
    numberOfYears <- settings$numYears
    startYear <- settings$startYear
    baseData <- RBBGCMuso::calibMuso(settings,silent = TRUE,skipSpinup=skipSpinup) %>%
        as.data.frame() %>%
        tibble::rownames_to_column("date") %>%
        mutate(date2=date,date=as.Date(date,"%d.%m.%Y"),yearDay=rep(1:365,numberOfYears)) %>%
        tidyr::separate(date2,c("day","month","year"),sep="\\.")
    soilWater<-grep("vwc",colnames(baseData),value = TRUE)
    sWBase <- baseData %>%
        select(year,yearDay,soilWater)

    if(interactive){
        metData <- cbind(as.Date(musoDate(startYear,numberOfYears,corrigated=FALSE),"%d.%m.%Y"),
                         as.data.frame(read.table(settings$metInput[2],skip=4)))
        colnames(metData) <- c("date",
                               "year",
                               "yday",
                               "Tmax",
                               "Tmin",
                               "Tday",
                               "prcp",
                               "VPD",
                               "srad",
                               "daylen")
        
        metLabels <- c("maximális hőmérséklet (°C)",
                                           "minimális hőmérséklet (°C)",
                                           "nappali átlaghőmérséklet (°C)",
                                           "csapadék (cm)",
                       "nappali vízgőztelítési hiány (Pa)")
        
        metColors <- c("","","","brown","purple","orange","blue","green","","")
        
        varTable <- cbind(c("","","",metLabels,"",""),colnames(metData),metColors)
    }
    
    ui <- fluidPage(
        titlePanel("AgroMo-Lite talajnedvesség-profil"),
        fluidRow(
            column(3,
                   wellPanel(selectInput("year","év",startYear:(startYear+numberOfYears-1)),
                             sliderInput("yearDay", "év napja",
                                         min = 2,
                                         max = 364,
                                         value = 2),
                             selectInput("metVar","Meteorológiai változó",
                                         c("maximális hőmérséklet (°C)",
                                           "minimális hőmérséklet (°C)",
                                           "nappali átlaghőmérséklet (°C)",
                                           "csapadék (cm)",
                                           "nappali vízgőztelítési hiány (Pa)"),
                                         selected = "csapadék (cm)")
                             )
                   ),
           
             column(5,
                    plotOutput("meteorology"))## ,
            ## column(2,
            ##        img(src="http://meteor22.elte.hu/images/infoblokk_sm.jpg", align = "left", height=100)
            ##        )
        ),
        fluidRow(
            column(4,
                  plotOutput("soilProfil1")),
            column(4,
                   plotOutput("soilProfil2")),
            column(4,
                   plotOutput("soilProfil3"))
        )
        
         
        
    )

    server <- function(input, output, session){

        yLimits <- reactive({
            sWBase %>%
                filter(year==input$year,(yearDay==(input$yearDay-1)|yearDay==input$yearDay|yearDay==(input$yearDay+1))) %>%
                select(soilWater) %>%
                (function (x) {c(min(x),max(x))})
        })

        session$onSessionEnded(function() {
            stopApp()
        })

        
 
        
        output$soilProfil1<-renderPlot(
            
        {

            Year <- input$year
            YearDay <- input$yearDay-1
            soilWaterC(baseData, years=Year,yearDays=YearDay,yLimits=yLimits())
        }
        )
        
        output$soilProfil2<-renderPlot(
            
        {
            Year <- input$year
            YearDay <- input$yearDay
            soilWaterC(baseData, years=Year,yearDays=YearDay,yLimits=yLimits())
        }
        )

        output$soilProfil3<-renderPlot(
            
        {
            Year <- input$year
            YearDay <- input$yearDay+1
            soilWaterC(baseData, years=Year,yearDays=YearDay,yLimits=yLimits())
        }
        )

        output$meteorology <- renderPlot(
        {
            varName <- varTable[which(varTable[,1]==input$metVar),2]
            varCol <-  varTable[which(varTable[,1]==input$metVar),3]
            metData %>%
                filter(year==input$year, (input$yearDay-6)<=yday, yday<=input$yearDay) %>%
                ggplot(aes_string("date",varName))+geom_bar(stat="identity", fill=varCol)+
                theme(axis.title.x = element_blank())
            
        }
        )

        
        
    }
    
    shinyApp(ui=ui,server = server)

    ## if(!is.null(savePlot)){
    ##     ggplot2::ggsave(savePlot,soilWaterC(baseData, years=Year,yearDays=YearDay))
    ## } else
    ##     soilWaterC(baseData, years=Year,yearDays=YearDay)
}
