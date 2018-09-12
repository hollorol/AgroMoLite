upDir <- function(path,level=1){
    if(level==1){
        return(dirname(path))
    } else {
        return(upDir(dirname(path),(level-1)))
    }
}

agroMoLite <- function(baseDir=NULL){


    
    if(is.null(baseDir)){
        exampDir <- dirname(system.file("extdata","mosoly.txt", package = "AgroMoLite"))
        dir.create("AgroMo-CLI_test")
        exampFiles <- file.path(exampDir,
                           list.files(exampDir))
        for(i in exampFiles)
            file.copy(exampFiles,"./AgroMo-CLI_test/",recursive = TRUE)
        
        setwd("./AgroMo-CLI_test")
        file.copy(from=paste0("INI/",list.files("INI")),to="runDir",recursive = TRUE)
        file.copy(from=paste0("EXE/",list.files("EXE")),to="runDir",recursive = TRUE)
        file.copy(from=paste0("EPC/",list.files("EPC")),to="runDir",recursive = TRUE)
    }


}

## shinyApp(
##     shinyUI(
##         navbarPage("AgroMo-CLI",
##                    tabPanel("Input settings", uiOutput('page1')),
##                    tabPanel("Output control"),
##                    tabPanel("Ouput")
##                    )
##     ),
##     shinyServer(function(input, output, session) {
        
##         output$page1 <- renderUI({
##             sidebarLayout(
##                 sidebarPanel(
##                     selectizeInput(
##                         'id', label = "spinup ini", choices =   NULL, multiple=FALSE,selected="X2015",
##                         options = list(create = TRUE,placeholder = 'Choose the year')
##                     ),
##                     ## Make a list of checkboxes
##                     radioButtons("radio", label = h3("Radio buttons"),
##                                  choices = list("Choice 1" = 1, "Choice 2" = 2))
##                 ),
##                 mainPanel(
##                     plotOutput('distPlot')
##                 )
##             )
##         })

##         output$distPlot <- renderPlot({ plot(1) })
##     })
## )

