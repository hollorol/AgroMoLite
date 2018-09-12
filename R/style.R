
dplotStyl <- function(){
     theme(
            axis.line.x = element_line(size = 0.1),
            axis.line.y.left = element_line(size = 1),
            axis.line.y.right = element_line(color="blue", size= 1),
            axis.text.x = element_text(size=12),
            axis.title.x = element_blank(),
            axis.title.y.right = element_text(size=14,colour="blue", margin = margin(10,10,10,10)),
            axis.title.y.left = element_text(size=12, margin = margin(10,10,10,10)),
            axis.text.y.right  = element_text(size=12, colour = "blue"),
            axis.text.y.left  = element_text(size=12, colour = "black"),
            panel.background = element_blank(),
            plot.margin = margin(10,5,5,5),
            
        )
}


soilWaterPlotStyle <- function(){

       theme(
        axis.line.x.top = element_line(size=1),
        axis.line.x.bottom = element_blank(),
        axis.text.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        axis.title.x.bottom = element_blank(),
        axis.line.y.left = element_line(size = 1),
        axis.text.x.top = element_text(size=12),
        axis.text.y.left = element_text(size=12),
        axis.title.x.top = element_text(size = 14),
        axis.title.y.left = element_text(size = 14),
        panel.background = element_blank(),
        axis.ticks.y.left = element_blank())

}
