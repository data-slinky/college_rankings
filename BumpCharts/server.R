library("shiny")
library("ggplot2")
library("ggrepel")
library("scales")
library("RColorBrewer")

pal1 = c("#c57c3c", "#e392c2", "#a5e7a8", "#bea3ea", "#d7e298", "#81a4e3", "#a6b16a", "#a7baf2", "#e4c587", "#5ab6e6",
         "#d6a16d", "#62d9f3", "#eb9189", "#3ec1c8", "#e1a6b6", "#7fe3c5", "#e5b4e2", "#8bba83", "#cd5136", "#84bb9c",
         "#e1ceeb", "#72b7b0", "#cd9e8c", "#93e7e2", "#ecc0b1", "#7bb1c6", "#d8e8c5", "#acbadd", "#b2b593", "#acd8eb")

shinyServer(
  function(input, output) {

    output$plot1 <- renderPlot( {
      
      Data <- read.csv(paste(input$data, ".csv", sep="")) # Load in the data reactively
      Data$world_rank <- as.numeric(as.character(Data$world_rank))
      
      Data_top <- subset(Data, world_rank <= input$var1)[c("year","university_name", "world_rank")] 
      finalYear <- Data_top[which(Data_top$year == max(Data_top$year)),]
      firstAppearance <- Data_top[!duplicated(Data_top$university_name) & Data_top$year != max(Data_top$year),]
      
      g1 <- ggplot(data=Data_top, aes(x=year, y=world_rank)) + 
        geom_line(aes(colour=university_name), size=1.5) + 
        geom_point(shape = 21, stroke = 2, size=5, fill = "white", aes(colour=university_name)) + 
        geom_label_repel(data = firstAppearance, aes(label=university_name), size=3, fontface = "bold", color='#2f2f2f') +
        geom_label(data = finalYear, aes(x=year, y = world_rank, label=university_name), size=3, fontface = "bold", color='#2f2f2f', hjust=0) +
        scale_y_reverse(lim=c(input$var1, 1), breaks = scales::pretty_breaks(n = input$var1)) +
        scale_x_continuous(expand = c(.12, .12), breaks = scales::pretty_breaks(n = 5)) +
        ggtitle("College Rankings") +
        xlab(NULL) +
        ylab("World Rank") +
        theme_minimal() +
        theme_bw() +
        scale_colour_manual(values=pal1) + 
        theme(plot.background = element_rect(fill = "#222222"), 
              panel.background = element_rect(fill = '#222222'),
              plot.title = element_text(color = "white", size=20, family="sans", face="bold"), 
              legend.title=element_blank(),
              axis.text = element_text(color = "white", size=14, family="sans", face="bold"), 
              axis.title=element_text(color = "white", size=16, family="sans", face="bold"), 
              panel.border = element_blank(), legend.position='none', 
              panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
              axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
      
      print(g1)
    })
  })
