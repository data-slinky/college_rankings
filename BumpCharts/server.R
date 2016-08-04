library("shiny")
library("ggplot2")
library("ggrepel")
library("scales")
library("RColorBrewer")

pal1 = c("#8dc0b0", "#d5a6db", "#8ecc9c", "#b9b5f3", "#caedb3", "#74aff3", "#ede1a3",
         "#5ecae7", "#f4b189", "#71e2eb", "#eba396", "#67ccc2", "#eda4c1", "#94e9ce",
         "#e5c5f7", "#b1bf81", "#a9c6f0", "#d3be88", "#90cbf0", "#e9bf98", "#94d9df",
         "#efb4aa", "#bdf0dd", "#e8bad0", "#a5d1b3", "#b1b2da", "#b2c69a", "#cfa191",
         "#e3e5bc", "#cebe9b")

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
