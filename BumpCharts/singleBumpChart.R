require(ggplot2)
require(ggrepel)
require(scales)
require(reshape)
require(jsonlite)
require(RColorBrewer)
require(grid)

timesData <- read.csv("timesData.csv", sep=",") # Load in the data

# Convert from factors to numeric
timesData$world_rank <- as.numeric(as.character(timesData$world_rank))

# Take only the top 20
timesData_top <- subset(timesData, world_rank <= 20)[c("year","university_name", "world_rank")] 

# Abbreviate the long names
# timesData_top$university_name <- abbreviate(timesData_top$university_name,25)

timesfinalYear <- timesData_top[which(timesData_top$year == 2016),]
timesfirstAppearance <- timesData_top[!duplicated(timesData_top$university_name) & timesData_top$year != 2016,]

pal1 = c("#c57c3c", "#e392c2", "#a5e7a8", "#bea3ea", "#d7e298", "#81a4e3", "#a6b16a", "#a7baf2", "#e4c587", "#5ab6e6",
         "#d6a16d", "#62d9f3", "#eb9189", "#3ec1c8", "#e1a6b6", "#7fe3c5", "#e5b4e2", "#8bba83", "#cd5136", "#84bb9c",
         "#e1ceeb", "#72b7b0", "#cd9e8c", "#93e7e2", "#ecc0b1", "#7bb1c6", "#d8e8c5", "#acbadd", "#b2b593", "#acd8eb")

g1 <- ggplot(data=timesData_top, aes(x=year, y=world_rank)) + 
  geom_line(aes(colour=university_name), size=1.5) + 
  geom_point(shape = 21, stroke = 2, size=5, fill = "white", aes(colour=university_name)) + 
  geom_label_repel(data = timesfirstAppearance, aes(label=university_name), size=2.5, fontface = "bold", color='#2f2f2f') +
  geom_label(data = timesfinalYear, aes(x=year, y = world_rank, label=university_name), size=2.5, fontface = "bold", color='#2f2f2f', hjust=0) +
  scale_y_reverse(lim=c(20,1), breaks = scales::pretty_breaks(n = 20)) +
  scale_x_continuous(expand = c(.1, .1), breaks = scales::pretty_breaks(n = 5)) +
  ggtitle('Times World Universities Ranking') +
  xlab(NULL) +
  ylab("World Rank") +
  theme_minimal() +
  theme_bw() +
  scale_colour_manual(values=pal1) + 
  theme(panel.background = element_rect(fill = '#ffffff'),
    plot.title = element_text(size=14), legend.title=element_blank(),
    axis.text = element_text(size=11), axis.title=element_text(size=11), 
    panel.border = element_blank(), legend.position='none', 
    panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
    axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
g1