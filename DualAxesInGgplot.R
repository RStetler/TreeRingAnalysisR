setwd("N://R_Files/Thesis")
library(ggplot2)
library(tidyr)
library(gtable)
library(grid)

#Plot:
p1 <- ggplot()+
  geom_line(data=MovingCount,aes(x=Year,y=PercRel*100))+#ylab("Percent of Sites Released (3-year moving window)")+
  scale_x_continuous(breaks=seq(from = 1625, to = 2005, by = 25)) +
  ggtitle("Percent of Sites Released (3-year moving window)") +
  labs(x=NULL,y=NULL) +
  theme(plot.title = element_text(hjust = -0.135, vjust=2.12, colour="black", size = 14), panel.background = element_blank(), panel.grid.major.y=element_blank(), 
        panel.grid.minor=element_blank(),panel.grid.major.x=element_line(color = "gray88", size = 0.5),axis.text.y=element_text(size=12), axis.text.x=element_text(size=12))


p2 <- ggplot()+
  geom_line(data=MovingCount, aes(x=Year,y=NumSitesRec), color="dodgerblue4")+
  ggtitle("Sample Depth (no. of sites)")+
  labs(x=NULL,y=NULL) +
  scale_x_continuous(breaks=seq(from = 1625, to = 2005, by = 25)) +
  theme(plot.title = element_text(hjust = -0.135, vjust=2.12, colour="black", size = 14), panel.background = element_blank(), panel.grid.major.y=element_blank(), 
        panel.grid.minor=element_blank(),panel.grid.major.x=element_line(color = "gray88", size = 0.5),axis.text.y=element_text(size=12), axis.text.x=element_text(size=12))

g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)

# Get the locations of the plot panels in g1.
pp <- c(subset(g1$layout, name == "panel", se = t:r))

# Overlap panel for second plot on that of the first plot
g1 <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)

# ggplot contains many labels that are themselves complex grob; 
# usually a text grob surrounded by margins.
# When moving the grobs from, say, the left to the right of a plot,
# make sure the margins and the justifications are swapped around.
# The function below does the swapping.
# Taken from the cowplot package:
# https://github.com/wilkelab/cowplot/blob/master/R/switch_axis.R 
hinvert_title_grob <- function(grob){
  
  # Swap the widths
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  # Fix the justification
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}

# Get the y axis from g2 (axis line, tick marks, and tick mark labels)
index <- which(g2$layout$name == "axis-l")  # Which grob
yaxis <- g2$grobs[[index]]                  # Extract the grob

# yaxis is a complex of grobs containing the axis line, the tick marks, and the tick mark labels.
# The relevant grobs are contained in axis$children:
#   axis$children[[1]] contains the axis line;
#   axis$children[[2]] contains the tick marks and tick mark labels.

# Second, swap tick marks and tick mark labels
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths)
ticks$grobs <- rev(ticks$grobs)

# Third, move the tick marks
# Tick mark lengths can change. 
# A function to get the original tick mark length
# Taken from the cowplot package:
# https://github.com/wilkelab/cowplot/blob/master/R/switch_axis.R 
plot_theme <- function(p) {
  plyr::defaults(p$theme, theme_get())
}

tml <- plot_theme(p1)$axis.ticks.length   # Tick mark length
ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml

# Fourth, swap margins and fix justifications for the tick mark labels
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])

# Fifth, put ticks back into yaxis
yaxis$children[[2]] <- ticks

# Put the transformed yaxis on the right side of g1
g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
g1 <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")

# Labels grob
left = textGrob("Percent of Sites Released (3-year moving window)", x = 0, y = 0.9, just = c("left", "top"), gp = gpar(fontsize = 14, col =  "black"))
right =  textGrob("Sample Depth (no. of sites)", x = 1, y = 0.9, just = c("right", "top"), gp = gpar(fontsize = 14, col =  "dodgerblue4"))
labs = gTree("Labs", children = gList(left, right))

# New row in the gtable for labels
height = unit(3, "grobheight", left)
g1 <- gtable_add_rows(g1, height, 2)  

# Put the label in the new row
g1 = gtable_add_grob(g1, labs, t=3, l=3, r=5)

# Turn off clipping in the plot panel
g1$layout[which(g1$layout$name == "panel"), ]$clip = "off"

# Print to .png
ggsave("plot2.png", g1, width=11, height=5)
