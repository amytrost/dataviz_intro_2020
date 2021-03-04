# installing packages, setting appropriate direcotry
setwd("C:/Users/cassa/Desktop/Rgraphics")

install.packages("tidyverse")
install.packages("ggrepel")
install.packages("ggplot2")

library(tidyverse) # for organizing data
library(ggplot2)   # for fancy graphics
library(ggrepel)  # adding some functionality to ggplot2

# small multiples with housing data
# faceting
housing <- read_csv("dataSets/landdata-states.csv")
p5 <- ggplot(housing, aes(x = Date, y = Home.Value))
p5 + geom_line(aes(color = State))  
(p5 <- p5 + geom_line() +
    facet_wrap(~State, ncol = 10))
p5 + theme_linedraw()
p5+ theme_light()



# Recreating Economist Graph on corruption and human development
# 1. read in data
dat <- read_csv("dataSets/EconomistData.csv")
# 2. create basic graph
pc1 <- ggplot(dat, aes(x = CPI, y = HDI, color = Region))
pc1 + geom_point()
# 3 add a trend line
pc2 <- pc1 +
  geom_smooth(mapping = aes(linetype = "r2"),
              method = "lm",
              formula = y ~ x + log(x), se = FALSE,
              color = "red")
pc2 + geom_point()

# learning about shapes from R tutorial
df2 <- data.frame(x = 1:5 , y = 1:25, z = 1:25)
s <- ggplot(df2, aes(x, y))
s + geom_point(aes(shape = z), size = 4) +
  scale_shape_identity()

# shape 1 is an open circle, use this
pc2 + geom_point(shape = 1, size = 4)

# set the size and thickness of line
(pc3 <- pc2 + geom_point(shape = 1, size = 2.5, stroke = 1.25))

# set points to label
pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", 
                   "Japan", "New Zealand", "Singapore")

(pc4 <- pc3 +
    geom_text(aes(label = Country),
              color = "gray20",
              data = filter(dat, Country %in% pointsToLabel)))

library("ggrepel")
(pc4 <- pc3 +
    geom_text_repel(aes(label = Country),
                    color = "gray20",
                    data = filter(dat, Country %in% pointsToLabel),
                    force = 10))

# formatting regions
dat$Region <- factor(dat$Region,
                     levels = c("EU W. Europe",
                                "Americas",
                                "Asia Pacific",
                                "East EU Cemt Asia",
                                "MENA",
                                "SSA"),
                     labels = c("OECD",
                                "Americas",
                                "Asia &\nOceania",
                                "Central &\nEastern Europe",
                                "Middle East &\nnorth Africa",
                                "Sub-Saharan\nAfrica"))

pc4$data <- dat
pc4

# adjusting thematic elements
library(grid)
(pc5 <- pc4 +
    scale_x_continuous(name = "Corruption Perceptions Index, 2011 (10=least corrupt)",
                       limits = c(.9, 10.5),
                       breaks = 1:10) +
    scale_y_continuous(name = "Human Development Index, 2011 (1=Best)",
                       limits = c(0.2, 1.0),
                       breaks = seq(0.2, 1.0, by = 0.1)) +
    scale_color_manual(name = "",
                       values = c("#24576D",
                                  "#099DD7",
                                  "#28AADC",
                                  "#248E84",
                                  "#F2583F",
                                  "#96503F")) +
    ggtitle("Corruption and Human development"))

(pc6 <- pc5 +
    theme_minimal() + # start with a minimal theme and add what we need
    theme(text = element_text(color = "gray20"),
          legend.position = c("top"), # position the legend in the upper left 
          legend.direction = "horizontal",
          legend.justification = 0.1, # anchor point for legend.position.
          legend.text = element_text(size = 11, color = "gray10"),
          axis.text = element_text(face = "italic"),
          axis.title.x = element_text(vjust = -1), # move title away from axis
          axis.title.y = element_text(vjust = 2), # move away for axis
          axis.ticks.y = element_blank(), # element_blank() is how we remove elements
          axis.line = element_line(color = "gray40", size = 0.5),
          axis.line.y = element_blank(),
          panel.grid.major = element_line(color = "gray50", size = 0.5),
          panel.grid.major.x = element_blank()
    ))

mR2 <- summary(lm(HDI ~ CPI + log(CPI), data = dat))$r.squared
mR2 <- paste0(format(mR2, digits = 2), "%")

# final graphic
# png(file = "images/econScatter10.png", width = 700, height = 500)
p <- ggplot(dat,
            mapping = aes(x = CPI, y = HDI)) +
  geom_smooth(mapping = aes(linetype = "r2"),
              method = "lm",
              formula = y ~ x + log(x), se = FALSE,
              color = "red") +
  geom_point(mapping = aes(color = Region),
             shape = 1,
             size = 4,
             stroke = 1.5) +
  geom_text_repel(mapping = aes(label = Country, alpha = labels),
                  color = "gray20",
                  data = transform(dat,
                                   labels = Country %in% c("Russia",
                                                           "Venezuela",
                                                           "Iraq",
                                                           "Mayanmar",
                                                           "Sudan",
                                                           "Afghanistan",
                                                           "Congo",
                                                           "Greece",
                                                           "Argentina",
                                                           "Italy",
                                                           "Brazil",
                                                           "India",
                                                           "China",
                                                           "South Africa",
                                                           "Spain",
                                                           "Cape Verde",
                                                           "Bhutan",
                                                           "Rwanda",
                                                           "France",
                                                           "Botswana",
                                                           "France",
                                                           "US",
                                                           "Germany",
                                                           "Britain",
                                                           "Barbados",
                                                           "Japan",
                                                           "Norway",
                                                           "New Zealand",
                                                           "Sigapore"))) +
  scale_x_continuous(name = "Corruption Perception Index, 2011 (10=least corrupt)",
                     limits = c(1.0, 10.0),
                     breaks = 1:10) +
  scale_y_continuous(name = "Human Development Index, 2011 (1=best)",
                     limits = c(0.2, 1.0),
                     breaks = seq(0.2, 1.0, by = 0.1)) +
  scale_color_manual(name = "",
                     values = c("#24576D",
                                "#099DD7",
                                "#28AADC",
                                "#248E84",
                                "#F2583F",
                                "#96503F"),
                     guide = guide_legend(nrow = 1, order=1)) +
  scale_alpha_discrete(range = c(0, 1),
                       guide = FALSE) +
  scale_linetype(name = "",
                 breaks = "r2",
                 labels = list(bquote(R^2==.(mR2))),
                 guide = guide_legend(override.aes = list(linetype = 1, size = 2, color = "red"), order=2)) +
  ggtitle("Corruption and human development") +
  labs(caption="Sources: Transparency International; UN Human Development Report") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray"),
        text = element_text(color = "gray20"),
        axis.title.x = element_text(face="italic"),
        axis.title.y = element_text(face="italic"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.caption = element_text(hjust=0),
        plot.title = element_text(size = 16, face = "bold"))

p
dev.off()
getwd()
