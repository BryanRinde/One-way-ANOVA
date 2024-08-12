# loading libraries
library(readr)
library(ggplot2)
library(multcomp)
library(multcompView)
library(dplyr)
library(ggsignif)

DMSO <- c(2.10, 0.60, 0.44, 1.10, 2.95)
FADS1 <- c(1.69, 0.31, 2.82, 0.28, 0.06)
FADS2 <- c(0.76, 0.97, 0.52, 0.73, 0.18)
FADS12 <- c(0.86, 0.11, 1.20, 0.70, 0.13)
data <- data.frame(Group = factor(rep(c("DMSO", "FADS1", "FADS2", "FADS12"), each = 5)),
                   Value = c(DMSO, FADS1, FADS2, FADS12))

anova.rr <- aov(Value ~ as.factor(Group), data = data)
tukey.rr <- TukeyHSD(anova.rr)
tukey <- as.data.frame(tukey.rr$`as.factor(Group)`)

signifLevels <- cut(tukey$`p adj`,
                     breaks=c(-Inf,0.001,0.01,0.05,Inf), 
                     labels=c("***","**","*","ns"))


# Function to determine positions of significance asterisks
position_signif <- function(data, group1, group2) {
  max_y <- max(data$Mean[data$Group %in% c(group1, group2)])
  return(max_y + 2)
}


