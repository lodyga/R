#Regression Models for Data Science in R


library(reshape)
if (!require("UsingR")) install.packages("UsingR")
install.packages("reshape2")


data(galton)
str(galton)



long <- melt(galton)
g <- ggplot(long, aes(x = value, fill = variable))
g <- g + geom_histogram(colour = "black", binwidth=1)
g <- g + facet_grid(. ~ variable)
g