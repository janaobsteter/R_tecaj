meritve <- data.frame(ID = 1:1000, Vrednost1 = rnorm(1000), Vrednost2 = rnorm(1000),
                     Skupina = rep(c("A", "B", "C", "D"), 250))
head(meritve)


xyplot(plotDF$Vrednost1, plotDF$Vrednost2)
library(lattice)

plot(plotDF$ID, plotDF$Vrednost2, type="p")


head(mtcars)
library(ggplot2)

ggplot(data = mtcars, aes(x = mpg, y = cyl)) + geom_point()
