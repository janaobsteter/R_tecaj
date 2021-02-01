meritve <- data.frame(ID = 1:1000, Visina = abs(rnorm(1000)*10), Vsebnost = abs(rnorm(1000)),
                     Lokacija = rep(c("A", "B", "C", "D"), 250),
                     Leto = rep(2001:2010, each = 100))
head(meritve)
nrow(meritve)
write.csv(meritve, "~/Documents/Delavnice/R_tecaj/ModulB/Meritve.csv", quote=F, row.names=F)


xyplot(plotDF$Vrednost1, plotDF$Vrednost2)
library(lattice)

plot(plotDF$ID, plotDF$Vrednost2, type="p")


head(mtcars)
library(ggplot2)
head(meritve)
ggplot(data = meritve, aes(x=Visina, y = Vsebnost)) + geom_point()
ggplot(data = meritve, aes(x=Visina, y = Vsebnost, colour = Lokacija)) + geom_point() + scale_colour_brewer()
ggplot(data = meritve, aes(x=Visina, y = Vsebnost, colour = Lokacija)) + geom_point() + scale_colour_brewer(palette = "PuRd")
ggplot(data = meritve, aes(x=Visina, y = Vsebnost, colour = Visina)) + geom_point() + scale_colour_distiller()
ggplot(data = meritve, aes(x=Visina, y = Vsebnost, colour = Lokacija)) + geom_point(size = 5)
ggplot(data = meritve, aes(x=Visina, y = Vsebnost, colour = Lokacija)) + geom_point(colour = "red")
ggplot(data = meritve, aes(x=Visina, y = Vsebnost, colour = Visina)) + geom_point()
ggplot(data = meritve, aes(x=Leto, y = Visina, colour = Lokacija)) + geom_line()
ggplot(data = meritve, aes(x=Vsebnost, y = Visina, colour = Lokacija)) + geom_line()
ggplot(data = meritve, aes(x=Lokacija, y = Visina)) + geom_boxplot()
ggplot(data = meritve, aes(x= Visina)) + geom_histogram()
ggplot(data = meritve, aes(x= Visina)) + geom_histogram(bins=50)
ggplot(data = meritve, aes(x= Visina, fill = Lokacija)) + geom_histogram(bins=50)
meritve$Leto <- as.factor(meritve$Leto)
ggplot(data = meritve, aes(x= Lokacija, y = Vsebnost, fill = Leto, group = Leto)) + geom_col(position="dodge")
ggplot(data = meritve[1:50,], aes(x = Leto, y = Vrednost2, colour = Skupina)) +geom_line()
ggplot(data = meritve[1:100,], aes(x = Vrednost1, y = Vrednost2, colour = Skupina)) + geom_line()
ggplot(data = meritve, aes(x = Vrednost1, y = Vrednost2, colour = Skupina)) + geom_point() + 
  theme(panel.background = element_blank())
ggplot(data = meritve) +   geom_point(aes(x = Vrednost1, y = Vrednost2, colour = Skupina)) + 
               geom_line(aes(x = Vrednost1, y = Vrednost1))
                         
ggplot(data = meritve, aes(x = Vrednost1, y = Vrednost2, colour = Vrednost1)) + geom_point()
ggplot(data = meritve, aes(x = Vrednost1, y = Vrednost2, colour = Skupina)) + geom_point(colour="red")
ggplot(data = meritve, aes(x = Vrednost1, y = Vrednost2, colour = Vrednost2)) + geom_point() + 
  scale_colour_fermenter(palette = "YlOrRd")
ggplot(data = meritve, aes(x = Visina, y = Vsebnost, colour = Lokacija)) + 
  #geom_point() + scale_colour_manual("Lokacija", values = c("red", "green", "black", "blue", "yellow"))
  geom_point() + scale_colour_manual("Skupine", values = c("#d14996", "#b7e34f",  "#4ab6cf", "#e39f46"))

ggplot(data = meritve, aes(x = Skupina, y = Vrednost1)) + geom_boxplot()

ggplot(data = meritve, aes(x = Vrednost1, y = Vrednost2, group=Skupina, fill = Skupina)) + geom_boxplot()
p <- ggplot(data = meritve, aes(x = Leto, y = Vrednost2, colour = Skupina)) + geom_point() 
p
p + theme(panel.grid = element_blank())
p + theme(axis.title.x = element_text(size=16))
p + theme(axis.title = element_blank())
p + theme(panel.background = element_blank(), axis.title.x = element_text(size=16))
p + theme(axis.text.x = element_text(angle = 90, size=12))
p + theme(legend.position = "top")
ggplot(data = meritve[1:100,], aes(x = Leto, y = Vrednost2, group = Skupina, colour = Skupina)) + geom_line()



ggplot(data = meritve, aes(x = Vrednost1, fill = Skupina)) + geom_histogram(bins = 100)
ggplot(data = meritve, aes(x = Skupina, y = Vrednost2)) + geom_bar(stat="identity", position = "dodge")

library(dplyr)
meritve <- data.frame(ID = 1:1000, Vrednost1 = rnorm(1000), Vrednost2 = rnorm(1000),
                      Skupina = rep(c("A", "B", "C", "D"), 250),
                      Leto = rep(1:2, each = 500))

head(meritve)
table(meritve$Skupina, meritve$Leto)
povpVisina <- meritve %>% group_by(Lokacija, Leto) %>% 
                      summarize(Povprecje = mean(Visina), 
                                 StOdklon = sd(Visina))
povpVisina$Leto <- as.factor(povpV1$Leto)
head(povpVisina)
ggplot(data = as.data.frame(povpVisina), aes(x = Leto, y = Povprecje, group= Lokacija, colour = Lokacija)) + geom_line() + geom_point()
ggplot(data = povpVisina[povpV1$Leto %in% 2001:2002,], aes(x=Lokacija, y = Povprecje, fill = Leto)) + geom_col()
ggplot(data =  povpVisina[povpV1$Leto %in% 2001:2002,], aes(x=Lokacija, y = Povprecje, fill = Leto)) + geom_col(position="dodge")
povpV1

meritve

meritve1 <- data.frame(ID = 1:1000, Vrednost1 = c(sample(27:30, 250, replace = T), sample(15:20, 250, replace = T), sample(33:38, 250, replace = T), sample(30:35, 250, replace = T)),
                      Skupina = rep(c("Ljubljana", "Glasgow", "Marakeš", "Lizbona"), each = 250),
                      Leto = rep(2001:2010, 100))
table(meritve1$Leto)
meritve1_sd <- meritve1 %>% group_by(Skupina, Leto) %>% dplyr::summarize(povpTemp = mean(Vrednost1), soTemp = sd(Vrednost1))
meritve1_sd$Leto <- as.factor(meritve1_sd$Leto)
meritve1_sd$Skupina <- as.factor(meritve1_sd$Skupina)
table(meritve1_sd$Leto, meritve1_sd$Skupina)
head(meritve1_sd)

ggplot(data = povpVisina, aes(x = Leto, y = Povprecje, group = Lokacija, colour = Lokacija)) + geom_line() + geom_ribbon(aes(ymin = Povprecje - StOdklon, ymax = Povprecje + StOdklon, fill = Lokacija), alpha = 0.2, linetype = 0)
