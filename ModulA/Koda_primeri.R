temp <- data.frame(Mesec = rep(c("Julij", "Avgust"), 2), 
                   Ljubljana = c(34, 28, 30, 31), 
                   Maribor = c(32, 30, 26, 26), 
                   Datum = c(1, 15, 1, 15))
temp$Ljubljana <- as.numeric(temp$Ljubljana)
temp$Maribor <- as.numeric(temp$Maribor)

temp <- as.numeric(temp)

temp
rowSums(temp[,2:3])

colMeans(temp[,2:3])


var(temp[,2:3])


tempM <- melt(temp, id.vars = "Mesec", variable_name = "Mesto")
tempM

aggregate(tempM$value ~ tempM$Mesec, FUN="mean")

aggregate(tempM$value ~ tempM$Mesec + tempM$Mesto, FUN="mean")



temp %>% pivot_longer(cols = c(Ljubljana, Maribor), names_to="Mesto") %>% 
          unite("MesecDatum", Mesec, Datum, sep="_", remove = F) %>% 
          group_by(MesecDatum, Mesto) %>% 
          summarize(povpTemp = mean(value))
