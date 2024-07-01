ge <- read.csv("^Data/gws_events.csv")
paesi <- c("Yemen", "Algeria", "Libya", "Egypt", "Syria", "Lebanon", "Pakistan", "Cambodia", "Myanmar", "Thailand",
           "Chile", "Peru", "Ecuador", "Paraguay", "Uruguay", "Brazil")
data <-subset(ge, type=="state")
data <- subset(data, country %in% paesi)
x1 <- data$value; x2 <- data$gws_anomalies10; x3 <- data$gws_std10; x4 <- data$CV10
y <- data$count


par(mfrow=c(2,2))
plot(x1[x1<400], y[x1<400], cex=0.3, pch=19, main="Value")  
plot(x2[x2<2], y[x2<2], cex=0.3, pch=19, main="Anomalies 10-y")
plot(x3[x3<5], y[x3<5], cex=0.3, pch=19, main="STD 10-y")
plot(x4[x4<60], y[x4<60], cex=0.3, pch=19, main="CV 10-y")





data <- subset(data, select = c(year, country, region, count, value, gws_anomalies10,gws_std10,CV10))
write.csv(data, "prova_nonlinear_relationships.csv", row.names=FALSE)






