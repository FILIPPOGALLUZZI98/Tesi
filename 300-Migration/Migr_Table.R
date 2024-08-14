migr_tabella <- function(tabella1, tabella5) {
  # Converti la tabella in un data.frame
  tabella_df1 <- as.data.frame(tabella1)
  tabella_df5 <- as.data.frame(tabella5)
  
  obs1 <- tabella_df1[20, ]; obs1 <- t(obs1); obs1 <- obs1[-1, ]
  sqr1 <- tabella_df1[21,]; sqr1 <- t(sqr1); sqr1 <- sqr1[-1,]
  obs5 <- tabella_df5[20, ]; obs5 <- t(obs5); obs5 <- obs5[-1, ]
  sqr5 <- tabella_df5[21,]; sqr5 <- t(sqr5); sqr5 <- sqr5[-1,]
  
  tabella_df1 <- head(tabella_df1, -9)
  tabella_df1 <- tabella_df1[-c(1, 2), ]
  tabella_df5 <- head(tabella_df5, -9)
  tabella_df5 <- tabella_df5[-c(1, 2), ]
  
  valori_diagonali1 <- diag(as.matrix(tabella_df1[,-1]))
  tabella_df1[, 2] <- valori_diagonali1
  tabella_df1 <- tabella_df1[, 1:2]
  valori_diagonali5 <- diag(as.matrix(tabella_df5[,-1]))
  tabella_df5[, 2] <- valori_diagonali5
  tabella_df5 <- tabella_df5[, 1:2]
  
  tabella_df1$observations <- obs1
  tabella_df1$squared <- sqr1
  tabella_df5$observations <- obs5
  tabella_df5$squared <- sqr5
  
  colnames(tabella_df1)[c(1, 2, 3, 4)] <- c("Variables", "Coefficients 1-y", "Observations", "Squared Cor.")
  tabella_df1$Variables <- c(
    "GWS", "GWS 5-y", "GWS 10-y", "GWS Anomalies 1-y", "GWS Anomalies 5-y", "GWS Anomalies 10-y",
    "Coefficient of Variation 1-y", "Coefficient of Variation 5-y", "Coefficient of Variation 10-y",
    "Logarithmic Return 1-y", "Logarithmic Return 5-y", "Logarithmic Return 10-y")
  colnames(tabella_df5)[c(1, 2, 3, 4)] <- c("Variables", "Coefficients", "Observations", "Squared Cor.")
  tabella_df1$Coefficients5<-tabella_df5$Coefficients
  tabella_df1$Observations5<-tabella_df5$Observations
  tabella_df1$Squared5<-tabella_df5$`Squared Cor.`
  
  colnames(tabella_df1)[c(1, 2, 3, 4, 5, 6, 7)] <- c("Variables", "Coefficients 1-y", "Observations 1-y", "Squared Cor. 1-y",
                                                     "Coefficients 5-y", "Observations 5-y", "Squared Cor. 5-y")
  
  
return(tabella_df1)}



















