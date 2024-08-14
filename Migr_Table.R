data_1 <- subset(gm, interval==1)
model <- fixest::feglm(data=data_1, n_migr~sw(n_value,n_gws_avg5,n_gws_avg10,gws_anomalies, gws_anomalies5, gws_anomalies10,CV1, CV5, CV10,gws_logret, gws_logret5, gws_logret10)|region + year, family=gaussian)
tabella <- etable(model)
tabella_df <- as.data.frame(tabella)

obs <- tabella_df[20, ]; obs <- t(obs);obs <- obs[-1, ]
sqr <- tabella_df[21,]; sqr <- t(sqr); sqr <- sqr[-1,]
pseudo <- tabella_df[22,]; pseudo <- t(pseudo); pseudo <- pseudo[-1,]
bic <- tabella_df[23,]; bic <- t(bic); bic <- bic[-1,]

tabella_df <- head(tabella_df, -9)
tabella_df <- tabella_df[-c(1, 2), ]
valori_diagonali <- diag(as.matrix(tabella_df[,-1]))
tabella_df[,2] <- valori_diagonali
tabella_df <- tabella_df[, 1:2]
tabella_df$observations <- obs
tabella_df$squared <- sqr
tabella_df$pseudo <- pseudo
tabella_df$bic <- bic
tabella_df <- tabella_df[,-1]
colnames(tabella_df)[c(1, 2, 3,4,5)] <- c("Coefficients", "Observations","Squared Cor.", "Pseudo R2", "BIC")
rownames(tabella_df)[c(1:12)] <- c("GWS", "GWS 5-y", "GWS 10-y","GWS Anomalies 1-y", "GWS Anomalies 5-y", "GWS Anomalies 10-y",
                                   "Coefficient of Variation 1-y", "Coefficient of Variation 5-y", "Coefficient of Variation 10-y",
                                   "Logarithmic Return 1-y", "Logarithmic Return 5-y", "Logarithmic Return 10-y")





