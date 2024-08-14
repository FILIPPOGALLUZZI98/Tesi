confl_tabella <- function(tabella) {
  tabella_df <- as.data.frame(tabella)
  
  obs <- tabella_df[20, ]; obs <- t(obs); obs <- obs[-1, ]
  sqr <- tabella_df[21,]; sqr <- t(sqr); sqr <- sqr[-1,]
  
  tabella_df <- head(tabella_df, -7)
  tabella_df <- tabella_df[-c(1, 2), ]
  
  valori_diagonali <- diag(as.matrix(tabella_df[,-1]))
  tabella_df[, 2] <- valori_diagonali
  tabella_df <- tabella_df[, 1:2]
  
  tabella_df$observations <- obs
  tabella_df$squared <- sqr
  
  colnames(tabella_df)[c(1, 2, 3, 4)] <- c("Variables", "Coefficients", "Observations", "Squared Cor.")
  tabella_df$Variables <- c(
    "GWS", "GWS 5-y", "GWS 10-y", "GWS Anomalies 1-y", "GWS Anomalies 5-y", "GWS Anomalies 10-y",
    "Coefficient of Variation 1-y", "Coefficient of Variation 5-y", "Coefficient of Variation 10-y",
    "Logarithmic Return 1-y", "Logarithmic Return 5-y", "Logarithmic Return 10-y")
  
  return(tabella_df)}



















