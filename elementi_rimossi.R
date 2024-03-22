nomi_rimossi
elementi <- c(25, 15, 12, c(5, 6), 27, 19, 10, 11, 9, 13, 14, 39, 14, 13, 5, 20, 38, 52, 20)


state <- subset(shp, CNTRY_NAME == "United States")
state <- state[-elementi[18],]

for (i in seq_along(nomi_rimossi)){
  state <- subset(shp, CNTRY_NAME == nomi_rimossi[i])
  state <- state[-elementi[i], ]
  
}

plot(state[,"geometry"])  


# elementi geometry empty
argentina 25
ecuador 15
ethipia 12
fiji 5, 6
france 27
honduras 19
israel 10
lesotho 11
mali 9
morocco 13
nicaragua 14
nigeria 39
paraguay 14
slovenia 13
south africa 5
spain 20
uganda 38
stati uniti 52
uruguay 20














