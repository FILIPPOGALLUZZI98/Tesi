if (!require(gridExtra)) install.packages("gridExtra", dependencies = TRUE)
if (!require(grid)) install.packages("grid", dependencies = TRUE)

# Carica i pacchetti
library(gridExtra)
library(grid)

# Mettere nome del file
nome_file <- ""

csv_path <- paste0("^Tabelle", nome_file, ".csv")
df <- read.csv(csv_path)
create_table <- function(df) {
  table <- tableGrob(df)
  return(table)}
table <- create_table(df)
pdf_file <- paste0(nome_file, ".pdf")
pdf(pdf_file, width = 25, height = 7) # specifica le dimensioni della pagina
grid.draw(table)
dev.off()
