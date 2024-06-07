library(readr)
library(gridExtra)
library(ggplot2)



csv_file <- "^Tabelle/conflicts_Africa.csv"
df <- read_csv(csv_file)

# Crea una tabella utilizzando ggplot2
table_plot <- tableGrob(df)

# Salva la tabella in un file PDF
pdf_file <- "output2.pdf"
pdf(pdf_file, width =38, height = 13) 
grid.draw(table_plot)
dev.off()

cat("Tabella salvata in", pdf_file, "\n")
