if (!require(gridExtra)) install.packages("gridExtra", dependencies = TRUE)
if (!require(grid)) install.packages("grid", dependencies = TRUE)

# Carica i pacchetti
library(gridExtra)
library(grid)

# Leggi il file CSV
df <- read.csv("^Tabelle/Conflict_Governance/conflicts_gov3.csv")

# Funzione per creare una tabella da un data frame
create_table <- function(df) {
  table <- tableGrob(df)
  return(table)
}

# Crea la tabella
table <- create_table(df)

# Salva la tabella in un file PDF
pdf("output.pdf", width = 40, height = 15) # specifica le dimensioni della pagina
grid.draw(table)
dev.off()
