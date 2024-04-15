table<- xtable(etable(Africa), tex=TRUE)
print(table, caption = "Total Conflicts", caption.placement = "top", file = "^Tables/glm_migration_africa_1.tex", include.rownames = FALSE)
