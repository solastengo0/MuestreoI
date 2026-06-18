load("C:/Users/USER/MuestreoI/Proyecto/Rivera.RData")

View(riv)

install.packages("writexl")
library(writexl)
# Exporta el dataframe a Excel
write_xlsx(riv, path = "riv.xlsx")
