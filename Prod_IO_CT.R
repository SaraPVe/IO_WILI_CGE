# Propuesta RM: hacer una tabla con los sectores que sirva de nexo para unizar y wili

library(openxlsx)
library(tidyverse)

#**MWMWMWMWMMWMWMWMWMMWMWMWMWMMWMWMWMWMMWMWMWMWMMWMWMWMWMMWMWMWMWM
# CARGAR Y LIMPIAR UNIZAR -------------

M_unizar <- read.xlsx("data_unizar.xlsx", sheet = 3)  # Leer solo la tercera hoja
countries_unizar <- c("AUSTRIA", M_unizar[[1]])  # Los países están en la primera columna (soy un poco tonta y falta austria)
names_unizar <- expand.grid(as.character(1:48), countries_unizar) |> rename(in_sec_uz = Var1, in_cou = Var2)

# meter la matriz tocha
unizar_m <- read.xlsx("data_unizar.xlsx", sheet = 1, colNames = F)

# unificar sectores y paises para meterlo como nombre de columna
colnames_unizar <- names_unizar |> unite(col = "colnames_unizar")

# meterlo como nombre de columna
colnames(unizar_m) <- colnames_unizar$colnames_unizar

# meter nombres de file
unizar_done <- cbind(names_unizar, unizar_m)

# pivorar a lo largo: (todo menos las dos primeras columnas (que son los nombres de paises y sectores))
# despues separo los nombres de las columnas y convierto los sectores en caracteres par ahacer bien el join despues
unizar_pivot <- unizar_done |> 
  pivot_longer(-c(1,2), names_to = "tmp", values_to = "unizar_values") |> 
  separate(tmp, into = c("out_sec_uz", "out_cou"), sep = "_") |> #este paso toma bastante tiempo,  buscar forma más eficiente
  mutate(out_sec_uz = as.character(out_sec_uz),
  in_sec_uz = as.character(in_sec_uz)
)

# save(unizar_pivot,file = "./Data_CT/unizar_pivot.RData")
load("./Data_CT/unizar_pivot.RData")

#**MWMWMWMWMMWMWMWMWMMWMWMWMWMMWMWMWMWMMWMWMWMWMMWMWMWMWMMWMWMWMWM
# CARGAR Y LIMPIAR WILIAM -------------

# cargar matriz tocha
W_norm <- read.xlsx("W_normalizada.xlsx")

# pivotar a lo largo. Primero tengo que preparar la matriz, y despues de pivotar algo algo muy similar a lo que hacía con unizar
wiliam_pivot <- W_norm |> 
  rename(in_wil = 1) |> 
  separate(in_wil, into = c("in_cou", "in_sec_wi"), sep = "-") |> 
  pivot_longer(cols = -c("in_cou", "in_sec_wi"), names_to = "tmp", values_to = "wiliam_values") |> 
  separate(tmp, into = c("out_cou", "out_sec_wi"), sep = "-")  
    
# save(wiliam_pivot,file = "./Data_CT/wiliam_pivot.RData")
load("./Data_CT/wiliam_pivot.RData")



#**MWMWMWMWMMWMWMWMWMMWMWMWMWMMWMWMWMWMMWMWMWMWMMWMWMWMWMMWMWMWMWM
# UNIRLAS TABLAS POR FIN ----------------------

# limpiar todo menos mis dos matrices pivotadas
rm(list = setdiff(ls(), c("wiliam_pivot", "unizar_pivot")))

# la tabla nexo
sector_join <- read.xlsx("./info/Correspondance_final.xlsx", sheet = "Rafa_intermediate_wili") |> 
  mutate_all(~as.character(.)) #importante que sea caracter

# check nombres de paises y sectores
a = distinct(wiliam_pivot |> select(in_cou))
b = distinct(unizar_pivot |> select(in_cou))
a == b

a = distinct(wiliam_pivot |> select(out_cou))
b = distinct(unizar_pivot |> select(out_cou))
a == b

a = distinct(wiliam_pivot |> select(in_sec_wi) |> arrange(in_sec_wi))
b = distinct(sector_join |> select(code_wi)|> arrange(code_wi))
a == b


# union de la tabla nexo con la de wiliam. Uno primero los setores IN y despues OUT
# importante le cambio el nombre  para que esté acorde
wiliam_join <- wiliam_pivot |> 
  left_join(sector_join |> rename(in_sec_uz = code_za), by = c("in_sec_wi" = "code_wi")) |> 
  left_join(sector_join |> rename(out_sec_uz = code_za), by = c("out_sec_wi" = "code_wi"))
  
# ahora que tiene bien el nexo, procedo a attach la matriz
# con el full_join me aseguro de que "duplican" los valores de zar para que no queden huecos libres 
# en la matriz final ( que debe tener el tamaño de wili)
# para que se una tiene que cumplir que sean iguales: pais de entrada, salida, y los sectores de entrada y salida de unizar

  wiliam_unizar <- wiliam_join |> 
    full_join(unizar_pivot,
    by = c("in_cou", "out_cou", "in_sec_uz", "out_sec_uz")) 
  
  
  # save(wiliam_unizar,file = "./Data_CT/wiliam_unizar.RData")
  load("./Data_CT/wiliam_unizar.RData")


# ahora calculo el producto (wiliam_values * unizar_values)
# limpio la matriz y la preparo para su pivote
# y la pivoto a lo ancho
wiliam_wide_c <- wiliam_unizar |> 
  mutate(values = wiliam_values * unizar_values) |> 
  unite(c("in_cou", "in_sec_wi"), col = "insecou") |> 
  unite(c("out_cou", "out_sec_wi"), col = "outsecou") |> 
  select(-c("in_sec_uz", "out_sec_uz", "unizar_values", "wiliam_values")) |> 
  pivot_wider(names_from = "outsecou", values_from = "values")

save(wiliam_wide_c, file = "./Data_CT/wiliam_wide_c.RData")
# write.xlsx(wiliam_wide_c, "IO_wiliam.xlsx")
