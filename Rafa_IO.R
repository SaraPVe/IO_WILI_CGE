# Variante Rafa: hcaer una tabla con los sectores que sirva de nexo para unizar y wili

library(openxlsx)
library(tidyverse)


# CARGAR Y LIMPIAR UNIZAR -------------

M_unizar <- read.xlsx("data_unizar.xlsx", sheet = 3)  # Leer solo la tercera hoja
countries_unizar <- c("AUSTRIA", M_unizar[[1]])  # Los países están en la primera columna
names_unizar <- expand.grid(as.character(1:48), countries_unizar) |> rename(in_sec_uz = Var1, in_cou = Var2)

# meter la matriz tocha
unizar_m <- read.xlsx("data_unizar.xlsx", sheet = 1, colNames = F)

colnames_unizar <- names_unizar |> unite(col = "colnames_unizar")

colnames(unizar_m) <- colnames_unizar$colnames_unizar
unizar_done <- cbind(names_unizar, unizar_m)

unizar_pivot <- unizar_done |> 
  pivot_longer(-c(1,2), names_to = "tmp", values_to = "unizar_values") |> 
  separate(tmp, into = c("out_sec_uz", "out_cou"), sep = "_") |> #este paso toma bastante tiempo,  buscar forma más eficiente
  mutate(out_sec_uz = as.character(out_sec_uz),
in_sec_uz = as.character(in_sec_uz)
)

# save(unizar_pivot,file = "./DataRafa/unizar_pivot.RData")
load("./DataRafa/unizar_pivot.RData")

# CARGAR Y LIMPIAR WILIAM -------------

W_norm <- read.xlsx("W_normalizada.xlsx")

# pivotar a lo largo 
wiliam_pivot <- W_norm |> 
  rename(in_wil = 1) |> 
  separate(in_wil, into = c("in_cou", "in_sec_wi"), sep = "-") |> 
  pivot_longer(cols = -c("in_cou", "in_sec_wi"), names_to = "tmp", values_to = "wiliam_values") |> 
  separate(tmp, into = c("out_cou", "out_sec_wi"), sep = "-")  
    
# save(wiliam_pivot,file = "./DataRafa/wiliam_pivot.RData")
load("./DataRafa/wiliam_pivot.RData")
      
      


rm(list = setdiff(ls(), c("wiliam_pivot", "unizar_pivot")))

# la tabla nexo
sector_join <- read.xlsx("Correspondance_final.xlsx", sheet = "Rafa_intermediate_wili") |> 
  mutate_all(~as.character(.)) 

wiliam_join <- wiliam_pivot |> 
  left_join(sector_join |> rename(in_sec_uz = code_za), by = c("in_sec_wi" = "code_wi")) |> 
  left_join(sector_join |> rename(out_sec_uz = code_za), by = c("out_sec_wi" = "code_wi"))


wiliam_unizar <- wiliam_join |> 
  left_join(unizar_pivot)


save(wiliam_unizar,file = "./DataRafa/wiliam_unizar.RData")

wiliam_wide_c <- wiliam_unizar |> 
  mutate(values = wiliam_values * unizar_values) |> 
  unite(c("in_cou", "in_sec_wi"), col = "insecou") |> 
  unite(c("out_cou", "out_sec_wi"), col = "outsecou") |> 
  select(-c("in_sec_uz", "out_sec_uz", "unizar_values", "wiliam_values")) |> 
  pivot_wider(names_from = "outsecou", values_from = "values")


