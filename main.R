library(tidyverse)
library(readxl)

# Cargamos los valores del archivo fuente
datos = read_csv("datasets/simulated_HF_mort_data_for_GMPH.csv") 


# Con estos pipe hacemos: 
# Filtrado de datos 
#           id distinto de NA
#           death tiene que ser 0 o 1
# Seleccionamos una serie de variables
# Usamos distinct para quedarnos con valor únicos, tomando 
# el primero que encontremos
filtrados =   select(datos,
                   id,
                   death,
                   age,
                   gender,
                   copd,
                   diabetes,
                   obesity,
                   renal_disease,
                   hypertension,
                   ihd,
                   pvd,
                   valvular_disease,
                   pacemaker,
                   cancer,
                   diabetes,
                   pneumonia) |>
                        distinct() |> 
                        filter(!is.na(id), (death == 0 | death == 1))

# Una vez omitidas las columnas con NA importantes para la resolución de 
# la pregunta, pasamos a remplazar los valores por el promedio de los valores
# más cercanos

#filtrados = replace_na(LoanAmount,median(LoanAmount,na.rm=T))