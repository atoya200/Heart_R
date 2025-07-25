library(tidyverse)
library(janitor)
library(readxl)

# Cargamos los valores del archivo fuente
datos = read_csv("datasets/simulated_HF_mort_data_for_GMPH.csv") 


# Con estos pipe hacemos: 
# Filtrado de datos 
#           id distinto de NA
#           death tiene que ser 0 o 1
# Seleccionamos una serie de variables
# Usamos distinct para quedarnos con valor únicos, tomando 
# el primero que encontremos, distinguiendo entre id y genero más que nada
# dado que si hay un dato ingresado de un paciente dos veces, pero lo que cambia
# es que en un registro tenía obesidad y en el otro no, con usar
# un distinct generico ya nos va a decir que son dos campos distintos
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
                   pneumonia) |>
                        distinct(id, age, gender, keep_all = T) |> 
                        filter(!is.na(id), (death == 0 | death == 1))


# Una vez omitidas las columnas con NA importantes para la resolución de 
# la pregunta, pasamos a remplazar los valores por el promedio de los valores
# más cercanos

# Retocar y ver con que valor remplazar
#filtrados = filtrados |> 
#      mutate(
#            gender_NA=replace_na(gender,(gender,na.rm=T)) 
#      ) |> 
#      view()

# Primero vamos a obtener dos subconjuntos, uno de vivos y otro de muertos
sobrevientes = filter(filtrados, death == 1)
fallecidos = filter(filtrados, death == 0)

# Maximo y minimo de edad de los fallecidos
edad_maxima = max(fallecidos$age)
edad_minima = min(fallecidos$age)

# Vamos a ver que tan presentes estaban las enferemedades elegidas
# en los pacientes que fallecieron

select(fallecidos, copd,diabetes,obesity,renal_disease, hypertension,ihd,pvd,valvular_disease,cancer,pneumonia)
summary(fallecidos)



# Generamos una vista sobre los datos de las enferemdades prexistentes evaluar
# los casos de fallecimientos en los que si se tenía estas condiciones
freq_rel_enferm_prexistentes  = fallecidos |> 
      count(copd,diabetes,obesity,renal_disease, hypertension,ihd,pvd,valvular_disease,cancer,pneumonia, name = "freq_abs") |> 
      mutate(copd_freq_rel = 100* freq_abs/sum(freq_abs))
      
# Solo para visualizar los datos con sus totales
freq_rel_enferm_prexistentes |> 
      adorn_totals(where = c("row")) |> 
      View()


# Ahora obtenemos la cantidad de veces que estaba presente cada enfermeedad, es
# decir, genermaosu un nuevo df con los datos de totales, para tener esa info
totales = fallecidos |> 
            mutate(
                  cantidad_copb = ?count()
            )

# A continuación lo que voy a hacer es buscar sobre precondiciones de tratamietno
# es decir, marcapasos, operaciones, etc