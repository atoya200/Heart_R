#Cargamos librerias
library(tidyverse)
library(janitor)
library(readxl)
library(pheatmap)


# Cargamos los valores del archivo fuente
datos = read_csv("datasets/simulated_HF_mort_data_for_GMPH.csv") 


colnames(datos)

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
# REVISAR USO DE DISTINCT
filtrados =   datos |>  
      filter(!is.na(id), death == 0 | death == 1) |> 
      select(      id,
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
      distinct()



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
sobrevientes = filter(filtrados, death == 0)
fallecidos = filter(filtrados, death == 1)

# Maximo de edades por fallecidos y por vivos luego del infarto
edades  = data.frame(
      "vivo" = c(1, 0),
      "edad_maxima" = c(max(sobrevientes$age),max(fallecidos$age)), 
      "edad_minima" = c(min(sobrevientes$age),min(fallecidos$age))
)



############ Fallecidos
# Vamos a ver que tan presentes estaban las enferemedades elegidas
# en los pacientes que fallecieron

# Generamos una vista sobre los datos de las enferemdades prexistentes evaluar
# los casos de fallecimientos en los que si se tenía estas condiciones
freq_rel_enferm_prexistentes_f  = fallecidos |> 
      count(copd,diabetes,obesity,renal_disease, hypertension,ihd,pvd,valvular_disease,cancer,pneumonia, name = "freq_abs") |> 
      mutate(copd_freq_rel = 100* freq_abs/sum(freq_abs))


freq_rel_enferm_prexistentes_f  = fallecidos |> 
      count(copd) |> 
      mutate(copd_freq_rel = 100* freq_abs/sum(freq_abs))


# Solo para visualizar los datos con sus totales
freq_rel_enferm_prexistentes_f |> 
      adorn_totals(where = c("row")) |> 
      View()

fallecidos |> 
      mutate(
            fr_copd = sum(copd) / n(),
            fr_obesity = sum(obesity) / n(),
            fr_renal_disease = sum(renal_disease) / n(),
            fr_hypertension = sum(hypertension) / n(),
            fr_ihd = sum(ihd) / n(),
            fr_pvd = sum(pvd) / n(),
            frm_valvular_disease = sum(valvular_disease) / n(),
            fr_cancer = sum(cancer) / n(),
            fr_pneumonia = sum(pneumonia) / n()
      ) |> 
      select(fr_copd, fr_obesity,
             fr_renal_disease,
             fr_hypertension,
             fr_ihd, 
             fr_pvd,
             frm_valvular_disease ) |> 
      distinct() |> 
      view()

frs = fallecidos |> 
      summarise(
            copd = sum(copd) / n(),
            obesity = sum(obesity) / n(),
            renal_disease = sum(renal_disease) / n(),
            hypertension = sum(hypertension) / n(),
            ihd = sum(ihd) / n(),
            pvd = sum(pvd) / n(),
            frm_valvular_disease = sum(valvular_disease) / n(),
            cancer = sum(cancer) / n(),
            pneumonia = sum(pneumonia) / n()
      )
frs = frs |> 
            mutate (tipo = 'FRECUENCIA_RELATIVA')


tabla_f <- frs |> 
      pivot_wider(
            #primero pongo el nombre de la variable donde estarán las columnas  
            names_from = tipo,
            #luego donde están los valores
            values_from = c(copd, obesity, renal_disease, hypertension, ihd, pvd, frm_valvular_disease, cancer, pneumonia)
)

tabla_f <- frs |> 
      pivot_wider(
            #primero pongo el nombre de la variable donde estarán las columnas  
            names_from = c(tipo, copd),
            #luego donde están los valores
            values_from = c(copd, obesity, renal_disease, hypertension, ihd, pvd, frm_valvular_disease, cancer, pneumonia)
      )


fas = fallecidos |> 
      summarise(
            copd = n(),
            obesity = n(),
            renal_disease = n(),
            hypertension = n(),
            ihd =n(),
            pvd = n(),
            frm_valvular_disease = n(),
            cancer = n(),
            pneumonia = n()
      )

bind_rows(frs, fas)

bind_cols(frs, c(fas$copd))

tabla_freqs = bind_rows(list(freqs_rel = frs, freq_abs = fas), .id = "id")
tabla_freqs = bind_cols(list(freqs_rel = frs, freq_abs = c(fas$copd)), .id = "id")

cuadro_1 <- tabla_freqs |> 
      pivot_wider(
            #primero pongo el nombre de la variable donde estarán las columnas  
            names_from = c(id, id),
            #luego donde están los valores
            values_from = c(copd, obesity, renal_disease, hypertension, ihd, pvd, frm_valvular_disease, cancer, pneumonia)
      )

# otra forma
fr_copd = sum(fallecidos$copd) / count(fallecidos)
fr_obesity = sum(fallecidos$obesity) / count(fallecidos)
fr_renal_disease = sum(fallecidos$renal_disease) / count(fallecidos)
fr_hypertension = sum(fallecidos$hypertension) / count(fallecidos)
fr_ihd = sum(fallecidos$ihd) / count(fallecidos)
fr_pvd = sum(fallecidos$pvd) / count(fallecidos)
frm_valvular_disease = sum(fallecidos$valvular_disease) / count(fallecidos)
fr_cancer = sum(fallecidos$cancer) / count(fallecidos)
fr_pneumonia = sum(fallecidos$pneumonia) / count(fallecidos)

############ Vivos
# Maximo y minimo de edad de los sobrevientes
edad_maxima_s = max(sobrevientes$age)
edad_minima_s = min(sobrevientes$age)


# Generamos una vista sobre los datos de las enferemdades prexistentes evaluar
# los casos donde los pacientes sobrevivieron a una falla cardiaca
# en los que si se tenía estas condiciones
freq_rel_enferm_prexistentes_s  = sobrevientes |> 
      count(copd,diabetes,obesity,renal_disease, hypertension,ihd,pvd,valvular_disease,cancer,pneumonia, name = "freq_abs") |> 
      mutate(copd_freq_rel = 100* freq_abs/sum(freq_abs))

# Solo para visualizar los datos con sus totales
freq_rel_enferm_prexistentes_s |> 
      adorn_totals(where = c("row")) |> 
      View()

fallecidos |> 
      summarise()

filtrados|> 
      summary()
###############################


# Ahora obtenemos la cantidad de veces que estaba presente cada enfermeedad, es
# decir, genermaosu un nuevo df con los datos de totales, para tener esa info
totales = fallecidos |> 
      mutate(
            cantidad_copb = ?count()
      )



# A continuación lo que voy a hacer es buscar sobre precondiciones de tratamietno
# es decir, marcapasos, operaciones, etc








matriz <- filtrados |> 
      
      select(where(is.numeric), -id, -peacemaker) |> 
      cor()

#heatmap
pheatmap(
      #matriz de correlación
      matriz,
      #pongo para que muestre el coeficiente de correlación en el gráfico
      display_numbers = T,
      #saco clustering
      cluster_rows = FALSE, 
      cluster_cols = FALSE
)









# Por edad
filtrados |> 
      group_by(age, death) |> 
      summarise(
            cantidad = n()
      ) |> 
      ungroup()




cant_duplicados = datos |> 
                  duplicated() |> 
                  sum()

print(if_else(cant_duplicados != 0, paste('Hay duplicados:', cant_duplicados), 'No hay duplicados' ) )

# Hayar los NA
summary(datos)

datos_col = colSums(datos)
class(datos_col)

names(which(colSums(is.na(datos)) > 0))

rows(datos)

nro_rows = nrow(filtrados)

filtrados |> 
      count(death, name = "frec_abs") |> 
      mutate(freq_rel=frec_abs/nro_rows) |> 
      view();



# Calculo de mediana, media y desviación estándar
mediana = median(filtrados$age)
media = mean(filtrados$age)
desviacion = round(sd(filtrados$age), 2)
   