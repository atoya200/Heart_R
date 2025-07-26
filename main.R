#Cargamos librerias
library(tidyverse)
library(janitor)
library(readxl)
library(pheatmap)


# Cargamos los valores del archivo fuente
datos = read_csv("datasets/simulated_HF_mort_data_for_GMPH.csv") 

# Cantidad de filas y columnas originalmente
dim(datos)



# Revisamos los nombres de las variables/columnas
colnames(datos)

# Usamos summary para ver si hay o no NAs en cada columna
summary(datos)

# Comprobar si hay duplicados
cant_duplicados = datos |> 
      duplicated() |> 
      sum()

# Otra opción para ver si hay NAs es usar colSums, si solo nos interesa ese dato
colSums(datos)

# Junto a la opción anterior, se pueda hacer uso de la función names para ver
# que columnas es que tienen estos valores
names(which(colSums(is.na(datos)) > 0))


# Emitimos mensaje para confirmar que hay o no hay
print(if_else(cant_duplicados != 0, paste('Hay duplicados:', cant_duplicados), 'No hay duplicados' ) )

# Con estos pipe hacemos: 
# Filtrado de datos 
#           id distinto de NA
#           death tiene que ser 0 o 1
# Seleccionamos una serie de variables
# Usamos distinct para quedarnos con valor únicos, tomando 
# el primero que encontremos, distinguiendo entre todos los casos 
# como se pedía en la letra
filtrados =   datos |>  
      filter(!is.na(id), death == 0 | death == 1) |> 
      select(id,
             los,
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
                        drop_na() |> 
                        distinct()

# Al poner el drop_na al final, hacemos que se eliminen los registros
# luego de hacer el filtrado. Si se hiciera antes del filter
# quedarían cerca de 950 registros de 1000 por el faltante 
# en la variable de grupo étnico, el cual para la pregunta planteada
# no nos es de utilidad

# Primero vamos a obtener dos subconjuntos, uno de vivos y otro de fallecidos
sobrevientes = filter(filtrados, death == 0)
fallecidos = filter(filtrados, death == 1)

# Maximo y mínimo de edades por fallecidos/sobrevientes 
# luego de la falla cardíaca
edades  = data.frame(
            "vivo" = c(1, 0),
            "edad_maxima" = c(max(sobrevientes$age),max(fallecidos$age)), 
            "edad_minima" = c(min(sobrevientes$age),min(fallecidos$age))
          )
      

# Frecuencias relativas y absolutas

# Total de registros del dataframe filtrado
cant_filas = nrow(filtrados)

# Frecuencia relativa de la variable fallecimiento
freqs = filtrados |> 
      count(death, name = "frec_abs") |> 
      mutate(
            freq_rel=frec_abs/nro_rows
      ) |> 
      
      
# Calculo de mediana, media y desviación estándar
mediana = median(filtrados$age)
media = mean(filtrados$age)
desviacion = round(sd(filtrados$age), 2)

# Vamos con los graficos

# Grafico de Barras
# Edades y estado
filtrados |> 
      ggplot(aes(x=death))+
      geom_bar()

#Dispersión
filtrados |> 
      mutate(death = if_else(death == 0, 'No', 'Si')) |> 
      ggplot()+
      geom_point(aes(x= age,
                     y=los,
                     color=death
      )
      )

# De mientras, la correlación la podemos ver de esta forma
coeficientes = filtrados |> 
      #con esto selecciono todas las variables numéricas
      select(los, age) |> 
      cor() 

coeficiente = round(coeficientes[1, 2], 2)

# Boxplot
filtrados |> 
      mutate(death = if_else(death == 0, 'No', 'Si')) |> 
      ggplot(aes(x=age,y=death))+
      geom_boxplot()



