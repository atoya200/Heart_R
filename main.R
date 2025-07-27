#Cargamos librerias
library(tidyverse)
library(janitor)
library(readxl)
library(pheatmap)
library(dplyr)
library(rlang)


# Funciones
# Como son varias variables lo que hacemos es generar una función
# que cree un data_frame vacio y luego le vaya haciendo
# binds con el resultado de calcular la frecuencia absoluta y la 
# relativa para cada valor pasado en en vector
calcular_freqs = function(data_frame, lista_variables) {
      freqs_data_frame <- data.frame(valor = logical(),
                                     frec_abs = numeric(),
                                     frec_rel = numeric(),
                                     variable = character())
      
      cant_filas = nrow(data_frame)
      
      for(var in lista_variables) {
            var_sym <- sym(var) 
            freqs_data_frame = freqs_data_frame |> 
                  bind_rows(
                        data_frame |> 
                              count(!!var_sym, name = "frec_abs") |> 
                              rename(valor = !!var_sym) |> 
                              mutate(
                                    frec_rel=frec_abs/cant_filas,
                                    variable = var
                              ) 
                  )
      }
      
      return(freqs_data_frame)
}


# Al finalizar el script guadamos las imagenes de las graficas
guardar_archivos = function(vector_archivos){
      for(v in vector_archivos){
            nombre = v[1]
            plot = v[2]
            ggsave(
                  nombre,
                  plot = plot,
                  device = NULL,
                  path = NULL,
                  scale = 1,
                  width = NA,
                  height = NA,
                  units = c("in", "cm", "mm", "px"),
                  dpi = 300,
                  limitsize = TRUE,
                  bg = 'white',
                  create.dir = FALSE
            )
      }
      
      return(TRUE)
}

# Cargamos los valores del archivo fuente
datos = read_csv("datasets/simulated_HF_mort_data_for_GMPH.csv") 

# Variables que vamos a tener en cuenta para ver las enferemdades o el grado
# de enfermedad que tenía el paciente antes de ingresar
variables_enfermedades = c('copd', 
                           'obesity', 
                           'renal_disease',
                           'hypertension',
                           'ihd',
                           'pvd',
                           'valvular_disease',
                           'cancer',
                           'pneumonia')


vector_archivos = c()


# Cantidad de filas y columnas originalmente
dimensiones = dim(datos)

print(paste("Filas", dimensiones[1]))
print(paste("Columnas", dimensiones[2]))


# Revisamos los nombres de las variables/columnas
colnames(datos)

# Comprobar si hay datos duplicados
cant_duplicados = datos |> 
      duplicated() |> 
      sum()

# Usamos summary para ver si hay o no NAs en cada columna
summary(datos)


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

# Agregamos una columna con la suma de las enferemdades que consideramos importantes
# como forma de crear una metrica de que tan enfermo estaba cuando tuvo el fallo cardiaco
filtrados$ponderacion_enfermedades = rowSums (filtrados[ , variables_enfermedades])

# Al poner el drop_na al final, hacemos que se eliminen los registros
# luego de hacer el filtrado. Si se hiciera antes del filter
# quedarían cerca de 950 registros de 1000 por el faltante 
# en la variable de grupo étnico, el cual para la pregunta planteada
# no nos es de utilidad

# Primero vamos a obtener dos subconjuntos, uno de vivos y otro de fallecidos
sobrevivientes = filter(filtrados, death == 0)
fallecidos = filter(filtrados, death == 1)



# Maximo y mínimo de edades por fallecidos/sobrevivientes 
# luego de la falla cardíaca
edades  = data.frame(
            "estado" =  c("Vivo", "Muerto"),
            "edad_maxima" = c(max(sobrevivientes$age),max(fallecidos$age)), 
            "edad_minima" = c(min(sobrevivientes$age),min(fallecidos$age))
          )
      

# Frecuencias relativas y absolutas

# Total de registros del dataframe filtrado
cant_filas = nrow(filtrados)

# Frecuencia relativa de la variable fallecimiento y de las enfermedades
# seleccionadas anteriormente



# Calculamos la frecuencia de cada variable categorica
freqs = calcular_freqs(filtrados, append(variables_enfermedades, 'death') )


# Ahora calculamos las frecuencias pero solo para aquellos que fallecieron
freqs_f = calcular_freqs(fallecidos, variables_enfermedades)

# Ahora calculamos las frecuencias pero solo para aquellos que sobreviveron
freqs_s = calcular_freqs(sobrevivientes, variables_enfermedades)

# Esto responde a cual era la patología más común que presentaban los supervivientes
filtrados_s = filter(freqs_s, valor == 1, .preserve = T) 
max_freq_s =  round(max(filtrados_s$frec_rel) * 100, 2)
wich_max_freq_s = filtrados_s[4, which.max(filtrados_s$frec_rel)]
print(paste("De las personas que sobrevivieron ", max_freq_s, "% teninan ", wich_max_freq_s))


# Esto responde a cual es la enfermedad más común que padecen los fallecidos por 
# falla cardiaca
filtrados_f = filter(freqs_f, valor == 1, .preserve = T) 
max_freq_f =  round(max(filtrados_f$frec_rel) * 100, 2)
wich_max_freq_f = filtrados_f[4, which.max(filtrados_f$frec_rel)]
print(paste("De las personas fallecidas ", max_freq_f, "% teninan ", wich_max_freq_f))


# Calculo de mediana, media y desviación estándar de la edad de los pacinentes
mediana = median(filtrados$age)
media = mean(filtrados$age)
desviacion = round(sd(filtrados$age), 2)

# Vamos con los graficos

# Grafico de Barras
# Cantidad por estado
filtrados |> 
      mutate(death = if_else(death == 0, 'Muertos', 'Vivos')) |> 
      ggplot(aes(x=death,  fill=death))+
      labs(
            title = "Representación de la cantidad de fallecidos y supervivientes",
            x = "Estado",
            y = "Cantidad",
      ) +
      scale_y_continuous(breaks = seq(0, cant_filas, by = 20)) +
      geom_bar() +
      theme(legend.position = "none")+
      coord_flip()

# Calculamos por precencia de enfermedad en fallecidos
filtrados_f |> 
      ggplot(aes(x=variable, y=round(frec_rel * 100, 3),  fill=variable))+
      labs(
            title = "Representación grafica de porcentaje de precencia de enfermedades en los fallecidos",
            x = "Enfermedad",
            y = "Porcentaje",
      ) +
      geom_bar(stat = "identity") +
      theme(legend.position = "none")+
      coord_flip()


# Calculamos por precencia de enfermedad en sobrevivientes
filtrados_s |> 
      ggplot(aes(x=variable, y=round(frec_rel * 100, 3),  fill=variable))+
      labs(
            title = "Representación grafica de porcentaje de precencia de enfermedades en los sobrevivientes",
            x = "Enfermedad",
            y = "Porcentaje",
            color = "Enfermedad"
      ) +
      geom_bar(stat = "identity") +
      theme(legend.position = "none")+
      coord_flip()


# Falta grafica de vivos/muertos relación con la enfermedades ponderadas
freq_precencia_enfermedades = sobrevivientes |> 
                  count(ponderacion_enfermedades, name = "frec_abs") |> 
                  mutate(
                        frec_rel=frec_abs/cant_filas,
                        estado = "vivos"
                  ) |> 
                  bind_rows(
                        fallecidos |> 
                              count(ponderacion_enfermedades, name = "frec_abs") |> 
                              mutate(
                                    frec_rel=frec_abs/cant_filas,
                                    estado = "fallecidos"
                              ) 
                  )

ggplot(freq_precencia_enfermedades, aes(x = factor(ponderacion_enfermedades), y = frec_abs, fill = estado)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
            title = "Frecuencia de estados según cantidad de enfermedades",
            x = "Cantidad de enfermedades",
            y = "Frecuencia absoluta",
            fill = "Estado"
      ) +
      scale_fill_manual(values = c("vivos" = "steelblue", "fallecidos" = "firebrick")) +
      theme_minimal()


# QUIZAS LOS HISTOGRAMAS ESTÁN DE MAS, LO DEJO A TU CRITERIO

# Histograma de personas que sobrevivieron
sobrevivientes |> 
      ggplot(aes(x=age))+
      geom_histogram(fill="darkolivegreen3", color='black',
                     bins = 20)+
      labs(
            title = "Distribución de edades de los sobrevivientes",
            x = "Edad",
            y = "Cantidad",
      ) +
            scale_x_continuous(breaks = seq(0, max(sobrevivientes$age), by = 5)) +
            scale_y_continuous(breaks = seq(0, 100, by = 10)) +
            theme_minimal()

# Histograma de personas que no sobrevivieron
fallecidos |> 
      ggplot(aes(x=age))+
      geom_histogram(fill="red", color='black',
               bins = 20)+
      labs(
            title = "Distribución de edades de los fallecidos",
            x = "Edad",
            y = "Cantidad",
      ) +
      scale_x_continuous(breaks = seq(0, max(fallecidos$age), by = 5)) +
      scale_y_continuous(breaks = seq(0, 100, by = 10)) +
      theme_minimal()

# Ambos en el mismo grafico
histograma_ambos = filtrados |> 
      ggplot(aes(x = age, fill = factor(death))) +
      geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
      labs(
            title = "Distribución de edades según muerte",
            x = "Edad",
            y = "Frecuencia",
            fill = "Muerte"
      ) +
      scale_fill_manual(values = c("blue", "red"), labels = c("Sobrevivio", "Murió")) +
      scale_x_continuous(breaks = seq(0, max(filtrados$age), by = 5), limits = c(0, 100)) +
      scale_y_continuous(breaks = seq(0, 100, by = 10)) +
      theme_minimal()
       




#Dispersión
filtrados |> 
      mutate(death = if_else(death == 0, FALSE, TRUE)) |> 
      ggplot(aes(x = age, y = los, color = death)) + 
      geom_point() +
      labs(
            title = "Distribución de edades según tiempo de hospitalización con coloreado por estado",
            x = "Edad",
            y = "Días internados",
            color = "Estado"
      ) +
      scale_color_manual(
            values = c("blue", "red"),
            labels = c("Sobrevivió", "Murió")
      ) +
      scale_x_continuous(breaks = seq(0, max(filtrados$age), by = 5), limits = c(0, 100))



# De mientras, la correlación la podemos ver de esta forma
coeficientes = filtrados |> 
      #con esto selecciono todas las variables numéricas
      select(los, age) |> 
      cor() 

coeficiente = round(coeficientes[1, 2], 2)
print(parse("Coeficinete de correlacion entre la edad y el tiempo internado es de ", coeficiente ))

# Boxplot
filtrados |> 
      mutate(death = if_else(death == 0, 'No', 'Si')) |> 
      ggplot(aes(x=age,y=death,
           fill=death))+
      labs(
            title = "Diagrama de cajas en base a la edad y estado",
            x = "Edad",
            y = "Fallecio"
      ) +
      theme(legend.position = "none")+
      scale_x_continuous(breaks = seq(0, max(filtrados$age), by = 10))+
      geom_boxplot()

