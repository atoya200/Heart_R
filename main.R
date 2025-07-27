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
# binds_rows con el resultado de calcular la frecuencia absoluta y la 
# relativa para cada variable  pasada en en vector
calcular_freqs = function(data_frame, vector_variables) {
      
      # Creación del dataframe vacio
      freqs_data_frame <- data.frame(valor = logical(),
                                     frec_abs = numeric(),
                                     frec_rel = numeric(),
                                     variable = character())
      
      # Obtenemos la cantidad de filas que tiene el dataframe actual
      cant_filas = nrow(data_frame)
      
      # Iteramos sobre los elementos del vector
      for(var in vector_variables) {
            # Es como si le agregaramos el simbolo de pesos, para poder
            # acceder a cada columna
            var_sym <- sym(var) 
            
            # Reasigmanos los valores al dataframe interno
            # Donde lo primero que hacemos ees calcular la frecuencia
            # absoluta y luego la relativa
            # aclarando de que variable se trata al agregar la columna
            # variable
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
# por ende creamos una función que recibe un vector el cual contendrá
# un subvector cuyos elementos serán el nombre del archivo y el objeto ggpolot
# correspondiente
guardar_archivos = function(vector_archivos){
      for(v in vector_archivos){
            nombre = v[1]
            plot = v[2]
            ggsave(
                  nombre,
                  plot = plot,
                  scale = 1,
                  units = c("in", "cm", "mm", "px"),
                  dpi = 300,
                  limitsize = TRUE,
                  bg = 'white',
                  create.dir = 'plots'
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
                           'pneumonia',
                           'stroke')

# Definimos el vector de archivos a usar
vector_archivos = c()


# Obtemos la cantidad de filas y columnas del original
dimensiones_original = dim(datos)
print(paste("El dataset original tiene", dimensiones_original[1], "filas y", dimensiones_original[2], 'columnas'))


# Revisamos los nombres de las variables/columnas
nombres_columnas = colnames(datos)

# Comprobar si hay datos duplicados
cant_duplicados = datos |> 
      duplicated() |> 
      sum()

# Emitimos mensaje para confirmar que hay o no hay
print(paste("En el dataset origianl", if_else(cant_duplicados != 0, paste0('hay duplicados (', cant_duplicados, ')'), 'No hay duplicados' ) ))
rm(cant_duplicados)

# Usamos summary para ver si hay o no NAs en cada columna
summary(datos)


# Otra opción para ver si hay NAs es usar colSums, si solo nos interesa ese dato
# Junto a la opción anterior, se pueda hacer uso de la función names para ver
# que columnas es que tienen estos valores
cols_con_nas = names(which(colSums(is.na(datos)) > 0))
print(paste("Las columnas con na son ", paste(cols_con_nas, collapse = ', ')))



# Borramos por que no las neceistamos más
rm(hay_nas, counter, cols_con_na)

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


# Calculo de mediana, media y desviación estándar de la edad de los pacinetes fallecidos
mediana = median(fallecidos$age)
media = mean(fallecidos$age)
desviacion = round(sd(fallecidos$age), 2)

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
      scale_y_continuous(breaks = seq(0, 60, by = 5)) +
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
      scale_y_continuous(breaks = seq(0, 60, by = 5)) +
      theme(legend.position = "none")+
      coord_flip()

# Tratamos de meter ambos ne un solo grafico
freq_enfermedades = freqs_s |> 
      filter(valor == 1, .preserve = T) |> 
      mutate(
            estado = "Vivos"
      ) |> 
      bind_rows(
            freqs_f |> 
                  filter(valor == 1, .preserve = T) |> 
                  mutate(
                        estado = "Fallecidos"
                  ) 
      )


# Luego pasamoas a hacer una grafica de dos barras por enfermedad
freq_enfermedades |> 
      ggplot(aes(x = variable, y=round(frec_rel * 100, 3), fill = estado)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
            title = "Frecuencia de estados según cantidad de enfermedades",
            x = "Cantidad de enfermedades",
            y = "Frecuencia absoluta",
            fill = "Estado"
      ) +
      scale_fill_manual(values = c("Vivos" = "blue", "Fallecidos" = "red")) +
      scale_y_continuous(breaks = seq(0, 60, by = 5)) +
      theme_minimal()

#############################################################################
#  Grafica de vivos/muertos en relación con la enfermedades ponderadas
# Primero generamos un subconjunto de datos
freq_precencia_enfermedades = sobrevivientes |> 
                  count(ponderacion_enfermedades, name = "frec_abs") |> 
                  mutate(
                        frec_rel=frec_abs/cant_filas,
                        estado = "Vivos"
                  ) |> 
                  bind_rows(
                        fallecidos |> 
                              count(ponderacion_enfermedades, name = "frec_abs") |> 
                              mutate(
                                    frec_rel=frec_abs/cant_filas,
                                    estado = "Fallecidos"
                              ) 
                  )
# Luego pasamoas a hacer una grafica de dos barras por enfermedad
freq_precencia_enfermedades |> 
      ggplot(aes(x = factor(ponderacion_enfermedades), y = frec_abs, fill = estado)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
            title = "Frecuencia de estados según cantidad de enfermedades",
            x = "Cantidad de enfermedades",
            y = "Frecuencia absoluta",
            fill = "Estado"
      ) +
      scale_fill_manual(values = c("Vivos" = "blue", "Fallecidos" = "red")) +
      theme_minimal()



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
       
# Visualizamos el histograma de ambos casos
histograma_ambos


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

