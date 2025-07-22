library(tidyverse)
library(readxl)
library(writexl)


datos = read_csv("datasets/simulated_HF_mort_data_for_GMPH.csv") 

colnames(datos)

filtrados = select(datos, id, death, age, gender, diabetes, hypertension, arrhythmias, copd, renal_disease, ihd, valvular_disease, prior_appts_attended, quintile, obesity)
