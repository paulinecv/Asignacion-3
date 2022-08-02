library(tidyverse)
library(readxl)
library(haven)
library(data.table)
library(sjlabelled)
library(sjPlot)
library(mice)
library(survey)


# DATOS ------------------------------------------------------------------------

personas <- read_sav("C:/Users/Samuel/Desktop/Data.Science UCAB/Clase10/encovi_personas2017_ds.sav") 


# SET  UP -----------------------------------------------------------------

# PERSONAS
new_names_pers <- c("id_hogar", "id_per", "parentesco", "edad", "sexo", 
                    "sit_conyu", "nivel_edu", "edu_ano_aprobado", "edu_sem_aprobado",
                    "tipo_edu", "sit_econo", "sector_eco", "cat_ocupa",
                    "trab_remun", "ing_laboral", "horas_trab", "ing_otro",
                    "ing_pension",
                    "pesop", "grp_edad", "anos_edu", "tipo_ciudad")

# Renombrar
personas <- personas %>%
  setnames(old = colnames(.), new = new_names_pers) %>%
  
  # Convierte los identificadores a caracteres
  mutate(across(.cols = c("id_hogar", "id_per"),
                .fns = as.character))


# IMPUTACIONES -----------------------------------------------------------------

personas[personas == 98 | personas == 99] <- NA



# Cuantos NA hay por columna en la base de hogares
sapply(personas, function(x) sum(is.na(x)))

# Revisar cuantas personas declaran trabajar de forma remunerada pero no
# reportan ingresos validos

sum(!is.na(personas$trab_remun) & personas$trab_remun == 1 & (personas$ing_laboral <= 0 | is.na(personas$ing_laboral)))

# Revision de cuantas personas declaran trabajar de forma remunerada pero no 
# reportar horas trabajadas validas
sum(!is.na(personas$trab_remun) & personas$trab_remun == 1 & (personas$horas_trab <= 0 | is.na(personas$horas_trab)))

# Revision de cuantas personas no declaran trabajar de forma remunerada pero si 
# reportar horas trabajadas validas
sum(!is.na(personas$trab_remun) & personas$trab_remun == 2 & (personas$horas_trab > 0 | is.na(personas$horas_trab)))


# Alternativa con dplyr
nas_ing <- personas %>%
  filter(trab_remun == 1 & !is.na(trab_remun) &
           (ing_laboral <= 0 | is.na(ing_laboral)))

# Veamos la situacion laboral de este sector
table(nas_ing$sector_eco)


#-------------------------------------------------------------------------------

# Creacion de la Variable con las imputaciones

# Se considero usar como principal variable de criterio para la imputacion las
# horas de trabajo ya que como podemos demostrar en los codigos anteriores la 
# variable de trabajo remunerado alguno datos tienen como valor 2 (que significa
# que no posee trabajo remunerado) si poseen una cantidad de horas de trabajo, 
# por lo cual se prefirio trabajar por las horas de trabajo en las imputaciones

# Tambien se utilizaron las variables de sector economico, sexo y grupo de edad 
# para la imputacion debido a que consideramos que estas podrian dar un mejor
# funcionamiento a la imputacion

personas_new <- personas %>%
  mutate(ing_laboral_imp = ing_laboral)


# Operaciones
ing_imp <- lm(ing_laboral_imp ~  horas_trab
              + as.factor(sector_eco)
              + sexo
              + grp_edad,
              data=personas_new)

for (i in 1:nrow(personas_new)) 
{
  if(is.na(personas_new[i,"ing_laboral_imp"]))
  { personas_new[i,"ing_laboral_imp"]<-predict(ing_imp,newdata = personas_new[i,]) } 
}

table(personas_new$ing_laboral_imp)

