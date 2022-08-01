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

# SET UP BASES REDUCIDAS --------------------------------------------------

# Inspeccionar tabla
glimpse(personas)

# Inspeccionar columnas y etiquetas
personas$CMHP17 %>% class
personas$CMHP17 %>% attr('label')
personas$CMHP17 %>% attr('labels')

# Ver todas las etiquetas
view_df(personas)

# PERSONAS
cols_personas <- c("ENNUMC", "LIN", "CMHP17", "CMHP18", "CMHP19",
                   "CMHP22", "EMHP28N", "EMHP28A", "EMHP28S",
                   "EMHP32", "TMHP36", "TMHP41", "TMHP43",
                   "TMHP44", "TMHP44BS", "TMHP48", "TMHP45BS",
                   "PMHP60BS", 
                   "PESOPERSONA", "GRPEDAD", "AESTUDIO", "Tciudad_max")

# Filtro por las columnas de interes
personas_reduc <- personas %>%
  select(all_of(cols_personas))


# MODIFICACIONES ADICIONALES ----------------------------------------------

# Guardo el archivo para trabajar en clases
write_sav(personas_reduc, "C:/Users/Samuel/Desktop/Data.Science UCAB/Clase10/encovi_personas2017_ds.sav")

# Modificaciones para quedar igual que uds
personas <- personas_reduc


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

personas_new <- personas %>%
  mutate(ing_laboral_imp = ing_laboral)

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











