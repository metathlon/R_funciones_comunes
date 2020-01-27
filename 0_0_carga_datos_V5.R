# install.packages("readODS")
# install.packages("dplyr")
# install.packages("xlsx")

library(readODS)
library(dplyr)        #RECODE ojo que car tiene recode tb, así que usar dplyr::recode
library(xlsx)


#========================================================= CARGA EXCEL
# rm(list=ls())
# df <- read_ods("DATOS/BASE_TEST_2020_01_27.ods")
# DF_ORIGINAL <- df
# save(DF_ORIGINAL, file="DATOS/DF_ORIGINAL.RData")

#========================================================= CARGA DF ORIGINAL para no cargar EXCEL

rm(list=ls())
load(file="DATOS/DF_ORIGINAL.RData")
df <- DF_ORIGINAL

MAX_FARM_NUM = 10
FARMACOS_MINIMO = 10
DIVISION_TIEMPO_LUSTRO = c(2000,2005,2010,2015,2020) # division por años
DIVISION_TIEMPO_DECADA = c(2000,2010,2020) # division por años

#========================================================= TRANSFORMACIONES

df$FECHA_NAC <- as.Date.character(df$F_Nacimiento,"%d/%m/%Y")

#======================================================================================
#===================================== FACTORES =======================================
#======================================================================================

df$SEXO <- as.factor(
              dplyr::recode(df$Sexo,`0` = "0 - Hombre",
                             `1` = "1 - Mujer",
                              .default ="ERROR"
                    )
  )



df$DIAGNOSTICO <- as.factor(
                      dplyr::recode(df$Diagnostico,
                             `1` = "AR",
                             `2` = "SpA",
                             `3` = "APs",
                             `4` = "SpA_EII",
                             `5` = "Otras",
                             .default= "ERROR"
                              )
)

#------------------------------------------------
#     De farmacos a factores
#------------------------------------------------
# - Lo primero es hacerlo todo numerico
# - Despues recodificamos los numeros en texto
# - Por ultimo ese texto pasa a ser un factor
#------------------------------------------------

#Farmaco_9 ESTA VACIO, esto se puede ampliar si fuera necesario
for(n in 1:MAX_FARM_NUM) {
  #-------------------------------------------------------------- FARMACOS
  var_original <- paste("Farmaco_",n,sep = "")  
  var_numerica <- paste("FARMACO_NUM_",n,sep = "")  
  var_factor <- paste("FARMACO_",n,sep = "")  
  var_posicion <- paste("FARMACO_POS_",n,sep = "")  
  
  
  if(class(df[[var_original]]) != "numeric"){
    df[[var_numerica]] <- as.numeric(df[[var_original]])
  } else{
    df[[var_numerica]] <- df[[var_original]]
  } 
  
  # cat(paste("\n VAR ->",var_factor))
  
  if(class(df[[var_numerica]]) == "numeric"){
    df[[var_factor]] <- dplyr::recode(df[[var_numerica]], 
                               `0` = "Ninguno",
                               `1` = "Remicade",
                               `2` = "Enbrel",
                               `3` = "Humira",
                               `4` = "MabThera",
                               `5` = "Orencia",
                               `6` = "RoActemra",
                               `7` = "Simponi",
                               `8` = "Certolizumab",
                               `9` = "Benlysta",
                               `10` = "Stelara",
                               `11` = "Otezla",
                               `12` = "Cosentyx",
                               `13` = "Xeljanz",
                               `14` = "Olumiant",
                               `15` = "Kenvzara",
                               `16` = "Otros",
                               .default= "ERROR")
    
    df[[var_factor]] <- as.factor(df[[var_factor]])
  } else {
    df[[var_factor]] <- NA
  }
  
  #-------------------------------------------------------------- FECHAS
  var_f_ini <- paste("FECHA_INI_",n,sep = "")
  var_f_ini_original <- paste("Fecha_Ini_",n,sep = "")
  
  var_f_fin <- paste("FECHA_FIN_",n,sep = "")  
  var_f_fin_original <- paste("Fecha_Fin_",n,sep = "")
  
  f_ini_temp <- as.Date.character(df[[var_f_ini_original]],"%d/%m/%Y")
  if (length(f_ini_temp) > 0) df[[var_f_ini]] <- f_ini_temp
  else df[[var_f_ini]] <- NA

  f_fin_temp <- as.Date.character(df[[var_f_fin_original]],"%d/%m/%Y")
  if (length(f_fin_temp) > 0) df[[var_f_fin]] <- f_fin_temp
  else df[[var_f_fin]] <- NA
  
  #-------------------------------------------------------------- MOTIVOS
  var_motivo <- paste("Motivo_Susp_",n,sep = "")  
  var_motivo_num <- paste("MOTIVO_SUSP_NUM_",n,sep = "")  
  var_motivo_factor <- paste("MOTIVO_SUSP_",n,sep = "")  
  
  if(class(df[[var_motivo]]) != "numeric"){
    df[[var_motivo_num]] <- as.numeric(df[[var_motivo]])
  } else{
    df[[var_motivo_num]] <- df[[var_motivo]]
  } 
  
  if(class(df[[var_motivo_num]]) == "numeric"){
    df[[var_motivo_factor]] <- dplyr::recode(df[[var_motivo_num]], 
                                      `0` = "No suspensión",
                                      `1` = "Ineficacia",
                                      `2` = "AA",
                                      `3` = "Remisión",
                                      `4` = "Otros Motivos",
                                      .default = "ERROR"
                                      )
  } else {
    df[[var_motivo_factor]] <- NA
  }
  df[[var_motivo_factor]] <- as.factor(df[[var_motivo_factor]])

  #-------------------------------------------------------------- ABANDONO
  var_abandono <- paste("ABANDONO_",n,sep = "")  
  abandonos <- c("AA","Ineficacia","Otros Motivos")
  
  
  df[[var_abandono]] <- (as.character(df[[var_motivo_factor]]) %in% abandonos)
  
}

rm(list=c("n" ,"var_numerica","var_original","var_factor",
          "var_motivo" ,"var_motivo_num" ,"var_motivo_factor","var_abandono" , "abandonos" ))

#------------------------------------------------------------------- CONTAMOS CUANTOS FARMACOS TOMA CADA PACIOENTE
farmacos <- c("Ninguno","Remicade","Enbrel","Humira","MabThera",
              "Orencia","RoActemra","Simponi","Certolizumab",
              "Benlysta","Stelara","Otezla","Cosentyx","Xeljanz",
              "Olumiant","Kenvzara","Otros","ERROR")
farmacos <- farmacos[ farmacos != "Ninguno" & farmacos != "ERROR" ]


for(n in 1:MAX_FARM_NUM) {
  if (n == 1 ) farm_vars <- paste("FARMACO_",n,sep = "")
  else farm_vars <- c(farm_vars,paste("FARMACO_",n,sep = ""))
}


df$TOTAL_FARMACOS <- rowSums( sapply(df[,farm_vars], "%in%", farmacos))
rm(farmacos)

#----------------------------------------------------- Sujetos que toman mas de X farmacos
df$TOTAL_MAYOR_5 <- df$TOTAL_FARMACOS >= 5


#----------------------------------------------------- EDAD
df$EDAD <- as.numeric( (df$FECHA_INI_1 - df$FECHA_NAC)/365.25)


#==================================================== 
#GESTION DE ERRORES
#==================================================== 
#----------------------------------------------------- SEXO
# table(df$SEXO)
file_error_sexo <- "ERRORES/Errores_sexo_MODIFICAR_EN_LA_ORIGINAL.xlsx"
if (file.exists(file_error_sexo)) file.remove(file_error_sexo)
if (nrow(df[df$SEXO == "ERROR",]) > 0 ) write.xlsx(df[df$SEXO == "ERROR",],file_error_sexo)


df$SEXO[df$SEXO == "ERROR"] <- NA
df$SEXO <- factor(df$SEXO)

#----------------------------------------------------- DIAGNOSTICO
# table(df$DIAGNOSTICO)
file_error_diagnostico <- "ERRORES/Errores_diagnostico_MODIFICAR_EN_LA_ORIGINAL.xlsx"
if (file.exists(file_error_diagnostico)) file.remove(file_error_diagnostico)
if (nrow(df[df$DIAGNOSTICO == "ERROR",]) > 0 ) write.xlsx(df[df$DIAGNOSTICO == "ERROR",],file_error_diagnostico)

df$DIAGNOSTICO[df$DIAGNOSTICO == "ERROR"] <- NA
df$DIAGNOSTICO <- factor(df$DIAGNOSTICO)

#----------------------------------------------------- EDAD
# summary(df$EDAD)
file_error_edad <- "ERRORES/Errores_edad_MODIFICAR_EN_LA_ORIGINAL.xlsx"
if (file.exists(file_error_edad)) file.remove(file_error_edad)
if (nrow(df[df$EDAD <= 0,]) > 0) write.xlsx(df[df$EDAD <= 0,],file_error_edad)

df$EDAD[df$EDAD <= 0] <- NA


#----------------------------------------------------- DATOS LISTOS PARA ANALIZAR
save(df, file= "DATOS/DF_ANALIZAR.RDAta")

#===============================================================================================
#                   Cada toma de farmacos es un registro
#===============================================================================================

for(n in 1:MAX_FARM_NUM) {
  var_farm <- paste("FARMACO_",n,sep = "")  
  var_f_ini <- paste("FECHA_INI_",n,sep = "")  
  var_f_fin <- paste("FECHA_FIN_",n,sep = "")  
  var_motivo <- paste("MOTIVO_SUSP_",n,sep = "")  
  var_abandono <- paste("ABANDONO_",n,sep = "")  
  
  if(n > 1) rm(df_t)
  cat(paste("\n",n))
  
  df_t <- df[,c("Numero","DIAGNOSTICO","SEXO",var_farm,var_f_ini,var_f_fin, var_motivo, var_abandono)]
  names(df_t) <-c("Sujeto","DIAGNOSTICO","SEXO","FARMACO","F_INI","F_FIN","MOTIVO_SUSP","ABANDONO")
  
  df_t$ORDEN <- n
  
  if (!exists("DF_FARMACOS")){
    DF_FARMACOS <- data.frame(df_t)
  } else {
    DF_FARMACOS <- rbind(DF_FARMACOS,df_t)
  }
}

rm(list = c("n","var_farm","var_f_ini","var_f_fin","df_t", "var_motivo","var_abandono"))

#--------------------------------------------------------- Para aquellos que siguen ponemos de fecha fin hoy
DF_FARMACOS$F_FIN[ (DF_FARMACOS$MOTIVO_SUSP == "No suspensión" | is.na(DF_FARMACOS$MOTIVO_SUSP))  & DF_FARMACOS$FARMACO != "Ninguno"] <- Sys.Date()

DF_FARMACOS$TIEMPO <- (DF_FARMACOS$F_FIN - DF_FARMACOS$F_INI)/365.25

DF_FARMACOS <- DF_FARMACOS[DF_FARMACOS$FARMACO != "Ninguno",]
# al quitar "Ninguno" quitamos los casos pero no el nivel del factor, así que "refactorizamos", así se va
DF_FARMACOS$FARMACO <- factor(DF_FARMACOS$FARMACO)

DF_FARMACOS$ID <- seq.int(nrow(DF_FARMACOS))

save(DF_FARMACOS, file="DATOS/DF_FARMACOS.RData")



# load("DATOS/DF_FARMACOS.RData")
# source("R_funciones_comunes/explicit_missing.R")

#==================================================== 
#GESTION DE ERRORES
#==================================================== 
#------------------------------------------------------------------------- ERROR EN TIEMPO
DF_FARM_ERROR_TIEMPO <- DF_FARMACOS[DF_FARMACOS$TIEMPO <= 0 | DF_FARMACOS$TIEMPO >= 100,]
DF_FARM_ERROR_TIEMPO <- DF_FARM_ERROR_TIEMPO[!is.na(DF_FARM_ERROR_TIEMPO$Sujeto),]

#--- grabar archivo
file_error_tiempo <- "ERRORES/Errores_tiempo_MODIFICAR_EN_LA_ORIGINAL.xlsx"
if (file.exists(file_error_tiempo)) file.remove(file_error_tiempo)
if (nrow(DF_FARM_ERROR_TIEMPO) > 0) write.xlsx(DF_FARM_ERROR_TIEMPO,file_error_tiempo)


#------------------------------------------------------------------------- FARMACOS CON POCAS ADMINISTRACIONES
file_error_farmacos_menos_10 <- "ERRORES/Errores_famacos_menos_10_MODIFICAR_EN_LA_ORIGINAL.txt"
farmacos_con_10_o_menos <- levels(DF_FARMACOS$FARMACO)[table(DF_FARMACOS$FARMACO) <= FARMACOS_MINIMO]
if (length(farmacos_con_10_o_menos) > 0) write(farmacos_con_10_o_menos,file_error_farmacos_menos_10)

#==================================================== 
# FORMACION DE BASE FINAL
#==================================================== 
#----------------------------------------------------- SEXO
DF_FARMACOS$SEXO[DF_FARMACOS$SEXO == "ERROR"] <- NA
DF_FARMACOS$SEXO <- factor(DF_FARMACOS$SEXO)

# -- los reordenamos para que la mujer vaya antes y que en la regresión de COX SEXO sea un factor de riesgo y no de proteccion
DF_FARMACOS$SEXO <- factor(DF_FARMACOS$SEXO, levels(DF_FARMACOS$SEXO)[c(2,1)])


#----------------------------------------------------- DIAGNOSTICO
DF_FARMACOS$DIAGNOSTICO[DF_FARMACOS$DIAGNOSTICO == "ERROR"] <- NA
DF_FARMACOS$DIAGNOSTICO <- factor(DF_FARMACOS$DIAGNOSTICO)

#------------------------------------------------------ QUITAMOS LOS ERRORES
DF_FARMACOS <- DF_FARMACOS[DF_FARMACOS$FARMACO != "ERROR",]
DF_FARMACOS$FARMACO <- factor(DF_FARMACOS$FARMACO)


#------------------------------------------------------ Quitamos los que hemos identificado con problemas de tiempo
DF_FARMACOS_BRUTOS <- DF_FARMACOS
save(DF_FARMACOS_BRUTOS, file= "DATOS/DF_FARMACOS_BRUTOS.RDAta")

DF_FARMACOS <- DF_FARMACOS[!(DF_FARMACOS$ID %in% DF_FARM_ERROR_TIEMPO$ID),]


farmacos_con_mas_de_10 <- levels(DF_FARMACOS$FARMACO)[table(DF_FARMACOS$FARMACO) > FARMACOS_MINIMO]
DF_FARMACOS <- DF_FARMACOS[as.character(DF_FARMACOS$FARMACO) %in% farmacos_con_mas_de_10, ]
DF_FARMACOS$FARMACO <- factor(DF_FARMACOS$FARMACO)


DF_FARMACOS$YEAR <- as.numeric(substring(DF_FARMACOS$F_INI,1,4))
DF_FARMACOS$LUSTRO <- cut(DF_FARMACOS$YEAR,breaks = DIVISION_TIEMPO_LUSTRO )
DF_FARMACOS$DECADA <- cut(DF_FARMACOS$YEAR,breaks = DIVISION_TIEMPO_DECADA, dig.lab = 4 )


save(DF_FARMACOS, file= "DATOS/DF_FARMACOS_SUPERVIVENCIA.RDAta")


