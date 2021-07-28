library(readr)
library(foreign)

viviendas<- read.csv(file.choose(), sep = ',', encoding = 'UTF-8')
hogares<- read.csv(file.choose(), sep = ',', encoding = 'UTF-8')
personas<- read.csv(file.choose(), sep = ',', encoding = 'UTF-8')
georreferenciacion <- read_csv("Sebastian_Temp/Personal/Maestria/Tsis/CNPV2018_MGN_A2_05.CSV", col_types = cols(COD_DANE_ANM = col_character()))
Vulnerabilidad <-read.csv(file.choose(), sep = ',', encoding = 'UTF-8')
manzana<-read.dbf(file.choose(),  as.is = FALSE)

library(sqldf)
library(stringr)

personas_M<-personas[personas$U_MPIO==1, ]
georreferenciacion_M<-georreferenciacion[georreferenciacion$U_MPIO=="001", ]
manzana1<-manzana[manzana$DPTO_CCDGO=="05",]
manzana1<-manzana1[manzana1$MPIO_CCDGO=="001",]
campos <- c("COD_DANE_A","LATITUD","LONGITUD")
manzana_M<-manzana1[ , (names(manzana1) %in% campos)]


datos_agrupados_1<-sqldf("SELECT * FROM personas_M a 
                       INNER JOIN hogares b ON (a.COD_ENCUESTAS=b.COD_ENCUESTAS)")
borrar <- c("TIPO_REG")
datos_agrupados_1 <- datos_agrupados_1[ , !(names(datos_agrupados_1) %in% borrar)]

datos_agrupados_2<-sqldf("SELECT * FROM datos_agrupados_1 a 
                       INNER JOIN viviendas b ON (a.COD_ENCUESTAS=b.COD_ENCUESTAS)")

borrar <- c("U_DPTO")
datos_agrupados_2 <- datos_agrupados_2[ , !(names(datos_agrupados_2) %in% borrar)]

georreferenciacion_M_1<-sqldf("SELECT a.COD_DANE_ANM, a.COD_ENCUESTAS FROM georreferenciacion_M a
                              GROUP BY 1,2")

datos_agrupados_3<-sqldf("SELECT * FROM datos_agrupados_2 a 
                       INNER JOIN georreferenciacion_M_1 b ON (a.COD_ENCUESTAS=b.COD_ENCUESTAS)")
borrar <- c("U_MPIO")
datos_agrupados_3 <- datos_agrupados_3[ , !(names(datos_agrupados_3) %in% borrar)]



datos_agrupados_4<-sqldf("SELECT * FROM datos_agrupados_3 a 
                       inner JOIN manzana_M b ON (a.COD_DANE_ANM=b.COD_DANE_A)")

#names(datos_agrupados_4)

seleccionados<-c("U_MPIO","U_EDIFICA","COD_ENCUESTAS","U_VIVIENDA","P_NROHOG","P_NRO_PER","P_SEXO","P_EDADR","P_PARENTESCOR","PA_LUG_NAC","PA_VIVIA_5ANOS","PA_VIVIA_1ANO","P_ENFERMO","P_QUEHIZO_PPAL","PA_LO_ATENDIERON","CONDICION_FISICA","P_ALFABETA","PA_ASISTENCIA","P_NIVEL_ANOSR","P_TRABAJO","P_EST_CIVIL","PA_HNV","PA1_THNV","PA_HNVS","PA1_CALIDAD_SERV","UA_CLASE","U_EDIFICA","U_VIVIENDA","UVA_USO_UNIDAD","V_TIPO_VIV","V_CON_OCUP","V_TOT_HOG","V_MAT_PARED","V_MAT_PISO","VA_EE","VA1_ESTRATO","VB_ACU","VC_ALC","VD_GAS","VE_RECBAS","VE1_QSEM","VF_INTERNET","V_TIPO_SERSA","L_TIPO_INST","L_EXISTEHOG","L_TOT_PERL","U_VIVIENDA","H_NROHOG","H_NRO_CUARTOS","H_NRO_DORMIT","H_DONDE_PREPALIM","H_AGUA_COCIN","HA_TOT_PER","UA_CLASE","UA1_LOCALIDAD","U_SECT_RUR","U_SECT_URB","U_MZA","U_EDIFICA","COD_DANE_ANM","LATITUD","LONGITUD")
datos_agrupados_5<-datos_agrupados_4[ , (names(datos_agrupados_4) %in% seleccionados)]


#espacio_publico<-read.dbf(file.choose(),  as.is = FALSE)   #revisar cruce espacial

#llamo violencia familiar
violencia_familiar <-read.csv(file.choose(), sep = ';', encoding = 'UTF-8')
violencia_familiar["año"]<-strtoi(substr(violencia_familiar$seguridad.fecha_hecho, 1, 4),base = 0L)
violencia_familiar<-violencia_familiar[violencia_familiar$año>=2017, ]
violencia_familiar <- violencia_familiar[!is.na(violencia_familiar$seguridad.latitud),]
violencia_familiar <- violencia_familiar[!is.na(violencia_familiar$seguridad.longitud),]
violencia_familiar <- violencia_familiar[violencia_familiar$seguridad.longitud<0,]

#llamollamad123

llamada123<-read.csv(file.choose(), sep = ';', encoding = 'UTF-8')
llamada123["año"]<-strtoi(substr(llamada123$seguridad.fecha_hecho, 1, 4),base = 0L)
llamada123<-llamada123[llamada123$año>=2017, ]
llamada123 <- llamada123[!is.na(llamada123$seguridad.latitud),]
llamada123 <- llamada123[!is.na(llamada123$seguridad.longitud),]
llamada123 <- llamada123[llamada123$seguridad.longitud<0,]

#llamoconductacontraria

conductacontraria<-read.csv(file.choose(), sep = ';', encoding = 'UTF-8')
conductacontraria["año"]<-strtoi(substr(conductacontraria$seguridad.fecha_hecho, 1, 4),base = 0L)
conductacontraria<-conductacontraria[conductacontraria$año>=2017, ]
conductacontraria <- conductacontraria[!is.na(conductacontraria$seguridad.latitud),]
conductacontraria <- conductacontraria[!is.na(conductacontraria$seguridad.longitud),]
conductacontraria <- conductacontraria[conductacontraria$seguridad.longitud<0,]

#extorsion

extorsion<-read.csv(file.choose(), sep = ';', encoding = 'UTF-8')
extorsion["año"]<-strtoi(substr(extorsion$seguridad.fecha_hecho, 1, 4),base = 0L)
extorsion<-extorsion[extorsion$año>=2017, ]
extorsion <- extorsion[!is.na(extorsion$seguridad.latitud),]
extorsion <- extorsion[!is.na(extorsion$seguridad.longitud),]
extorsion <- extorsion[extorsion$seguridad.longitud<0,]

#homicidio

homicidio<-read.csv(file.choose(), sep = ';', encoding = 'UTF-8')
homicidio["año"]<-strtoi(substr(homicidio$seguridad.fecha_hecho, 1, 4),base = 0L)
homicidio<-homicidio[homicidio$año>=2017, ]
homicidio <- homicidio[!is.na(homicidio$seguridad.latitud),]
homicidio <- homicidio[!is.na(homicidio$seguridad.longitud),]
homicidio <- homicidio[homicidio$seguridad.longitud<0,]

#lesiones_no_fatales

lesiones_no_fatales<-read.csv(file.choose(), sep = ';', encoding = 'UTF-8')
lesiones_no_fatales["año"]<-strtoi(substr(lesiones_no_fatales$seguridad.fecha_hecho, 1, 4),base = 0L)
lesiones_no_fatales<-lesiones_no_fatales[lesiones_no_fatales$año>=2017, ]
lesiones_no_fatales <- lesiones_no_fatales[!is.na(lesiones_no_fatales$seguridad.latitud),]
lesiones_no_fatales <- lesiones_no_fatales[!is.na(lesiones_no_fatales$seguridad.longitud),]
lesiones_no_fatales <- lesiones_no_fatales[lesiones_no_fatales$seguridad.longitud<0,]

#aprehension

aprehension<-read.csv(file.choose(), sep = ';', encoding = 'UTF-8')
aprehension["año"]<-strtoi(substr(aprehension$seguridad.fecha_hecho, 1, 4),base = 0L)
aprehension<-aprehension[aprehension$año>=2017, ]
aprehension <- aprehension[!is.na(aprehension$seguridad.latitud),]
aprehension <- aprehension[!is.na(aprehension$seguridad.longitud),]
aprehension <- aprehension[aprehension$seguridad.longitud<0,]

#secuestro

secuestro<-read.csv(file.choose(), sep = ';', encoding = 'UTF-8')
secuestro["año"]<-strtoi(substr(secuestro$seguridad.fecha_hecho, 1, 4),base = 0L)
secuestro<-secuestro[secuestro$año>=2017, ]
secuestro <- secuestro[!is.na(secuestro$seguridad.latitud),]
secuestro <- secuestro[!is.na(secuestro$seguridad.longitud),]
secuestro <- secuestro[secuestro$seguridad.longitud<0,]

#hurtopersonas

hurtopersonas<-read.csv(file.choose(), sep = ';', encoding = 'UTF-8')
hurtopersonas["año"]<-strtoi(substr(hurtopersonas$seguridad.fecha_hecho, 1, 4),base = 0L)
hurtopersonas<-hurtopersonas[hurtopersonas$año>=2017, ]
hurtopersonas <- hurtopersonas[!is.na(hurtopersonas$seguridad.latitud),]
hurtopersonas <- hurtopersonas[!is.na(hurtopersonas$seguridad.longitud),]
hurtopersonas <- hurtopersonas[hurtopersonas$seguridad.longitud<0,]

#hurtoresidencias

hurtoresidencias<-read.csv(file.choose(), sep = ';', encoding = 'UTF-8')
hurtoresidencias["año"]<-strtoi(substr(hurtoresidencias$seguridad.fecha_hecho, 1, 4),base = 0L)
hurtoresidencias<-hurtoresidencias[hurtoresidencias$año>=2017, ]
hurtoresidencias <- hurtoresidencias[!is.na(hurtoresidencias$seguridad.latitud),]
hurtoresidencias <- hurtoresidencias[!is.na(hurtoresidencias$seguridad.longitud),]
hurtoresidencias <- hurtoresidencias[hurtoresidencias$seguridad.longitud<0,]

#hurtocomercial

hurtocomercial<-read.csv(file.choose(), sep = ';', encoding = 'UTF-8')
hurtocomercial["año"]<-strtoi(substr(hurtocomercial$seguridad.fecha_hecho, 1, 4),base = 0L)
hurtocomercial<-hurtocomercial[hurtocomercial$año>=2017, ]
hurtocomercial <- hurtocomercial[!is.na(hurtocomercial$seguridad.latitud),]
hurtocomercial <- hurtocomercial[!is.na(hurtocomercial$seguridad.longitud),]
hurtocomercial <- hurtocomercial[hurtocomercial$seguridad.longitud<0,]

#table(violencia_familiar$seguridad.conducta)
library(foreach)
library(doParallel)

registerDoParallel(10)  # use multicore, set to the number of our cores
contador_vf<-data.frame()
contador123<-data.frame()
contadorconducta<-data.frame()
contadorext<-data.frame()
contadorhom<-data.frame()
contadorlesion<-data.frame()
contadorapreh<-data.frame()
contadorsecuestro<-data.frame()
contadorhurtp<-data.frame()
contadorhurtr<-data.frame()
contadorhurtc<-data.frame()



t<-proc.time()
t2<-0
for(i in 11040:nrow(manzana_M)) {
  x=c(manzana_M[i,2],manzana_M[i,3])
  a=0
  b=0
  c=0
  w=0
  e=0
  f=0
  g=0
  h=0
  m=0
  n=0
  p=0
  t1<-proc.time()
  for(j in 1:nrow(secuestro)){
    y=c(aprehension[j,3],aprehension[j,4])
    d<-dist(rbind(x,y))/0.000009009020
    if (d<100 && !is.na(d)) {
      g=g+1
    }
    y=c(secuestro[j,3],secuestro[j,4])
    d<-dist(rbind(x,y))/0.000009009020
    if (d<100 && !is.na(d)) {
      h=h+1
    }
  }
  for(j in 1:nrow(homicidio)){
    y=c(extorsion[j,3],extorsion[j,4])
    d<-dist(rbind(x,y))/0.000009009020
    if (d<100 && !is.na(d)) {
      w=w+1
    }
    y=c(homicidio[j,3],homicidio[j,4])
    d<-dist(rbind(x,y))/0.000009009020
    if (d<100 && !is.na(d)) {
      e=e+1
    }
  }
  for(j in 1:nrow(lesiones_no_fatales)){
    y=c(violencia_familiar[j,3],violencia_familiar[j,4])
    d<-dist(rbind(x,y))/0.000009009020
    if (d<100 && !is.na(d)) {
      a=a+1
    }
    y=c(hurtoresidencias[j,3],hurtoresidencias[j,4])
    d<-dist(rbind(x,y))/0.000009009020
    if (d<100 && !is.na(d)) {
      n=n+1
    } 
    y=c(hurtocomercial[j,3],hurtocomercial[j,4])
    d<-dist(rbind(x,y))/0.000009009020
    if (d<100 && !is.na(d)) {
      p=p+1
    }
  }
  # for(j in 1:nrow(llamada123)){
  #   y=c(llamada123[j,3],llamada123[j,4])
  #   d<-dist(rbind(x,y))/0.000009009020
  #   if (d<100 && !is.na(d)) {
  #     b=b+1
  #   }
  #   y=c(hurtopersonas[j,3],hurtopersonas[j,4])
  #   d<-dist(rbind(x,y))/0.000009009020
  #   if (d<100 && !is.na(d)) {
  #     m=m+1
  #   }
  # }
  for(j in 1:nrow(conductacontraria)){
    y=c(conductacontraria[j,3],conductacontraria[j,4])
    d<-dist(rbind(x,y))/0.000009009020
    if (d<100 && !is.na(d)) {
      c=c+1
    }
  }
  contador_vf[i,1]<-a
  contador123[i,1]<-b
  contadorconducta[i,1]<-c
  contadorext[i,1]<-w
  contadorhom[i,1]<-e
  contadorlesion[i,1]<-f
  contadorapreh[i,1]<-g
  contadorsecuestro[i,1]<-h
  contadorhurtp[i,1]<-m
  contadorhurtr[i,1]<-n
  contadorhurtc[i,1]<-p
  t2=t2+proc.time()-t1
  print(i)
  print(proc.time()-t1)
  print(t2)
  
}
proc.time()-t


campos1 <- c("Riña","Riña con arma blanca","Riña con arma de fuego")
campos2 <- c("Lesiones con arma blanca","Lesiones con arma contundente","Lesiones con arma de fuego")
llamada123_R<-llamada123[llamada123$seguridad.modalidad %in% campos1, ]
llamada123_L<-llamada123[llamada123$seguridad.modalidad %in% campos2, ]
llamada123_N<-llamada123[llamada123$seguridad.modalidad =="Narcoticos", ]
llamada123_D<-llamada123[llamada123$seguridad.modalidad =="Disparos", ]

#contador123_R<-data.frame() #es igual contador123
#contador123_L<-data.frame()
#contador123_N<-data.frame()
#contador123_D<-data.frame()

t<-proc.time()
t2<-0
for(i in 5210:nrow(manzana_M)) {
  x=c(manzana_M[i,2],manzana_M[i,3])
  b=0
  m=0
  t1<-proc.time()
  for(j in 1:nrow(llamada123_R)){
    y=c(llamada123_R[j,3],llamada123_R[j,4])
    d<-dist(rbind(x,y))/0.000009009020
    if (d<100 && !is.na(d)) {
      b=b+1
    }
    y=c(hurtopersonas[j,3],hurtopersonas[j,4])
    d<-dist(rbind(x,y))/0.000009009020
    if (d<100 && !is.na(d)) {
      m=m+1
    }
  }
  contador123[i,1]<-b
  contadorhurtp[i,1]<-m
  t2=t2+proc.time()-t1
  print(i)
  print(proc.time()-t1)
  print(t2)
  
}
proc.time()-t


manzana_M["Vio_Fam"]<-contador_vf
manzana_M["123_Pel"]<-contador123
manzana_M["Cond_Cont"]<-contadorconducta
manzana_M["Extor"]<-contadorext
manzana_M["Homic"]<-contadorhom
manzana_M["Lesion"]<-contadorlesion
manzana_M["Aprehens"]<-contadorapreh
manzana_M["Secues"]<-contadorsecuestro
manzana_M["Hurt_Per"]<-contadorhurtp
manzana_M["Hurt_Res"]<-contadorhurtr
manzana_M["Hurt_Com"]<-contadorhurtc

#Guardado temporal
save(datos_agrupados_3,manzana_M,file="primerosdatos_tesis.RData")

datos_agrupados_3<-datos_agrupados_3[ , (names(datos_agrupados_3) %in% seleccionados)]

#leer
load("primerosdatos_tesis.RData") 

datos_agrupados_4<-sqldf("SELECT * FROM datos_agrupados_3 a 
                       inner JOIN manzana_M b ON (a.COD_DANE_ANM=b.COD_DANE_A)")

#names(datos_agrupados_4)

seleccionados<-c("U_MPIO","U_EDIFICA","Vio_Fam","123_Pel","Cond_Cont","Extor","Homic","Lesion","Aprehens","Secues",
                 "Hurt_Per","Hurt_Res","Hurt_Com","COD_ENCUESTAS","U_VIVIENDA","P_NROHOG",
                 "P_NRO_PER","P_SEXO","P_EDADR","P_PARENTESCOR","PA_LUG_NAC","PA_VIVIA_5ANOS","PA_VIVIA_1ANO",
                 "P_ENFERMO","P_QUEHIZO_PPAL","PA_LO_ATENDIERON","CONDICION_FISICA","P_ALFABETA","PA_ASISTENCIA",
                 "P_NIVEL_ANOSR","P_TRABAJO","P_EST_CIVIL","PA_HNV","PA1_THNV","PA_HNVS","PA1_CALIDAD_SERV",
                 "UA_CLASE","U_EDIFICA","U_VIVIENDA","UVA_USO_UNIDAD","V_TIPO_VIV","V_CON_OCUP","V_TOT_HOG",
                 "V_MAT_PARED","V_MAT_PISO","VA_EE","VA1_ESTRATO","VB_ACU","VC_ALC","VD_GAS","VE_RECBAS",
                 "VE1_QSEM","VF_INTERNET","V_TIPO_SERSA","L_TIPO_INST","L_EXISTEHOG","L_TOT_PERL","U_VIVIENDA",
                 "H_NROHOG","H_NRO_CUARTOS","H_NRO_DORMIT","H_DONDE_PREPALIM","H_AGUA_COCIN","HA_TOT_PER","UA_CLASE",
                 "UA1_LOCALIDAD","U_SECT_RUR","U_SECT_URB","U_MZA","U_EDIFICA","COD_DANE_ANM","LATITUD","LONGITUD")
datos_agrupados_5<-datos_agrupados_4[ , (names(datos_agrupados_4) %in% seleccionados)]


datos_agrupados_5$PA_HNV[datos_agrupados_5$PA_HNV == 1] <- "Si"
datos_agrupados_5$PA_HNV[datos_agrupados_5$PA_HNV == 2] <- "No"
datos_agrupados_5$PA_HNV[datos_agrupados_5$PA_HNV == 9] <- "No informa/No aplica"

datos_agrupados_5$P_EST_CIVIL[datos_agrupados_5$P_EST_CIVIL == 1] <- "Union libre"
datos_agrupados_5$P_EST_CIVIL[datos_agrupados_5$P_EST_CIVIL == 2] <- "Casado"
datos_agrupados_5$P_EST_CIVIL[datos_agrupados_5$P_EST_CIVIL == 3] <- "Divorciado"
datos_agrupados_5$P_EST_CIVIL[datos_agrupados_5$P_EST_CIVIL == 4] <- "Separado"
datos_agrupados_5$P_EST_CIVIL[datos_agrupados_5$P_EST_CIVIL == 5] <- "Separado"
datos_agrupados_5$P_EST_CIVIL[datos_agrupados_5$P_EST_CIVIL == 6] <- "Viudo"
datos_agrupados_5$P_EST_CIVIL[datos_agrupados_5$P_EST_CIVIL == 7] <- "Soltero"
datos_agrupados_5$P_EST_CIVIL[datos_agrupados_5$P_EST_CIVIL == 9] <- "No informa/No aplica"

datos_agrupados_5$P_SEXO[datos_agrupados_5$P_SEXO == 1] <- "Masculino"
datos_agrupados_5$P_SEXO[datos_agrupados_5$P_SEXO == 2] <- "Femenino"

datos_agrupados_5$PA1_CALIDAD_SERV[datos_agrupados_5$PA1_CALIDAD_SERV == 1] <- "Muy bueno"
datos_agrupados_5$PA1_CALIDAD_SERV[datos_agrupados_5$PA1_CALIDAD_SERV == 2] <- "Bueno"
datos_agrupados_5$PA1_CALIDAD_SERV[datos_agrupados_5$PA1_CALIDAD_SERV == 3] <- "Malo"
datos_agrupados_5$PA1_CALIDAD_SERV[datos_agrupados_5$PA1_CALIDAD_SERV == 4] <- "Muy malo"
datos_agrupados_5$PA1_CALIDAD_SERV[datos_agrupados_5$PA1_CALIDAD_SERV == 9] <- "No informa/No aplica"

datos_agrupados_5$P_TRABAJO[datos_agrupados_5$P_TRABAJO == 1] <- "Trabajo por lo menos una hora en la semana con paga"
datos_agrupados_5$P_TRABAJO[datos_agrupados_5$P_TRABAJO == 2] <- "Trabajo por lo menos una hora en la semana sin paga"
datos_agrupados_5$P_TRABAJO[datos_agrupados_5$P_TRABAJO == 3] <- "No trabajó, pero tenia un medio de ingreso"
datos_agrupados_5$P_TRABAJO[datos_agrupados_5$P_TRABAJO == 5] <- "Vivio de jubilacion, o renta" 
datos_agrupados_5$P_TRABAJO[datos_agrupados_5$P_TRABAJO == 6] <- "Estudio" 
datos_agrupados_5$P_TRABAJO[datos_agrupados_5$P_TRABAJO == 7] <- "Oficios del hogar" 
datos_agrupados_5$P_TRABAJO[datos_agrupados_5$P_TRABAJO == 8] <- "Incapacidad permanente" 
datos_agrupados_5$P_TRABAJO[datos_agrupados_5$P_TRABAJO == 9] <- "Otra situacion" 
datos_agrupados_5$P_TRABAJO[datos_agrupados_5$P_TRABAJO == 4] <- "Busco trabajo" 
datos_agrupados_5$P_TRABAJO[datos_agrupados_5$P_TRABAJO == 0] <- "No informa/No aplica" 


datos_agrupados_5$P_EDADR[datos_agrupados_5$P_EDADR == 1] <- "00 a 04 años"
datos_agrupados_5$P_EDADR[datos_agrupados_5$P_EDADR == 2] <- "05 a 09 años"
datos_agrupados_5$P_EDADR[datos_agrupados_5$P_EDADR == 3] <- "10 a 14 años"
datos_agrupados_5$P_EDADR[datos_agrupados_5$P_EDADR == 4] <- "15 a 19 años"
datos_agrupados_5$P_EDADR[datos_agrupados_5$P_EDADR == 5] <- "20 a 24 años"
datos_agrupados_5$P_EDADR[datos_agrupados_5$P_EDADR == 6] <- "25 a 29 años"
datos_agrupados_5$P_EDADR[datos_agrupados_5$P_EDADR == 7] <- "30 a 34 años"
datos_agrupados_5$P_EDADR[datos_agrupados_5$P_EDADR == 8] <- "35 a 39 años"
datos_agrupados_5$P_EDADR[datos_agrupados_5$P_EDADR == 9] <- "40 a 44 años"
datos_agrupados_5$P_EDADR[datos_agrupados_5$P_EDADR == 10] <- "45 a 49 años"
datos_agrupados_5$P_EDADR[datos_agrupados_5$P_EDADR == 11] <- "50 a 54 años"
datos_agrupados_5$P_EDADR[datos_agrupados_5$P_EDADR == 12] <- "55 a 59 años"
datos_agrupados_5$P_EDADR[datos_agrupados_5$P_EDADR == 13] <- "60 a 64 años"
datos_agrupados_5$P_EDADR[datos_agrupados_5$P_EDADR == 14] <- "65 a 69 años"
datos_agrupados_5$P_EDADR[datos_agrupados_5$P_EDADR == 15] <- "70 a 74 años"
datos_agrupados_5$P_EDADR[datos_agrupados_5$P_EDADR == 16] <- "75 a 79 años"
datos_agrupados_5$P_EDADR[datos_agrupados_5$P_EDADR == 17] <- "80 a 84 años"
datos_agrupados_5$P_EDADR[datos_agrupados_5$P_EDADR == 18] <- "85 a 89 años"
datos_agrupados_5$P_EDADR[datos_agrupados_5$P_EDADR == 19] <- "90 a 94 años"
datos_agrupados_5$P_EDADR[datos_agrupados_5$P_EDADR == 20] <- "95 a 99 años"
datos_agrupados_5$P_EDADR[datos_agrupados_5$P_EDADR == 21] <- "99+"

datos_agrupados_5$P_NIVEL_ANOSR[datos_agrupados_5$P_NIVEL_ANOSR == 1] <- "Preescolar"
datos_agrupados_5$P_NIVEL_ANOSR[datos_agrupados_5$P_NIVEL_ANOSR == 2] <- "Básica primaria"
datos_agrupados_5$P_NIVEL_ANOSR[datos_agrupados_5$P_NIVEL_ANOSR == 3] <- "Básica secundaria"
datos_agrupados_5$P_NIVEL_ANOSR[datos_agrupados_5$P_NIVEL_ANOSR == 4] <- "Media academica o clasica"
datos_agrupados_5$P_NIVEL_ANOSR[datos_agrupados_5$P_NIVEL_ANOSR == 5] <- "Media tecnica"
datos_agrupados_5$P_NIVEL_ANOSR[datos_agrupados_5$P_NIVEL_ANOSR == 6] <- "Normalista"
datos_agrupados_5$P_NIVEL_ANOSR[datos_agrupados_5$P_NIVEL_ANOSR == 7] <- "Técnica profesional o Tecnologica"
datos_agrupados_5$P_NIVEL_ANOSR[datos_agrupados_5$P_NIVEL_ANOSR == 8] <- "Universitario"
datos_agrupados_5$P_NIVEL_ANOSR[datos_agrupados_5$P_NIVEL_ANOSR == 9] <- "Especialización, maestría, doctorado"
datos_agrupados_5$P_NIVEL_ANOSR[datos_agrupados_5$P_NIVEL_ANOSR == 10] <- "Ninguno"
datos_agrupados_5$P_NIVEL_ANOSR[datos_agrupados_5$P_NIVEL_ANOSR == 99] <- "No informa/No aplica"

datos_agrupados_5$UA_CLASE[datos_agrupados_5$UA_CLASE == 1] <- "Cabecera Municipal"
datos_agrupados_5$UA_CLASE[datos_agrupados_5$UA_CLASE == 2] <- "Centro Poblado"
datos_agrupados_5$UA_CLASE[datos_agrupados_5$UA_CLASE == 3] <- "Rural Disperso"
datos_agrupados_5$UA_CLASE[datos_agrupados_5$UA_CLASE == 4] <- "Resto Rural"

datos_agrupados_5$L_EXISTEHOG[datos_agrupados_5$L_EXISTEHOG == 1] <- "Si"
datos_agrupados_5$L_EXISTEHOG[datos_agrupados_5$L_EXISTEHOG == 2] <- "No"
datos_agrupados_5$L_EXISTEHOG[datos_agrupados_5$L_EXISTEHOG == 9] <- "No informa/No aplica"
 
datos_agrupados_5$V_TIPO_SERSA[datos_agrupados_5$V_TIPO_SERSA == 1] <- "Inodoro conectado al alcantarillado"
datos_agrupados_5$V_TIPO_SERSA[datos_agrupados_5$V_TIPO_SERSA == 2] <- "Inodoro conectado a pozo septico"
datos_agrupados_5$V_TIPO_SERSA[datos_agrupados_5$V_TIPO_SERSA == 3] <- "Inodoro sin conexion"
datos_agrupados_5$V_TIPO_SERSA[datos_agrupados_5$V_TIPO_SERSA == 4] <- "Letrina"
datos_agrupados_5$V_TIPO_SERSA[datos_agrupados_5$V_TIPO_SERSA == 5] <- "Inodoro con descarga directa a fuentes de agua (bajamar)"
datos_agrupados_5$V_TIPO_SERSA[datos_agrupados_5$V_TIPO_SERSA == 6] <- "No tiene servicio sanitario"

datos_agrupados_5$H_AGUA_COCIN[datos_agrupados_5$H_AGUA_COCIN == 1] <- "Acueducto público"
datos_agrupados_5$H_AGUA_COCIN[datos_agrupados_5$H_AGUA_COCIN == 2] <- "Acueducto veredal"
datos_agrupados_5$H_AGUA_COCIN[datos_agrupados_5$H_AGUA_COCIN == 3] <- "Red de distribución comunitaria"
datos_agrupados_5$H_AGUA_COCIN[datos_agrupados_5$H_AGUA_COCIN == 4] <- "Pozo con bomba"
datos_agrupados_5$H_AGUA_COCIN[datos_agrupados_5$H_AGUA_COCIN == 5] <- "Pozo sin bomba, aljibe, jaguey o barreno"
datos_agrupados_5$H_AGUA_COCIN[datos_agrupados_5$H_AGUA_COCIN == 6] <- "Agua lluvia"
datos_agrupados_5$H_AGUA_COCIN[datos_agrupados_5$H_AGUA_COCIN == 7] <- "RÍo, quebrada, manantial, nacimiento"
datos_agrupados_5$H_AGUA_COCIN[datos_agrupados_5$H_AGUA_COCIN == 8] <- "Pila pública"
datos_agrupados_5$H_AGUA_COCIN[datos_agrupados_5$H_AGUA_COCIN == 9] <- "Carrotanque"
datos_agrupados_5$H_AGUA_COCIN[datos_agrupados_5$H_AGUA_COCIN == 10] <- "Aguatero"
datos_agrupados_5$H_AGUA_COCIN[datos_agrupados_5$H_AGUA_COCIN == 11] <- "Agua embotellada o en bolsa"
datos_agrupados_5$H_AGUA_COCIN[datos_agrupados_5$H_AGUA_COCIN == 12] <- "No preparan alimentos"
datos_agrupados_5$H_AGUA_COCIN[datos_agrupados_5$H_AGUA_COCIN == 99] <- "No informa/No aplica"

datos_agrupados_5$H_DONDE_PREPALIM[datos_agrupados_5$H_DONDE_PREPALIM == 1] <- "En un cuarto usado solo para cocinar"
datos_agrupados_5$H_DONDE_PREPALIM[datos_agrupados_5$H_DONDE_PREPALIM == 2] <- "En un cuarto usado para dormir"
datos_agrupados_5$H_DONDE_PREPALIM[datos_agrupados_5$H_DONDE_PREPALIM == 3] <- "Salacomedor con lavaplatos"
datos_agrupados_5$H_DONDE_PREPALIM[datos_agrupados_5$H_DONDE_PREPALIM == 4] <- "Salacomedor sin lavaplatos"
datos_agrupados_5$H_DONDE_PREPALIM[datos_agrupados_5$H_DONDE_PREPALIM == 5] <- "En un patio, corredor, enramada o al aire libre"
datos_agrupados_5$H_DONDE_PREPALIM[datos_agrupados_5$H_DONDE_PREPALIM == 6] <- "No preparan alimentos en la vivienda"
datos_agrupados_5$H_DONDE_PREPALIM[datos_agrupados_5$H_DONDE_PREPALIM == 9] <- "No informa/No aplica"

datos_agrupados_5$VF_INTERNET[datos_agrupados_5$VF_INTERNET == 1] <- "Si"
datos_agrupados_5$VF_INTERNET[datos_agrupados_5$VF_INTERNET == 2] <- "No"

datos_agrupados_5$P_PARENTESCOR[datos_agrupados_5$P_PARENTESCOR == 1] <- "Jefe del hogar"
datos_agrupados_5$P_PARENTESCOR[datos_agrupados_5$P_PARENTESCOR == 2] <- "Pareja"
datos_agrupados_5$P_PARENTESCOR[datos_agrupados_5$P_PARENTESCOR == 3] <- "Hijo"
datos_agrupados_5$P_PARENTESCOR[datos_agrupados_5$P_PARENTESCOR == 4] <- "Otros parientes"
datos_agrupados_5$P_PARENTESCOR[datos_agrupados_5$P_PARENTESCOR == 5] <- "No parientes/No aplica"

datos_agrupados_5$VE1_QSEM[datos_agrupados_5$VE1_QSEM == 8] <- "Mayor periodicidad"
datos_agrupados_5$VE1_QSEM[datos_agrupados_5$VE1_QSEM == 9] <- "No sabe/No aplica"


datos_agrupados_5$VE_RECBAS[datos_agrupados_5$VE_RECBAS == 1] <- "Si"
datos_agrupados_5$VE_RECBAS[datos_agrupados_5$VE_RECBAS == 2] <- "No"

datos_agrupados_5$VD_GAS[datos_agrupados_5$VD_GAS == 1] <- "Si"
datos_agrupados_5$VD_GAS[datos_agrupados_5$VD_GAS == 2] <- "No"
datos_agrupados_5$VD_GAS[datos_agrupados_5$VD_GAS == 9] <- "No sabe/No informa"

datos_agrupados_5$VC_ALC[datos_agrupados_5$VC_ALC == 1] <- "Si"
datos_agrupados_5$VC_ALC[datos_agrupados_5$VC_ALC == 2] <- "No"

datos_agrupados_5$PA_ASISTENCIA[datos_agrupados_5$PA_ASISTENCIA == 1] <- "Si"
datos_agrupados_5$PA_ASISTENCIA[datos_agrupados_5$PA_ASISTENCIA == 2] <- "No"
datos_agrupados_5$PA_ASISTENCIA[datos_agrupados_5$PA_ASISTENCIA == 9] <- "No informa/No aplica"

datos_agrupados_5$VA1_ESTRATO[datos_agrupados_5$VA1_ESTRATO == 9] <- "No sabe el estrato"

datos_agrupados_5$VB_ACU[datos_agrupados_5$VB_ACU == 1] <- "Si"
datos_agrupados_5$VB_ACU[datos_agrupados_5$VB_ACU == 2] <- "No"

datos_agrupados_5$V_TIPO_VIV[datos_agrupados_5$V_TIPO_VIV == 1] <- "Casa"
datos_agrupados_5$V_TIPO_VIV[datos_agrupados_5$V_TIPO_VIV == 2] <- "Apartamento"
datos_agrupados_5$V_TIPO_VIV[datos_agrupados_5$V_TIPO_VIV == 3] <- "Tipo cuarto"
datos_agrupados_5$V_TIPO_VIV[datos_agrupados_5$V_TIPO_VIV == 4] <- "Vivienda tradicional Indigena"
datos_agrupados_5$V_TIPO_VIV[datos_agrupados_5$V_TIPO_VIV == 5] <- "Vivienda tradicional Etnica"
datos_agrupados_5$V_TIPO_VIV[datos_agrupados_5$V_TIPO_VIV == 6] <- "Otro"

datos_agrupados_5$V_CON_OCUP[datos_agrupados_5$V_CON_OCUP == 1] <- "Ocupada"
datos_agrupados_5$V_CON_OCUP[datos_agrupados_5$V_CON_OCUP == 2] <- "Ocupada"
datos_agrupados_5$V_CON_OCUP[datos_agrupados_5$V_CON_OCUP == 3] <- "Vivienda temporal (para vacaciones, trabajo etc.)"
datos_agrupados_5$V_CON_OCUP[datos_agrupados_5$V_CON_OCUP == 4] <- "Desocupada"

datos_agrupados_5$V_MAT_PISO[datos_agrupados_5$V_MAT_PISO == 1] <- "Marmol, parque, madera pulida y lacada"
datos_agrupados_5$V_MAT_PISO[datos_agrupados_5$V_MAT_PISO == 2] <- "Baldosa, vinilo, tableta, ladrillo, laminado"
datos_agrupados_5$V_MAT_PISO[datos_agrupados_5$V_MAT_PISO == 3] <- "Alfombra"
datos_agrupados_5$V_MAT_PISO[datos_agrupados_5$V_MAT_PISO == 4] <- "Cemento, gravilla"
datos_agrupados_5$V_MAT_PISO[datos_agrupados_5$V_MAT_PISO == 5] <- "Madera burda, tabla, tablon, otro vegetal"
datos_agrupados_5$V_MAT_PISO[datos_agrupados_5$V_MAT_PISO == 6] <- "Tierra, arena, barro"

datos_agrupados_5$VA_EE[datos_agrupados_5$VA_EE == 1] <- "Si"
datos_agrupados_5$VA_EE[datos_agrupados_5$VA_EE == 2] <- "No"

datos_agrupados_5$V_MAT_PARED[datos_agrupados_5$V_MAT_PARED == 1] <- "Bloque, ladrillo, piedra, madera pulida"
datos_agrupados_5$V_MAT_PARED[datos_agrupados_5$V_MAT_PARED == 2] <- "Concreto vaciado"
datos_agrupados_5$V_MAT_PARED[datos_agrupados_5$V_MAT_PARED == 3] <- "Material prefabricado"
datos_agrupados_5$V_MAT_PARED[datos_agrupados_5$V_MAT_PARED == 4] <- "Guadua"
datos_agrupados_5$V_MAT_PARED[datos_agrupados_5$V_MAT_PARED == 5] <- "Tapia pisada, bahareque, adobe"
datos_agrupados_5$V_MAT_PARED[datos_agrupados_5$V_MAT_PARED == 6] <- "Madera burda, tabla, tablon"
datos_agrupados_5$V_MAT_PARED[datos_agrupados_5$V_MAT_PARED == 7] <- "Caña, esterilla, otros vegetales"
datos_agrupados_5$V_MAT_PARED[datos_agrupados_5$V_MAT_PARED == 8] <- "Materiales de deshecho (Zinc, tela, carton, latas, plasticos, otros)"
datos_agrupados_5$V_MAT_PARED[datos_agrupados_5$V_MAT_PARED == 9] <- "No tiene paredes"

datos_agrupados_5$UVA_USO_UNIDAD[datos_agrupados_5$UVA_USO_UNIDAD == 1] <- "Vivienda"
datos_agrupados_5$UVA_USO_UNIDAD[datos_agrupados_5$UVA_USO_UNIDAD == 2] <- "Mixto"
datos_agrupados_5$UVA_USO_UNIDAD[datos_agrupados_5$UVA_USO_UNIDAD == 3] <- "Unidad NO Residencial"
datos_agrupados_5$UVA_USO_UNIDAD[datos_agrupados_5$UVA_USO_UNIDAD == 4] <- "LEA"

datos_agrupados_5$PA_LUG_NAC[datos_agrupados_5$PA_LUG_NAC == 1] <- "En este mpio"
datos_agrupados_5$PA_LUG_NAC[datos_agrupados_5$PA_LUG_NAC == 2] <- "En otro mpio Colombiano"
datos_agrupados_5$PA_LUG_NAC[datos_agrupados_5$PA_LUG_NAC == 3] <- "En otro país"
datos_agrupados_5$PA_LUG_NAC[datos_agrupados_5$PA_LUG_NAC == 9] <- "No informa/No aplica"

datos_agrupados_5$P_ALFABETA[datos_agrupados_5$P_ALFABETA == 1] <- "Si"
datos_agrupados_5$P_ALFABETA[datos_agrupados_5$P_ALFABETA == 2] <- "No"
datos_agrupados_5$P_ALFABETA[datos_agrupados_5$P_ALFABETA == 9] <- "No informa/No aplica"

datos_agrupados_5$PA_VIVIA_5ANOS[datos_agrupados_5$PA_VIVIA_5ANOS == 1] <- "No habia nacido"
datos_agrupados_5$PA_VIVIA_5ANOS[datos_agrupados_5$PA_VIVIA_5ANOS == 2] <- "En este mpio"
datos_agrupados_5$PA_VIVIA_5ANOS[datos_agrupados_5$PA_VIVIA_5ANOS == 3] <- "En otro mpio colombiano"
datos_agrupados_5$PA_VIVIA_5ANOS[datos_agrupados_5$PA_VIVIA_5ANOS == 4] <- "En otro país"
datos_agrupados_5$PA_VIVIA_5ANOS[datos_agrupados_5$PA_VIVIA_5ANOS == 9] <- "No informa/No aplica"

datos_agrupados_5$CONDICION_FISICA[datos_agrupados_5$CONDICION_FISICA == 1] <- "Si"
datos_agrupados_5$CONDICION_FISICA[datos_agrupados_5$CONDICION_FISICA == 2] <- "No"
datos_agrupados_5$CONDICION_FISICA[datos_agrupados_5$CONDICION_FISICA == 9] <- "No informa/No aplica"

datos_agrupados_5$PA_VIVIA_1ANO[datos_agrupados_5$PA_VIVIA_1ANO == 1] <- "No habia nacido"
datos_agrupados_5$PA_VIVIA_1ANO[datos_agrupados_5$PA_VIVIA_1ANO == 2] <- "En este mpio"
datos_agrupados_5$PA_VIVIA_1ANO[datos_agrupados_5$PA_VIVIA_1ANO == 3] <- "En otro mpio colombiano"
datos_agrupados_5$PA_VIVIA_1ANO[datos_agrupados_5$PA_VIVIA_1ANO == 4] <- "En otro país"
datos_agrupados_5$PA_VIVIA_1ANO[datos_agrupados_5$PA_VIVIA_1ANO == 9] <- "No informa/No aplica"

datos_agrupados_5$P_ENFERMO[datos_agrupados_5$P_ENFERMO == 1] <- "Si"
datos_agrupados_5$P_ENFERMO[datos_agrupados_5$P_ENFERMO == 2] <- "No"
datos_agrupados_5$P_ENFERMO[datos_agrupados_5$P_ENFERMO == 9] <- "No informa/No aplica"

datos_agrupados_5$P_QUEHIZO_PPAL[datos_agrupados_5$P_QUEHIZO_PPAL == 1] <- "Acudió a la entidad de seguridad social en salud de la cual es filiado(a)"
datos_agrupados_5$P_QUEHIZO_PPAL[datos_agrupados_5$P_QUEHIZO_PPAL == 2] <- "Acudió a un médico particular"
datos_agrupados_5$P_QUEHIZO_PPAL[datos_agrupados_5$P_QUEHIZO_PPAL == 3] <- "Acudió a un boticario, farmacéuta, droguista"
datos_agrupados_5$P_QUEHIZO_PPAL[datos_agrupados_5$P_QUEHIZO_PPAL == 4] <- "Asistió a terapias alternativas"
datos_agrupados_5$P_QUEHIZO_PPAL[datos_agrupados_5$P_QUEHIZO_PPAL == 5] <- "Acudió a una autoridad indígena espiritual"
datos_agrupados_5$P_QUEHIZO_PPAL[datos_agrupados_5$P_QUEHIZO_PPAL == 6] <- "Acudió a otro médico de un grupo étnico"
datos_agrupados_5$P_QUEHIZO_PPAL[datos_agrupados_5$P_QUEHIZO_PPAL == 7] <- "Usó remedios caseros"
datos_agrupados_5$P_QUEHIZO_PPAL[datos_agrupados_5$P_QUEHIZO_PPAL == 8] <- "Se autorrecetó"
datos_agrupados_5$P_QUEHIZO_PPAL[datos_agrupados_5$P_QUEHIZO_PPAL == 9] <- "No hizo nada"
datos_agrupados_5$P_QUEHIZO_PPAL[datos_agrupados_5$P_QUEHIZO_PPAL == 99] <- "No informa/No aplica"

datos_agrupados_5$PA_LO_ATENDIERON[datos_agrupados_5$PA_LO_ATENDIERON == 1] <- "Si"
datos_agrupados_5$PA_LO_ATENDIERON[datos_agrupados_5$PA_LO_ATENDIERON == 2] <- "No"
datos_agrupados_5$PA_LO_ATENDIERON[datos_agrupados_5$PA_LO_ATENDIERON == 9] <- "No informa/No aplica"


Autonumerico<-seq(1,nrow(datos_agrupados_5))
datos_agrupados_5<-cbind(datos_agrupados_5,Autonumerico)
sumatorio<-rep(1,nrow(datos_agrupados_5))
datos_agrupados_5<-cbind(datos_agrupados_5,sumatorio)

save(datos_agrupados_5,file="Datosfinales.RData")

load("Datosfinales.RData") 


library(sf)
library(rgeos)
barrios<-st_read("Sebastian_Temp/Personal/Maestria/Tsis/Barrio_Vereda.shp")
nc <- st_read("Sebastian_Temp/Personal/Maestria/Tsis/Riesgos_naturales.shp")
manzanacapa<-st_read(file.choose())
manzanacapa2<-manzanacapa[manzanacapa$DPTO_CCDGO=="05",]
manzanacapa3<-manzanacapa2[manzanacapa2$MPIO_CCDGO=="001",]
campos <- c("COD_DANE_A","Shape_Leng","Shape_Area","geometry")
manzanacapaf<-manzanacapa3[, (names(manzanacapa3) %in% campos)]
vulnerabilidad<-st_read(file.choose())
campos <- c("COD_DANE","geometry","LABEL","ipm")
vulnerabilidad<-vulnerabilidad[, (names(vulnerabilidad) %in% campos)]
nc_inund <- nc[nc$TIPO_AMENA=="Inundaciones",] 
pbuf = st_buffer(st_geometry(nc_inund), (0.000009009020*2))

# make spatial falta cambiar la logica del cruce de datos

isd_history2 <- as.data.frame(datos_agrupados_5) %>% 
  st_as_sf(coords=c("LONGITUD","LATITUD"), crs=4326, remove=FALSE)

names (manzanacapaf)[1] = "COD_DANE_ANM"
isd_ca_co_pts_pre <- st_join(manzanacapaf, left = FALSE, st_as_sf(pbuf)) # joinpoligonos
isd_ca_co_pts<-st_join(isd_history2, left = FALSE, st_as_sf(isd_ca_co_pts_pre))
isd_ca_co_pts <- st_join(isd_ca_co_pts, left = FALSE, st_as_sf(barrios)) # join points
isd_ca_co_pts<-isd_ca_co_pts[!duplicated(isd_ca_co_pts$Autonumerico), ]
riesgo<-rep("Inundacion",nrow(isd_ca_co_pts))
isd_ca_co_pts<-cbind(isd_ca_co_pts,riesgo)
st_geometry(vulnerabilidad) <- NULL
st_geometry(isd_ca_co_pts) <- NULL



library(sqldf)
datos_agrupados_10<-sqldf("SELECT * FROM isd_ca_co_pts a 
                       inner JOIN vulnerabilidad b ON (a.'COD_DANE_ANM.y'=b.COD_DANE)")

isd_ca_co_pts<-datos_agrupados_10
# plot
#plot(st_geometry(pbuf))
#plot(st_geometry(nc_inund_2),add=TRUE,col="red")
#plot(st_geometry(isd_ca_co_pts),add=TRUE,col="purple")


write.table(isd_ca_co_pts, file = "datos_inundacion.csv",
            sep = ";", row.names = F)

#Deslizamiento

nc_desl <- nc[nc$TIPO_AMENA=="Movimientos en masa",] 
pbuf_d = st_buffer(st_geometry(nc_desl), (0.000009009020*2))
plot(st_geometry(pbuf_d))

isd_ca_co_ptsd_pre <- st_join(manzanacapaf, left = FALSE, st_as_sf(pbuf_d)) # joinpoligonos
isd_ca_co_pts_d<-st_join(isd_history2, left = FALSE, st_as_sf(isd_ca_co_ptsd_pre))
isd_ca_co_pts_d <- st_join(isd_ca_co_pts_d, left = FALSE, st_as_sf(barrios)) # join points
isd_ca_co_pts_d<-isd_ca_co_pts_d[!duplicated(isd_ca_co_pts_d$Autonumerico), ]
riesgo<-rep("Movimientos en masa",nrow(isd_ca_co_pts_d))
isd_ca_co_pts_d<-cbind(isd_ca_co_pts_d,riesgo)


st_geometry(isd_ca_co_pts_d) <- NULL



library(sqldf)
datos_agrupados_10<-sqldf("SELECT * FROM isd_ca_co_pts_d a 
                       inner JOIN vulnerabilidad b ON (a.'COD_DANE_ANM.y'=b.COD_DANE)")

isd_ca_co_pts_d<-datos_agrupados_10


write.table(isd_ca_co_pts_d, file = "datos_deslizamiento.csv",
            sep = ";", row.names = F)


#Avenida torrencial
nc_AvTo <- nc[nc$TIPO_AMENA=="Avenidas Torrenciales",] 
pbuf_AvTo = st_buffer(st_geometry(nc_AvTo), (0.000009009020*2))
plot(st_geometry(pbuf_AvTo))

isd_ca_co_ptsAV_pre <- st_join(manzanacapaf, left = FALSE, st_as_sf(pbuf_AvTo)) # joinpoligonos
isd_ca_co_pts_AvTo<-st_join(isd_history2, left = FALSE, st_as_sf(isd_ca_co_ptsAV_pre))  
isd_ca_co_pts_AvTo <- st_join(isd_ca_co_pts_AvTo, left = FALSE, st_as_sf(barrios)) # join points
isd_ca_co_pts_AvTo<-isd_ca_co_pts_AvTo[!duplicated(isd_ca_co_pts_AvTo$Autonumerico), ]
riesgo<-rep("Avenida Torrencial",nrow(isd_ca_co_pts_AvTo))
isd_ca_co_pts_AvTo<-cbind(isd_ca_co_pts_AvTo,riesgo)


st_geometry(isd_ca_co_pts_AvTo) <- NULL



library(sqldf)
datos_agrupados_10<-sqldf("SELECT * FROM isd_ca_co_pts_AvTo a 
                       inner JOIN vulnerabilidad b ON (a.'COD_DANE_ANM.y'=b.COD_DANE)")

isd_ca_co_pts_AvTo<-datos_agrupados_10


write.table(isd_ca_co_pts_AvTo, file = "datosAveTorrencial.csv",
            sep = ";", row.names = F)



#######################################################
##Sitios d einteres cercano
pbuf_1 = st_buffer(st_geometry(nc_inund), (0.000009009020*100))
Espacio_Publico <- st_read("Sebastian_Temp/Personal/Maestria/Tsis/Espacio_Publico_Existente.shp")

Puntosdeinteres <- st_join(st_as_sf(pbuf_1), left = FALSE, st_as_sf(Espacio_Publico)) # join  

write.table(Puntosdeinteres, file = "datosespacioinund.csv",
            sep = ";", row.names = F)


#####################################################
#Consolidación calidad de vida
calidadvida <- read.csv(file.choose(), sep = ';', encoding = 'UTF-8')
resumen<-table(calidadvida$encuesta_calidad.barrio)
write.table(resumen, file = "resumenparahomo.csv",
            sep = ";", row.names = F)
filtro<-barrios$CODIGO
filtro<-cbind(filtro,barrios$NOMBRE)
write.table(filtro, file = "resumensegundoparahomo.csv",
            sep = ";", row.names = F)

resumen2<-read.csv(file.choose(), sep = ';', encoding = '65000')
names(resumen2)[1] <- "encuesta_calidad.barrio"
calidadvida2<-merge(x = calidadvida, y = resumen2, by = "encuesta_calidad.barrio", all.x = TRUE)
calidadvida2<-calidadvida2[calidadvida2$encuesta_calidad.año==2018, ]
write.table(calidadvida2, file = "DatosEncCalidaddevida.csv",
            sep = ";", row.names = F)



######################################################
#KNN
#Seleccion aleatoria de la manzana

manzanaref<-sample(isd_ca_co_pts_pre$COD_DANE_ANM,1)

#Calculo de distancias
library(readr)
library(foreign)
manzana<-read.dbf(file.choose(),  as.is = FALSE)
manzana1<-manzana[manzana$DPTO_CCDGO=="05",]
manzana1<-manzana1[manzana1$MPIO_CCDGO=="001",]
campos <- c("COD_DANE_A","LATITUD","LONGITUD")
manzana_M<-manzana1[ , (names(manzana1) %in% campos)]
manzana_M_1<-manzana_M[manzana_M$COD_DANE_A==manzanaref,]
x=c(manzana_M_1[1,2],manzana_M_1[1,3])
distanciamanzanas<-data.frame()
for(j in 1:nrow(manzana_M)){
  y=c(manzana_M[j,2],manzana_M[j,3])
  d<-dist(rbind(x,y))/0.000009009020
  distanciamanzanas[j,1]<-manzana_M[j,1]
  distanciamanzanas[j,2]<-d
}


#Vecinos mas cercanos
distanciamanzanas2<-distanciamanzanas[order(distanciamanzanas$V2),]

manzanasprueba<-distanciamanzanas2[1:6,]

#Validacion


validacion<-datos_agrupados_5[(datos_agrupados_5$COD_DANE_ANM %in% manzanasprueba$V1) , ]
validacion<-sqldf("SELECT * FROM validacion c 
                       inner JOIN vulnerabilidad b ON (c.COD_DANE_ANM=b.COD_DANE)
                  ")

save(validacion,file="Datosvalidacion.RData")
load("Datosvalidacion.RData")

validacion_espejo<-validacion
validacion_espejo[,"Hurt_Per"]<-toString(validacion_espejo[,"Hurt_Per"])

for (i in 1:nrow(validacion)){
  ifelse(validacion[i,"Hurt_Per"]==0,validacion_espejo[i,"Hurt_Per"]<-"0 Eventos",
         ifelse(validacion[i,"Hurt_Per"]<=10,validacion_espejo[i,"Hurt_Per"]<-"Entre 1 y 10",
                ifelse(validacion[i,"Hurt_Per"]<=20,validacion_espejo[i,"Hurt_Per"]<-"Entre 11 y 20",
                       ifelse(validacion[i,"Hurt_Per"]<=40,validacion_espejo[i,"Hurt_Per"]<-"Entre 21 y 40",validacion_espejo[i,"Hurt_Per"]<-"Mas de 40")
                )
         )
  )
  
}

validacion_espejo[,"Vio_Fam"]<-toString(validacion_espejo[,"Vio_Fam"])

for (i in 1:nrow(validacion)){
  ifelse(validacion[i,"Vio_Fam"]==0,validacion_espejo[i,"Vio_Fam"]<-"0 Eventos",
         ifelse(validacion[i,"Vio_Fam"]<=10,validacion_espejo[i,"Vio_Fam"]<-"Entre 1 y 10",
                ifelse(validacion[i,"Vio_Fam"]<=20,validacion_espejo[i,"Vio_Fam"]<-"Entre 11 y 20",
                       ifelse(validacion[i,"Vio_Fam"]<=40,validacion_espejo[i,"Vio_Fam"]<-"Entre 21 y 40",validacion_espejo[i,"Hurt_Per"]<-"Mas de 40")
                )
         )
  )
  
}



case<-validacion[validacion$COD_DANE=="0500110000000009090506", ]#manzanaref
case1<-validacion_espejo[validacion$COD_DANE=="0500110000000009090506", ]#manzanaref

#Nula: El rango de edad no esta asociado a la manzana
#Altenativa: El rango de edad esta asociado a la manzana

comparacionetareo<-tapply(case$sumatorio,case$P_EDADR,sum)

total<-tapply(validacion$sumatorio,validacion$P_EDADR,sum)

muestra<-rbind(comparacionetareo,total)

chisq.test(muestra)

#Nula: El genero no esta asociado a la manzana
#Altenativa: El genero esta asociado a la manzana

comparacionetareo<-tapply(case$sumatorio,case$P_SEXO,sum)

total<-tapply(validacion$sumatorio,validacion$P_SEXO,sum)

muestra<-rbind(comparacionetareo,total)

chisq.test(muestra)

#Nula: El estrato no esta asociado a la manzana
#Altenativa: El estrato esta asociado a la manzana

comparacionetareo<-tapply(case$sumatorio,case$VA1_ESTRATO,sum)
nombres<-names(comparacionetareo)
total<-tapply(validacion$sumatorio,validacion$VA1_ESTRATO,sum)
total<-total[(names(total)%in%nombres)]
muestra<-rbind(comparacionetareo,total)

chisq.test(muestra)

#Nula: El Fuente ingreso no esta asociado a la manzana
#Altenativa: El Fuente de ingreso esta asociado a la manzana

comparacionetareo<-tapply(case$sumatorio,case$P_TRABAJO,sum)
nombres<-names(comparacionetareo)
total<-tapply(validacion$sumatorio,validacion$P_TRABAJO,sum)
total<-total[(names(total)%in%nombres)]
muestra<-rbind(comparacionetareo,total)
chisq.test(muestra)

#Nula: El Alimentos no esta asociado a la manzana
#Altenativa: El Alimentos esta asociado a la manzana

comparacionetareo<-tapply(case$sumatorio,case$H_AGUA_COCIN,sum)
nombres<-names(comparacionetareo)
total<-tapply(validacion$sumatorio,validacion$H_AGUA_COCIN,sum)
total<-total[(names(total)%in%nombres)]
muestra<-rbind(comparacionetareo,total)
chisq.test(muestra)

#Nula: Criminalidad del entorno no esta asociado a la manzana
#Altenativa: Criminalidad del entorno asociado a la manzana

comparacionetareo<-tapply(case1$sumatorio,case1$Hurt_Per,sum)
nombres<-names(comparacionetareo)
total<-tapply(validacion_espejo$sumatorio,validacion_espejo$Hurt_Per,sum)
total<-total[(names(total)%in%nombres)]
muestra<-rbind(comparacionetareo,total)
muestra<-cbind(muestra,c(25,186))
chisq.test(muestra)

#Nula: ViolenciaFam del entorno no esta asociado a la manzana
#Altenativa: ViolenciaFam del entorno asociado a la manzana

comparacionetareo<-tapply(case1$sumatorio,case1$Vio_Fam,sum)
nombres<-names(comparacionetareo)
total<-tapply(validacion_espejo$sumatorio,validacion_espejo$Vio_Fam,sum)
#<-total[(names(total)%in%nombres)]
muestra<-rbind(comparacionetareo,total)
muestra[1,2]<-0
chisq.test(muestra)
