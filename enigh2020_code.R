### Analisis de la ENIGH 2020
## Naim Manriquez

## Librerias

# Instalamos

install.packages("sjmisc")

install.packages("sjlabelled")

install.packages("tidyverse")

install.packages("haven")

install.packages("survey")

install.packages("dplyr")

install.packages("ggplot2")

## Cargamos librerias 

library(pacman)
p_load(sjmisc, sjlabelled, tidyverse, haven, survey, dplyr, ggplot2)

## Carga de base de datos
## concentradohogar <- read_dta("concentradohogar.dta") # Por si lo cargan desde Stata

concentradohogar <- readRDS("concentradohogar.rds")

## CONOCIENDO LA ESTRUCTURA DE LOS DATOS ##

# Estructura de la base de datos y variables
str(concentradohogar)
glimpse(concentradohogar)

# Para una sola variable
str(concentradohogar$sexo_jefe)

# Nombre de las variables
names(concentradohogar)

# Tabla de frecuencias
concentradohogar %>% frq(sexo_jefe)
concentradohogar %>% frq(sexo_jefe, weights= factor)

# Jefes de hogares en Mazatlan
concentradohogar %>% filter(ubica_geo == 25012) %>% frq(sexo_jefe)

# Con ponderador (factor de expansion)
concentradohogar %>% filter(ubica_geo == 25012) %>% frq(sexo_jefe, weights= factor)

# deciles de ingreso
quantile(concentradohogar$ing_cor, prob=seq(0, 1, length = 11))

# deciles de gasto
quantile(concentradohogar$gasto_mon, prob=seq(0, 1, length = 11))


# Renombrando variables

# Clase de hogar

concentradohogar$clase_hog<- factor(concentradohogar$clase_hog,label= c("Unipersonal", "Nuclear", "Extenso", "Compuesto","Corresidentes"))

table(concentradohogar$clase_hog)

## Jefatura de hogar

concentradohogar$sexo_jefe<- factor(concentradohogar$sexo_jefe,label= c("Hombre", "Mujer"))

table(concentradohogar$sexo_jefe)

# Tabla de frecuencias
concentradohogar %>% frq(sexo_jefe)
concentradohogar %>% frq(sexo_jefe, weights= factor)

# ¿Como se distribuye el ingreso?

qplot(log(ing_cor), data=concentradohogar, geom="density", alpha=I(.5), 
      main="Distribución de los ingresos corrientes", xlab="Logaritmo", 
      ylab="Density")

## Y entre jefes de hogares

qplot(log(ing_cor), data=concentradohogar, geom="density", fill=factor(sexo_jefe), alpha=I(.5), 
      main="Distribución de los ingresos corrientes", xlab="Logaritmo", 
      ylab="Density")

qplot(log(ing_cor), data=concentradohogar, geom="histogram", fill=factor(sexo_jefe), alpha=I(.5), 
      main="Distribución de los ingresos corrientes", xlab="Logaritmo", 
      ylab="Frecuencia")

## Algunos descriptivos

mydata<- concentradohogar[, c("ing_cor", "tot_integ","ocupados", "sexo_jefe", "clase_hog")]

tail(mydata)

summary(mydata)

summary(mydata[mydata$sexo_jefe=="Hombre",])

summary(mydata[mydata$sexo_jefe=="Mujer",])

tapply(concentradohogar$ing_cor, concentradohogar$sexo_jefe, summary)

## tapply(mydata$ing_cor, mydata$sexo_jefe, summary)

## Funcion para aplicar de una el factor de expansion

expandir<- function(x){
  weighted.mean(x, w = concentradohogar$factor)
}

## Y ahora solo necesitamos agregar expandir sin necesidad de llamar al factor

expandir(concentradohogar$ing_cor)

expandir(concentradohogar$percep_tot) 

expandir(concentradohogar$gasto_mon)


### SINALOA

## Generando los datos de Sinaloa
datos_sinaloa <- filter(concentradohogar, ubica_geo >= "25001" & ubica_geo <= "25018")


# Promedio de ingresos en Sinaloa
summary(datos_sinaloa$ing_cor)

## Generar tabla con ingresos para los municipios de Sinaloa

aggregate(x = datos_sinaloa$ing_cor,                # Especificar la variable de interes
          by = list(datos_sinaloa$ubica_geo),       # Especificar el grupo
          FUN = mean)                               # Especificar que queremos la media

promedio_ingresos_sinaloa <- aggregate(x = datos_sinaloa$ing_cor,
                                       by = list(datos_sinaloa$ubica_geo),
                                       FUN = mean)


### Ejemplo de gasto en refacciones

aggregate(x = datos_sinaloa$refaccion,                # Especificar la variable de interes
          by = list(datos_sinaloa$ubica_geo),       # Especificar el grupo
          FUN = mean)                               # Especificar que queremos la media


## Nueva base con las entidades ##

enigh2020<-concentradohogar%>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(cve_ent=
           case_when(nchar(folioviv)==9 ~ substr(folioviv,1,1),
                     TRUE ~substr(folioviv,1,2)),
         #Creamos nombre de entidad
         nom_ent= case_when(cve_ent=="01" ~ "Aguascalientes",
                            cve_ent=="02" ~ "Baja California",
                            cve_ent=="03" ~ "Baja California Sur",
                            cve_ent=="04" ~ "Campeche",
                            cve_ent=="05" ~ "Coahuila de Zaragoza",
                            cve_ent=="06" ~ "Colima",
                            cve_ent=="07" ~ "Chiapas",
                            cve_ent=="08" ~ "Chihuahua",
                            cve_ent=="09" ~ "Ciudad de Mexico",
                            cve_ent=="10" ~ "Durango",
                            cve_ent=="11" ~ "Guanajuato",
                            cve_ent=="12" ~ "Guerrero",
                            cve_ent=="13" ~ "Hidalgo",
                            cve_ent=="14" ~ "Jalisco",
                            cve_ent=="15" ~ "Mexico",
                            cve_ent=="16" ~ "Michoacan de Ocampo",
                            cve_ent=="17" ~ "Morelos",
                            cve_ent=="18" ~ "Nayarit",
                            cve_ent=="19" ~ "Nuevo Leon",
                            cve_ent=="20" ~ "Oaxaca",
                            cve_ent=="21" ~ "Puebla",
                            cve_ent=="22" ~ "Queretaro",
                            cve_ent=="23" ~ "Quintana Roo",
                            cve_ent=="24" ~ "San Luis Potosí",
                            cve_ent=="25" ~ "Sinaloa",
                            cve_ent=="26" ~ "Sonora",
                            cve_ent=="27" ~ "Tabasco",
                            cve_ent=="28" ~ "Tamaulipas",
                            cve_ent=="29" ~ "Tlaxcala",
                            cve_ent=="30" ~ "Veracruz de Ignacio de la Llave",
                            cve_ent=="31" ~ "Yucatan",
                            cve_ent=="32" ~ "Zacatecas"))

# Nombre de las variables
names(enigh2020)

enigh2020 %>% frq(nom_ent)

## ¿cuantos hogares hay en la muestra?
nrow(enigh2020)

## el factor de expansion nos ayuda a ampliar la muestra
sum(enigh2020$factor)


# Calculo del ingreso promedio por entidad

aggregate(x = enigh2020$ing_cor,                # Especificar la variable de interes
          by = list(enigh2020$nom_ent),       # Especificar el grupo
          FUN = mean)                               # Especificar que queremos la media

aggregate(x = enigh2020$ing_cor,                # Especificar la variable de interes
          by = list(enigh2020$est_socio),       # Especificar el grupo
          FUN = mean)                               # Especificar que queremos la media


