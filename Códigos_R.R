
# Librerias útiles a cargar
library(readr) # Archivos excel, xls, xlsx
library(haven) # Archivos dta, sav, sas
library(readxl) # Archivos csv, csv2, tsv, delim
library(dplyr) # Manipulación de datos
library(ggplot2) # Gráficos
library(WDI) # Datos online del World Development Indicators
library(tidyquant) # Modela series financieras desde Yahoo Finance
library(stargazer) # Comparación de modelos en un tabla
library(mfx) # Efectos Marginales
library(tidyverse) 
library(stringr)
library(janitor) 
library(kableExtra)
library(lubridate)
library(zoo)
library(forcats)
library(ggthemes)
library(RColorBrewer)
library(ggfittext)
library(treemapify)
library(reshape2)
library(desc)
library(skimr)


# Importación de Datos
Base_Excel <- read_excel("Directorio/Base_Excel")
Base_Stata <- read_dta("Directorio/Base_Stata")
Base_Csv <- read.csv("Directorio/Base_Csv")

# Generando Data Frame
Nombre_Data_Frame <- data.frame(Var_1,Var_2, ...)

# Seleccionando Variables 
Sub_Data_Frame <- Data_Frame %>% select(Var_1, Var_2, ...)

# Filtrando Observaciones
Data_Frame_Filtrado <- Data_Frame %>% filter(condicion_1 | Condicion_2 & Condición_3 ... )

# Renombrando Variables
data_frame <- data_frame %>% rename(nombre_antiguo1 = nombre_nuevo1, 
                                       nombre_antiguo2 = nombre_nuevo2,
                                      ...)

# Generando Variables y modificando etiquetas
data_frame <- data_frame %>% 
  mutate(var_nueva = funcion(de_algo)) %>% 
  mutate(var_dummy_simple = ifelse(condicion_1 | Condicion_2 & Condición_3 ... , Valor_Verdadero, Valor_Falso)) %>% 
  mutate(var_dummy_compleja = ifelse(Condición_1... , Valor_Verdadero, ifelse(Condicion_1... , Valor_Verdadero, Valor_Falso))) %>%
  mutate(var_categórica= ifelse(condicion_1... , "Texto_Verdadero",
                         ifelse(condicion_1... , "Texto_Verdadero",
                         ifelse(condicion_1... , "Texto_Verdadero", "Texto_Falso")))) %>% 
  mutate(var_existente = recode(var_existente,
                         "Etiqueta_antigua_1" = "Etiqueta_nueva_1",
                         "Etiqueta_antigua_2" = "Etiqueta_nueva_2",
                         ...))

# Tabulación 
Data_Frame %>% 
  group_by(Var_categórica) %>% 
  summarize(Promedio = mean(Var_númerica),
            Mediana = median(Var_númerica),
            Mínimo=min(Var_númerica),
            Máximo=max(Var_númerica))

dcast(Data_Frame, Var_filas ~ Var_columnas, 
      estadística_descriptiva, 
      value.var="variable_de_la_estadística_descriptiva", 
      na.rm = TRUE)

# Gráficos
Data_Frame %>% ggplot(mapping=aes(x=Var_absicas, y=Var_ordenadas, color=Var_para_color)) +
  geom_() + # (point, line, density, col, bar, boxplot, histogram) 
  
  geom_smooth(data=Data_Frame[Data_Frame$Varcondicion_1 & Data_Frame$Varcondicion_2, ...], method='lm', color='algún_color', se=FALSE)+ # Regresión en el gráfico
  
  facet_wrap(~variable_categórica, ncol=N°) + # Gráfico separado por variable categórica
  
  xlim(N_min, N_máx) + # Valores eje absicas
  ylim(N_min, N_máx) + # Valores eje ordenadas
  scale_y_log10()+ # Escala logarítmica
  scale_x_date()+ # Escala con formato fechas
  scale_x_continuous(breaks = seq(Desde, Hasta, Delta)) + # Valores que queremos que aparezcan en el eje
  theme_()+ # Tema del gráfico (minimal, dark, economist, classic ...)
  scale_colour_manual(values=c("#1b9e77", "#d95f02", "#826bb5", "#e7298a")) + # Colores personalizados para Var_colores
  coord_flip() + # Invertir los ejes del gráfico
  
  labs(title = "Texto",
       subtitle = "Texto",
       x="Texto",
       y= NULL,
       caption = "Texto")+
  
  theme(plot.title = element_text(family = "serif", size = N°) "Elegimos el tipo de letra y tamaño"
        plot.subtitle = element_text(family = "Bookman", size = N°) "Elegimos el tipo de letra y tamaño"
        plot.caption = element_text(hjust = 1), # Ajuste de ubicación de las Notas
        axis.line = element_line(size = N°), # Elegimos grosor de ejes 
        legend.title = element_blank(), # Sin título de leyenda
        legend.position = c(0.9, 0.3)) # Posición de leyenda

# Regresiones 
mco <- lm(Var_dep ~ Var_ind_1 + Var_ind_2 + ..., data = Data_Frame) 
mco <- lm(Var_dep ~ 0 + Var_ind_1 + Var_ind_2 + ..., data = Data_Frame) # Modelo sin Constante
mco <- lm(Var_dep ~ 1, data = Data_Frame) # Modelo sólo con Constante

probit <- glm(Var_dep ~ Var_ind_1 + Var_ind_2 + ..., family = binomial(link = "probit"), data = Data_Frame)

probitmfx(Var_dep ~ Var_ind_1 + Var_ind_2 + ..., data = Data_Frame, atmean=FALSE) # Efecto Marginal Promedio
marginal_en_promedio <- probitmfx(Var_dep ~ Var_ind_1 + Var_ind_2 + ..., data = Data_Frame, atmean=TRUE)$mfxest # Efecto Marginal en el Promedio


# Comparación de modelos 
stargazer(mco, probit, coef = list(NULL, marginal_en_promedio[,1]), 
          se = list(NULL, marginal_en_promedio[,2]), 
          type = "text",
          title = "Comparación de modelos",
          dep.var.labels = "Etiqueta Var Dependiente",
          model.numbers = FALSE,
          column.labels = c("Etiqueta_1", "Etiqueta_2",...))

# Test de Chow
sctest(Data_frame$Var_dep ~ Data_frame$Var_ind, type = "Chow", point = Punto_de_quiebre)


# Códigos sueltos
arrange(Var_1, Var_2, ...) # Orden ascendente de observaciones en variables


