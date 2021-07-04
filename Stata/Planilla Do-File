                              ***FENVID STATA***

********************************************************************
clear all
import excel "C:\Users\agust\Desktop\Universidad de Chile\Videos FENVID\Stata\Datos\Sub Casen.xlsx", sheet("Hoja1") cellrange(A1:Y20001) firstrow

*-Importación de datos 
{
pwd 
cd "dirección base de datos/gráficos/tablas/etc"

global palabra "Texto"
cd "$palabra"

use "base_formato_stata", clear

import excel "directorio\nombre_base", sheet("Nombre_Hoja") cellrange(Coordenadas) firstrow
}

*-Estudiando y Modificando la Base
{
describe
describe var_1 var_2 ...
count
count if condición_1 condición_2 ...

browse var_1 var_2 ...
rename nombre_original nombre_nuevo

drop var_1 var_2 ...
drop if condición_1 condición_2 ...

keep var_1 var_2 ...
keep if condición_1 condición_2 ...

order var_1 var_2...

sort var_1 var_2...

}

*-Generación de variables y etiquetas
{
label variable nombre_variable "Descripción"

generate nueva_variable = función()

label define nombre_etiqueta 1 "categoría_1" 2 "categoría_2"...
label values nombre_variable nombre_etiqueta
label list nombre_etiqueta

generate nueva_variable=real(variable)

encode nombre_variable, generate(nueva_variable)

egen nueva_variable = función_ya_programada(nombre_variable)


}

*-Estadísticas Descriptivas
{
summarize var_1 var_2... 
summarize var_1 var_2..., detail
misstable sum var_1 var_2...

tab variable
tab variable, miss
tab variable [factor_de_expansión]
tab variable [factor_de_expansión] if condición_1 condición_2

tab var_1 var_2
tab var_1 var_2 [factor_de_expansión]
tab var_1 var_2 [factor_de_expansión], col nofreq
tab var_1 var_2 [factor_de_expansión], row nofreq
tab var_1 var_2 [factor_de_expansión], cell nofreq

tabstat var_1 var_2... if condición_1 condición_2... [factor_de_expansión], stats(mean p50 sd min max n ...)

latabstat var_1 var_2... if condición_1 condición_2... [factor_de_expansión], stats(mean p50 sd min max n ...)

by var_categórica : tabstat var_1 var_2..., stats()


}

*-Gráficos
{
graph pie [factor_de_expansión] if condición_1 condición_2, over(var_categorica) plabel(_all percent) by(, title(texto) subtitle(texto)) by(var_grupos)

graph box var_1 var_2... [factor_de_expansión] if condición_1 condición_2, over(var_categorica) ytitle(texto) by(, title(texto) subtitle(texto) note(texto)) by(var_grupos)

graph bar (Estadística_1) var_1 (Estadística_2) var_2...[factor_de_expansión], over(pareja) by(, title(texto) subtitle(texto) note(texto)) by(var_grupos)

histogram nombre_variable if condición_1 condición_2 [factor_de_expansión], opción_distribuciones ytitle(texto) xtitle(texto) by(, title(texto)) subtitle(texto) caption(texto)) by(var_grupos)

kdensity variable if condición_1 condición_2.. [factor_de_expansión], opción_dist_normal addplot((kdensity variable if if condición_1 condición_2.. [factor_de_expansión])) ytitle(texto) xtitle(texto) title(texto) subtitle(texto) caption(texto) legend(opciónes)

quantile nombre_variable if condición_1 condición_2..., recast(forma_datos) ytitle(texto) xtitle(texto) title(texto) rplots(connect(opción_de_conexión))

twoway (scatter var_1 var_2) (lfit var_1 var_2) if condición_1 condición_2..., ytitle(texto) xtitle(texto) by(, title(texto) subtitle(texto) caption(texto)) by(var_categorica)
}

*-Cambio en estructura de la base
{
global datos "C:\Users\agust\Desktop\Universidad de Chile\Videos FENVID\Stata\Datos"
use "$datos/base_reshape", clear	

reshape wide variable_interes, i(var_ID) j(var_temporal)
reshape wide precio, i(Empresa) j(year)


reshape long variable_interes, i(var_ID) j(var_temporal)
reshape long precio, i(Empresa) j(year_nuevo)
}

*-Combinación y Condensasión de Bases de datos
{
global datos "C:\Users\agust\Desktop\Universidad de Chile\Videos FENVID\Stata\Datos"

merge 1:1 varid_1 varid_2... using nombre_archivo
merge m:1 varid_1 varid_2... using nombre_archivo
merge 1:m varid_1 varid_2... using nombre_archivo
merge m:m varid_1 varid_2... using nombre_archivo
h joinby
	
append using "nombre_archivo"


use "$datos/Collapse", clear
collapse (mean) edad (max) educacion (p50) ingreso, by (hogar)
tabstat edad educacion ingreso, stats(mean min max)


use "$datos/Collapse", clear
preserve
collapse (mean) edad (max) educacion (p50) ingreso, by (hogar)
tabstat edad educacion ingreso, stats(mean min max)
restore
}

*-Regresiones (Próximamente)
{
global datos "C:\Users\agust\Desktop\Universidad de Chile\Videos FENVID\Stata\Datos"
global graficos "C:\Users\agust\Desktop\Universidad de Chile\Videos FENVID\Stata\Imágenes"
global tablas "C:\Users\agust\Desktop\Universidad de Chile\Videos FENVID\Stata\Tablas"

use "$datos/Casen 2017", clear

*Sintaxis y Exportación
regress var_dep regresores... [factor_expansión] if condicionales..., vce(opción_error_estándar) level(Nivel_confianza)

cd "$tablas"
outreg2 using nombre_archivo, formato_de_salida

*Variables Categóricas, Continuas e Interacciones
reg var_dep regresor_1 regresor_2... c.var_continua##c.var_continua [factor_expansión]
reg var_dep regresor_1 regresor_2... i.var_categorica [factor_expansión]
reg var_dep regresor_1 regresor_2... ibN°.var_categorica [factor_expansión]
reg var_dep regresor_1 regresor_2... i.var_categorica##c.var_continua [factor_expansión]

*Valores Predichos y Residuos
regresion
predict nueva_variable, xb
predict nueva_variable, resid

*Margins
regresion
margins, at(nombre_variable=(desde (delta) hasta) var_categorica=(N°_1 N°_2...))
marginsplot

*Heterocedasticidad, Multicolinealidad, Normalidad, No linealidad omitida
regresion
estat hettest // Test de Breusch-Pagan
imtest, white // Test de White
estat vif // Test de inflación de varianza
coldiag2, eigenval // Test de Belsey. Instalar coldiag2
jb resid // Test de Jaque-Bera. instalar jb
sktest resid // Test de Kolmogorov-Smirnov
estat ovtest // Test de Ramsey

*Variables Instrumentales
reg x z // Primera etapa
predict xgorro, xb
reg y xgorro w // Segunda etapa

ivregress 2sls var_dep var_exógenas... (var_endógena=instrumento) [factor_expansión] if condición_1 condición_2 ..., opción_primera_etapa

ivregress gmm var_dep var_exógenas... (var_endógena=instrumento) [factor_expansión] if condiciones..., opción_primera_etapa

estat firststage // Test Fortaleza de Instrumentos
estat overid


*Comparación de modelos
regresión_1
estimates store nombre_1
regresión_2
estimates store nombre_2
...
estimates table nombre_1 nombre_2..., stat(rss r2 r2_a ll aic bic n) 
esttab nombre_1 nombre_2..., stat(rss r2 r2_a ll aic bic n) 

*Bootstrap
bootstrap, reps(N°_repeticiones): reg var_dependiente regresores, opciones
estat bootstrap, all

                             *Probit/Logit
*Modelos Binarios
probit var_dependiente_binaria var_independientes if condicionales [factor_de_expansión] 
logit var_dependiente_binaria var_independientes if condicionales [factor_de_expansión] 

estat classification
lsens
margins [factor_de_expansión], dydx(*) atmeans

*Modelos Multinomiales
oprobit var_dependiente_categórica var_independientes if condicionales [factor_de_expansión] 
ologit var_dependiente_categórica var_independientes if condicionales [factor_de_expansión] 


                             *Cuantiles
qreg var_dep regresores, quantile(.N°)
bsqreg var_dep regresores, quantile(.N°), reps(N°)
grqreg variable, ci							 
							 
}

*-Series de Tiempo (Próximamente)
{
clear all
use "C:\Users\agust\Desktop\Universidad de Chile\Videos FENVID\Stata\Datos\base_precios.dta"

tsset variable_tiempo // Indicamos al Stata cuál es la var temporal

tsline var1 var2...  // Gráfico temporal
reg variable L.variable // L. es el operador lag

g g_dolar=100*(ln(dolar)-ln(L.dolar)) 
arima variable, arima(N° AR,N° I,N° MA)
estat ic

dfuller variable, noconstant
dfuller variable, drift
dfuller variable, trend

}

*-Programación (Próximamente)
{
*FGLS
regresion 
predict residuos_originales, resid

g nuevos_residuos=funcion(residuos_originales)

reg nuevos_residuos regresores 
predict valores_predichos, xb 

g pre_ponderador=exp(valores_predichos)
g ponderador=1/valores_predichos

regresion [w=ponderador]
	
	
*NLLS
nl (var_dependiente = {forma_funcional}), initial(_b[_cons] pto_partida _b[regresor_1] pto_partida _b[regresor_2] pto_partida...) // Por defecto el punto de partida es cero.

predict ygorro, xb 
scatter var_dependiente regresor || line ygorro regresor, sort

*Sesgo de Selección (Heckman)
probit var_categorica regresores, vce(opción_error_estándar)
predict variable_gorro, xb 
g mills=normalden(variable_gorro)/normal(variable_gorro)
reg var_dep regresores mills, vce(opción_error_estándar) //Método "Manual"

heckman var_dep regresores, select(var_primera_etapa regresores) two	
	
	
if 0<1 {
display("0 es menor que 1")}	
	
set obs número
set seed número 
set matsize número
mat define A = J(número_filas,número_columnas,Valores_matriz) 
mat list A 
mat A[1,1] = r(N) // Le estoy diciendo a Stata que rellene la fila uno columna con el N de la variable respectiva.
mat A[1,2] = r(mean) // Lo mismo con la media
mat A[1,3] = r(sd) // Lo mismo con la desviacion estandar.


matrix Beta=J(1000,2,0)
forval m=1(1)1000{
preserve
bsample
reg prom_gral asistencia
matrix Beta[`m',1]=_b[_cons]
matrix Beta[`m',2]=_b[asistencia]

restore 
}
svmat Beta 
}




