# Proyecto de Fin de Master: Automatización del procesado de encuestas de valoración

## Motivación

Las encuestas de valoración son una herramienta muy útil para cualquier empresa u organización, en la medida que permite un feedback entre la prestación del servicio y el usuario. 
No obstante el procesamiento de las mismas no es trivial en el caso de empresas medianas y pequeñas. Dicho proceso involucra la elaboración, recolección y procesado de los resultados.
En el presente proyecto nos concentramos en esta última.

El nuestro caso llamamos procesado al conjunto de acciones que van desde la limpieza del output de una lectora óptica hasta la presentación de dichos resultados pasando por un 
análisis de los mismos. En el caso de pequeñas empresas, realizar dicha labor con el software propietario al cual se suele tener acceso habitualmente puede llegar a ser una labor cuasi-artesanal, donde 
se procesan los datos de la lectora de forma manual, se consolidan en un software, se realiza el análisis estadístico en otro y la presentación en otro, siendo muy complicado y en ciertos casos imposibles
la automatización.

## Descripción del Proyecto

En virtud de lo anterior se propone un automatizar el procesado a través de los siguientes pasos 

* Automatizar el proceso de limpieza de los datos originales de la lectora, evitando cualquier manipulación manual. Esto se lleva a cabo con un código Python que realice las manipulaciones necesarias.

* Automatizar la realización y presentación del análisis descriptivo de las encuestas. Una vez realizado el paso anterior se emplea el lenguaje R para hacer todo el análisis estadístico descriptivo, y empleando el R markdown se elaboran reportes agregados y por división. Tentativamente si el tiempo lo permite se realizará un dashboard en Shiny para elaborar reportes interactivos.

* Automatizar el reporte individual por profesor empleando R markdown. Este paso es relevante ya que sustituiría la elaboración manual de unos 100 reportes.

* Aprovechando la disponibilidad de los datos se realiza un análisis de los determinantes del item valoración global de la encuesta en función del resto de items y otros datos externos (edad, división, categoría, sexo, naturaleza de la asignatura (cuantitativa o no), etc.) empleando un logit. Dada la alta correlación presente entre los distintos items de la encuesta, se propone realizar un Análisis Factorial Exploratorio previo al análisis logit<sup>[1](#myfootnote1)</sup>.

* Una vez analizados los datos, si se considera necesario se realizará un análisis de segmentación en función de detectar grupos diferenciados de profesores o asignaturas. El mismo se realizaría en R

## Potenciales Beneficios para la empresa

Se estima que este proyecto podría reducir en aproximadamente un tercio el tiempo de procesado de las encuestas. Las áreas críticas son el de limpieza manual de los datos brutos de la lectora y la 
elaboración de los reportes, los cuales al estar completamente automatizado su tiempo de ejecución sería de minutos cuando en la actualidad se dedican horas en el mismo.

Lo anterior permitiría a la empresa concentrarse en el diseño de la encuesta en lugar de su procesado, así como plantearse la posiblidad de una utilización más activa de esta herramienta en otras áreas

<a name="myfootnote1">1</a>: Gracias a Antonio Pita por esta recomendación.

## Resultados

Se ha logrado reducir los tiempos de limpieza de datos y elaboración de reportes de 4 y 5 horas respectivamente a menos de un minuto (entre 13 segundos y 18 segundos dependiendo de la máquina) el primero y 4 minutos el segundo.

Empíricamente se han identificado dos factores latentes que resumen los items de valoración de la encuesta denominados *docencia* y *relación profesor-alumno*, como los principales determinantes de la probabilidad de obtener una valoración alta. Asimismo, se determina que ciertas características individuales de los profesores tales como el sexo, categoría, edad, etc. no influye de manera determinante en el grado de satisfacción global del alumno.

## Replicación del Proyecto

Para poder replicar lo realizado en el proyecto, se deben ejecutar los archivos de la carpeta [Replicacion](https://github.com/kamecon/TFM_Kschool/tree/master/Replicacion), en el siguiente orden:

* Primero: El código python en el archivo limpieza.py

* Segundo: El código R en el archivo encuestas.r. Para ejecutar este código *se debe fijar el directorio de trabajo en el archivo*

Se obtendrá como resultado 96 reportes en formato pdf y los resultados del ejercicio empírico

## Dashboard y otros documentos

El dashboard se encuentra [ACÁ](https://github.com/kamecon/TFM_Kschool/tree/master/Replicacion)

Los resultados empíricos se encuentran descritos con detalle en el archivo analisis_empirico.html que se encuentra en la carpeta [Replicacion](https://github.com/kamecon/TFM_Kschool/tree/master/Replicacion)

Los detalles del proceso de limpieza de datos están detallados en este [notebook](https://github.com/kamecon/TFM_Kschool/blob/master/Replicacion/Tidy1.ipynb)

Se puede ver un resumen del proyecto en la [memoria](https://github.com/kamecon/TFM_Kschool/blob/master/Memoria/Memoria2.pdf)
