#Proyecto de Fin de Master: Automatización del procesado de encuestas de valoración

##Motivación

Las encuestas de valoración son una herramienta muy útil para cualquier empresa u organización, en la medida que permite un feedback entre la prestación del servicio y el usuario. 
No obstante el procesamiento de las mismas no es trivial en el caso de empresas medianas y pequeñas. Dicho proceso involucra la elaboración, recolección y procesado de los resultados,
en el presente proyecto nos concentramos en esta última.

El nuestro caso llamamos procesado al conjunto de acciones que van desde la limpieza del output de una lectora óptica hasta la presentación de dichos resultados pasando por un 
análisis de los mismos. En el caso de pequeñas empresas, realizar dicha labor con el software propietario al cual se suele tener acceso habitualmente puede llegar a ser una labor cuasi-artesanal, donde 
se procesan los datos de la lectora de forma manual, se consolidan en un software, se realiza el análisis estadístico en otro y la presentación en otro, siendo muy complicado y en ciertos casos imposibles
la automatización.

##Descripción del Proyecto

En virtud de lo anterior se propone un automatizar el procesado a través de los siguientes pasos 

* Automatizar el proceso de limpieza de los datos originales de la lectora, evitando cualquier manipulación manual. Esto se realiza con un código Python que realice las manipulaciones necesarias.

* Automatizar la realización y presentación del análisis descriptivo de las encuestas. Una vez realizado el paso anterior se emplea el lenguaje R para realizar todo el análisis estadístico descriptivo, y mediante el R markdown se elaboran reportes agragados y por división. Tentativamente si el tiempo lo permite se realizaráun dashboard en Shiny para elaborar reportes interactivos.

* Automatizar el reporte individual por profesor empleando R markdown. Este paso es relevante ya que sustituiría la elaboración manual de unos 100 reportes.

* Aprovechando la disponibilidad de los datos se realiza un análisis de los determinantes del item valoración global de la encuesta en función del resto de items y otros datos externos (edad, división, categoría, sexo, naturaleza de la asignatura (cuantitativa o no), etc.) empleando un logit. Dada la alta correlación presente entre los distintos items de la encuesta, se propone realizar un Análisis Factoral Exploratorio previo al análisis logit.

##Potenciales Beneficios para la empresa

Se estima que este proyecto podría reducir en aproximadamente un tercio el tiempo de procesado de las encuestas. Las áreas críticas son el de limpieza manual de los datos brutos de la lectora y la 
elaboración de los reportes, los cuales al estar completamente automatizado su tiempo de ejecución sería de minutos cuando en la actualidad se dedican horas en el mismo.

Lo anterior permitiría a la empresa concentrarse en el diseño de la encuesta en lugar de su procesado, así como plantearse la posiblidad de una utilización más activa de esta herramienta en otras áreas


