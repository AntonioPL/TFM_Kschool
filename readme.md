#Proyecto de Fin de Master: Automatizaci�n del procesado de encuestas de valoraci�n

##Motivaci�n

Las encuestas de valoraci�n son una herramienta muy �til para cualquier empresa u organizaci�n, en la medida que permite un feedback entre la prestaci�n del servicio y el usuario. 
No obstante el procesamiento de las mismas no es trivial en el caso de empresas medianas y peque�as. Dicho proceso involucra la elaboraci�n, recolecci�n y procesado de los resultados,
en el presente proyecto nos concentramos en esta �ltima.

El nuestro caso llamamos procesado al conjunto de acciones que van desde la limpieza del output de una lectora �ptica hasta la presentaci�n de dichos resultados pasando por un 
an�lisis de los mismos. En el caso de peque�as empresas, realizar dicha labor con el software propietario al cual se suele tener acceso habitualmente puede llegar a ser una labor cuasi-artesanal, donde 
se procesan los datos de la lectora de forma manual, se consolidan en un software, se realiza el an�lisis estad�stico en otro y la presentaci�n en otro, siendo muy complicado y en ciertos casos imposibles
la automatizaci�n.

##Descripci�n del Proyecto

En virtud de lo anterior se propone un automatizar el procesado a trav�s de los siguientes pasos 

* Automatizar el proceso de limpieza de los datos originales de la lectora, evitando cualquier manipulaci�n manual. Esto se realiza con un c�digo Python que realice las manipulaciones necesarias.

* Automatizar la realizaci�n y presentaci�n del an�lisis descriptivo de las encuestas. Una vez realizado el paso anterior se emplea el lenguaje R para realizar todo el an�lisis estad�stico descriptivo, y mediante el R markdown se elaboran reportes agragados y por divisi�n. Tentativamente si el tiempo lo permite se realizar�un dashboard en Shiny para elaborar reportes interactivos.

* Automatizar el reporte individual por profesor empleando R markdown. Este paso es relevante ya que sustituir�a la elaboraci�n manual de unos 100 reportes.

* Aprovechando la disponibilidad de los datos se realiza un an�lisis de los determinantes del item valoraci�n global de la encuesta en funci�n del resto de items y otros datos externos (edad, divisi�n, categor�a, sexo, naturaleza de la asignatura (cuantitativa o no), etc.) empleando un logit. Dada la alta correlaci�n presente entre los distintos items de la encuesta, se propone realizar un An�lisis Factoral Exploratorio previo al an�lisis logit.

##Potenciales Beneficios para la empresa

Se estima que este proyecto podr�a reducir en aproximadamente un tercio el tiempo de procesado de las encuestas. Las �reas cr�ticas son el de limpieza manual de los datos brutos de la lectora y la 
elaboraci�n de los reportes, los cuales al estar completamente automatizado su tiempo de ejecuci�n ser�a de minutos cuando en la actualidad se dedican horas en el mismo.

Lo anterior permitir�a a la empresa concentrarse en el dise�o de la encuesta en lugar de su procesado, as� como plantearse la posiblidad de una utilizaci�n m�s activa de esta herramienta en otras �reas


