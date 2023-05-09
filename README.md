# GeneradorDatosNegocios

Este repositorio contiene un R-package con funciones generadoras de datos aleatorios de ventas (próximamente también de costos). Genera planillas que simulan ventas mensuales de negocios de distintos ámbitos. Como resultado de su ejecución, se obtienen planillas mensuales en formato csv. Se hallarán:

-   Tablas de ventas codificadas simulando la extracción sucia de un CRM o algún otro sistema/base de datos,

-   Tablas de ventas decodificadas simulando la extracción user-friendly de los datos,

-   Tablas de dimensiones para procesar las tablas de ventas codificadas.

Los datos generados resultan de utilidad para aquellos que se inician en la programación, reportería y visualización de datos, como una base de datos que pueden segmentar y transformar de distintas maneras.

Para cargar el paquete es necesario instalar *devtools:*

`install.packages("devtools")`

Posteriormente o si ya se lo tenía instalado, se debe cargar *devtools*, instalar *GeneradorDatosNegocios* desde Github y cargalo con las 3 siguientes líneas de código:

`library(devtools)`

`install_github("orgranada/GeneradorDatosNegocios")`

`library(GeneradorDatosNegocios)`

La función `genVentas()` ya estará disponible para generar los datos. Cualquier duda sobre los parámetros de la función pueden clarificarse con `?genVentas`.

Espero que sea de utilidad.

¡¡Saludos y muchos éxitos!!
