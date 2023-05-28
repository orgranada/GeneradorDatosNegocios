#' Generador de datos de ventas
#'
#' genVentas simula la extracción de datos de ventas de una empresa. La función exporta los datos mensuales generados a una carpeta dentro del working directory, conteniendo:
#'     a. Tablas de dimensiones
#'     b. Tablas de ventas codificadas
#'     c. Tablas de ventas decodificadas con las tablas de dimensiones
#'
#'@param dataset_nombre String/texto, define el nombre de la carpeta que contendrá los datos generados. Predeterminado: "Dataset01".
#'@param negocio_tipo String/texto, define el tipo de negocio al cual se dedica la empresa simulada. Valores validos para este parámetro son: INFORMATICA, LIBROS, RESTAURANTE, TELECOMUNICACIONES, VESTIMENTA, VIDEOJUEGOS, VARIOS. Predeterminado: "VARIOS".
#'@param n Integer/número entero, determina la cantidad de transacciones totales deseadas. Predeterminado: 120000.
#'@param fecha_min String/texto, indica la fecha mínima a partir de la cual se generarán los datos. Debe indicarse en formato yyyy/mm/dd. Predeterminado: "2022/01/01".
#'@param fecha_max String/texto, indica la fecha máxima hasta la cual se generarán los datos. Debe indicarse en formato yyyy/mm/dd. Predeterminado: "2022/12/31".
#'@param moneda String/texto, señala la moneda en la cual estarán expresados los precios de los productos comercializados. Valores válidos para este parámetros son: USD, PYG. Predeterminado: "USD".
#'
#'@returns 1 carpeta definida por dataset_nombre exportada en el working directory. Dicha carpeta contiene 3 sub-carpetas: Tablas de dimensiones, tablas de ventas codificadas, tablas de ventas decodificadas.
#'
#'@examples
#'genVentas (dataset_nombre = "Dataset01", negocio_tipo = "VARIOS", n = 120000, fecha_min = "2022/01/01", fecha_max = "2022/12/31", moneda = "PYG")
#'genVentas ("miEmpresa", "2020/12/01", "2024/06/30", "VESTIMENTA")



genVentas <- function(dataset_nombre = "Dataset01",
                           negocio_tipo = "VARIOS",
                           n = 120000,
                           fecha_min = "2022/01/01",
                           fecha_max = "2022/12/31",
                           moneda = "USD")
{
  # PASO 1: VALIDACION ------------------------------------------------------
  #En este paso se validan los inputs introducidos al ejecutar la función.
  #____________


  #Validar dataset type
  validDbType <- c("INFORMATICA", "LIBROS", "RESTAURANTE", "TELECOMUNICACIONES", "UTILES", "VARIOS", "VESTIMENTA", "VIDEOJUEGOS")
  if (!negocio_tipo %in% validDbType) {
    stop("Valor invalido para negocio_tipo.
    Revise la ortografia para coincidir con:
         INFORMATICA
         LIBROS
         RESTAURANTE
         TELECOMUNICACIONES
         UTILES
         VARIOS
         VESTIMENTA
         VIDEOJUEGOS")
  }

  #Validar n
  if (!class(n) == "numeric"){
    stop("Valor invalido para n.
         Introduzca un valor numérico.")
  }

  #Validar fechas
  if (!class(as.Date(fecha_min)) == "Date"){
    stop("Valor invalido para fecha_min.
    Introduzca un valor texto con formato:
         yyyy/mm/dd")
  }

  if (!class(as.Date(fecha_max)) == "Date"){
    stop("Valor invalido para fecha_max.
    Introduzca un valor texto con formato:
         yyyy/mm/dd")
  }

  #Validar moneda
  validMoneda <- c("PYG","USD")
  if (!moneda %in% validMoneda) {
    stop("Valor invalido para moneda.
    Revise la ortografia para coincidir con:
         PYG
         USD")
  }

  # PASO 2: DEFINICION DE PARAMETROS Y FUNCIONES ----------------------------
  #En este paso se configuran las variables que se utilizarán en los siguientes
  #____________


  #Forzar que las n sean numeros enteros
  n <- round(n,0)

  #Definir i como número de iteraciones para correr for-loops
  i <- 1

  #Definir db como variable numérica del dataset (tipo de negocio).
  #Servirá para indicar el índice de las listas generadas más adelante.
  db <- if (negocio_tipo == "INFORMATICA") {1} else if (negocio_tipo == "LIBROS") {2} else if (negocio_tipo == "RESTAURANTE") {3} else if (negocio_tipo == "TELECOMUNICACIONES") {4} else if (negocio_tipo == "UTILES") {5} else if (negocio_tipo == "VARIOS") {6} else if (negocio_tipo == "VESTIMENTA") {7} else if (negocio_tipo == "VIDEOJUEGOS") {8}

  #Definir catNum como variable del número máximo de categorías del dataset
  catNum <- max(dbProdCategorias[[db]])

  #Definir clnType y clnNum como variable del tipo de cliente y número máximo de clientes según el tipo de negocio
  # 1 = FISICA, 2 = JURIDICA
  if (negocio_tipo == "INFORMATICA" || negocio_tipo == "TELECOMUNICACIONES" || negocio_tipo == "VARIOS") {
    clnType <- 2
    clnNum <- 30
  } else {
    clnType <- 1
    clnNum <- 100
  }


  #genCodProd: función que genera n códigos aleatorios sin repetición de 3 letras y 3 dígitos
  genCodProd <- function(n = 10) {
    prefijo <- do.call(paste0, replicate(3, sample(LETTERS, n, TRUE), FALSE))
    paste0(prefijo, sprintf("%03d", sample(999, n, TRUE)))
  }

  #genCodCliente: función que genera n códigos aleatorios sin repetición de 1 letras y 3 dígitos
  genCodCliente <- function(n = 10) {
    prefijo <- do.call(paste0, replicate(1, sample(LETTERS, n, TRUE), FALSE))
    paste0(prefijo, sprintf("%03d", sample(999, n, TRUE)))
  }

  #genNumContacto: función que genera números telefónicos aleatorios de Paraguay sin repetición
  genNumContacto <- function(n = 10) {
    paste0("(09",sample(7:9,n,TRUE),sample(1:7,n,TRUE),") ", sprintf("%03d", sample(999,n,FALSE)), sprintf("%03d", sample(999,n,FALSE)))
  }


  # PASO 3: REVISION Y CREACION DEL DIRECTORIO ------------------------------
  #En este paso se revisa y crea el conjunto de carpetas de output
  #____________


  #Revisar si existe carpeta del dataset
  if(!file.exists(dataset_nombre)) {
    dir.create(file.path(dataset_nombre), showWarnings = FALSE)
  }

  #Revisar si existen carpetas para las exportaciones
  ##Para las tablas de dimensiones:
  if (!file.exists(paste0(dataset_nombre,"/TABLAS_DIMENSIONES"))) {
    dir.create(file.path(paste0(dataset_nombre,"/TABLAS_DIMENSIONES")), showWarnings = FALSE)
  }

  ##Para las tablas de ventas codificadas
  if (!file.exists(paste0(dataset_nombre,"/VENTAS_CODIFICADO"))) {
    dir.create(file.path(paste0(dataset_nombre,"/VENTAS_CODIFICADO")), showWarnings = FALSE)
  }

  ##Para las tablas de ventas decodificados
  if (!file.exists(paste0(dataset_nombre,"/VENTAS_DECODIFICADO"))) {
    dir.create(file.path(paste0(dataset_nombre,"/VENTAS_DECODIFICADO")), showWarnings = FALSE)
  }

  message("1 de 6: Directorio preparado...")


  # PASO 4: CREACION DE TABLAS DE DIMENSIONES -------------------------------
  #En este paso se crean todas las tablas de dimensiones que servirán para decodificar la tabla de hechos
  #____________


  D_PRODUCTOS <- data.frame(
    PRODUCTO_ID = 1:10,
    PRODUCTO_CODIGO = genCodProd(10),
    PRODUCTO_NOMBRE = dbProductos[[db]],
    CATEGORIA_CODIGO = dbProdCategorias[[db]]
  )
  #controlar si los códigos de productos generados son únicos y volver a generar si no lo son
  while(TRUE %in% duplicated(D_PRODUCTOS$PRODUCTO_CODIGO) == TRUE){
    D_PRODUCTOS$PRODUCTO_CODIGO <- genCodProd(10)
  }


  D_CATEGORIA <- data.frame(
    CATEGORIA_CODIGO = 1:catNum,
    CATEGORIA_NOMBRE = dbCategorias[[db]]
  )

  D_TIENDA <- data.frame(
    TIENDA_ID = 1:11,
    TIENDA_CODIGO = c("CC", "LUQ", "SLZ", "CDE", "PJC", "ENC", "PLR", "COV",
                      "CZP", "BOQ", "CON"),
    TIENDA_NOMBRE = c("CASA CENTRAL", "SUC. LUQUE", "SUC. SAN LORENZO",
                      "SUC. CIUDAD DEL ESTE", "SUC. PEDRO JUAN CABALLERO",
                      "SUC. ENCARNACION", "SUC. PILAR", "SUC. CORONEL OVIEDO",
                      "SUC. CAAZAPA", "SUC. BOQUERON", "SUC. CONCEPCION"),
    REGION_CODIGO = c(0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
  )

  D_REGION <- data.frame(
    REGION_CODIGO = 0:5,
    REGION_NOMBRE = c("CAPITAL", "CENTRAL", "ESTE", "SUR", "CENTRO", "NORTE")
  )

  D_VENDEDOR <- data.frame(
    VENDEDOR_ID = 1:21,
    VENDEDOR_CODIGO = c("0076", "0034", "0101", "0234", "0061", "0845", "0140", "0410", "0222", "1020",
                        "1045", "0874", "1309", "0945", "0557", "0421", "1142", "1236", "0063", "0878",
                        "0010"),
    VENDEDOR_NOMBRE = c("ROMAN RIQUELME, JULIO ANDRES",
                        "RONALDO, CRISTIANO",
                        "HERNANDEZ CASCANTE, RODRIGO",
                        "WOODS, TIGER ANTONIO",
                        "SALAH, MOJAMÉ",
                        "SON, KASHI AI",
                        "MESSI, LIONEL",
                        "OBU, OBU",
                        "HAMILTON, LEWIS",
                        "VERSTAPPEN, MAX",
                        "CORONEL, JOSE LUIS",
                        "ARZAMENDIA DELGADILLO, ENRIQUETTO",
                        "CHILAVERT, MARIA LUISA",
                        "CHALAMET, MIBABI",
                        "SERVIAN, PHIL",
                        "BENITEZ OVELAR, PANAMBI MICAELA",
                        "QUILMES LEGUIZAMON, FABIO MARTIN",
                        "DIAZ, LAMENTACIONES",
                        "ROLON, EDMUNDO RAUL",
                        "BOLAÑOS GIMENEZ, TIMOTEO MAURICIO",
                        "REGENTE SOLOMON, VICTOR VICENTE"),

    FECHA_INGRESO = sample(seq(as.Date(fecha_min) - 12*365, as.Date(fecha_min), by="day"), 21, replace = F),
    HIJOS_MENORES = round(abs(rnorm(21,2,2)),0),
    NIVEL_ESTUDIOS = round(runif(21,1,3),0)
  )
  D_VENDEDOR$NIVEL_ESTUDIOS <- factor(D_VENDEDOR$NIVEL_ESTUDIOS, levels = 1:3, labels = c("BACHILLER", "ESTUDIANTE UNIVERSITARIO", "GRADO UNIVERSITARIO"))


  D_MEDIOPAGO <- data.frame(
    MEDIOPAGO_ID = 1:5,
    MEDIOPAGO_CODIGO = c("EF", "CH", "TC", "TD", "TB"),
    MEDIOPAGO_DESCRIPCION = c("EFECTIVO", "CHEQUE", "TARJETA DE CREDITO", "TARJETA DE DEBITO", "TRANSFERENCIA BANCARIA")
  )

  D_FORMAPAGO <- data.frame(
    FORMAPAGO_ID = 1:4,
    FORMAPAGO_CODIGO = c("CNTD", "CR30", "CR60", "CR90"),
    FORMAPAGO_DESCRIPCION = c("CONTADO", "CREDITO A 30 DIAS", "CREDITO A 60 DIAS", "CREDITO A 90 DIAS")
  )


  D_PRECIOS <- data.frame(
    PRODUCTO_CODIGO = D_PRODUCTOS$PRODUCTO_CODIGO,
    PRECIO_UNITARIO = if (moneda == "PYG"){dbPreciosPyg[[db]]} else if (moneda == "USD"){dbPreciosUsd[[db]]}
  )

  D_CLIENTE <- data.frame(
    CLIENTE_ID = c(1:clnNum),
    CLIENTE_CODIGO = genCodCliente(clnNum),
    CLIENTE_DENOMINACION = sample(dbClientes[[clnType]],clnNum,replace = FALSE),
    FECHA_PRIMERA_COMPRA = sample(seq(as.Date(fecha_min) - 12*365, as.Date(fecha_min), by="day"), clnNum, replace = T),
    CONTACTO_TELEFONO = genNumContacto(clnNum),
    RESIDENCIA = sample(dbPaises,clnNum,replace = TRUE)
  )
  while(TRUE %in% duplicated(D_CLIENTE$CLIENTE_CODIGO) == TRUE){
    D_CLIENTE$CLIENTE_CODIGO <- genCodCliente(clnNum)
  }
  while(TRUE %in% duplicated(D_CLIENTE$CONTACTO_TELEFONO) == TRUE){
    D_CLIENTE$CONTACTO_TELEFONO <- genCodCliente(clNum)
  }
  D_CLIENTE$RESIDENCIA[1:round(clnNum*0.45)] <- "PRY - PARAGUAY"

  message("2 de 6: Tablas de dimensiones preparadas...")


  # PASO 5: DEFINICION DE TABLA DE HECHOS -----------------------------------
  #En este paso se crea la tabla de ventas.
  #Primeramente se generan valores aleatorios que luego serán reemplazados por códigos y también por sus descripciones.
  #____________


  #Crear datos aleatorios codificados
  H_VENTAS <- data.frame(
    TRANSACCION = sprintf("%06d", sample(1:n, n, replace = F)),
    FECHA_VENTA = sample(seq(as.Date(fecha_min), as.Date(fecha_max), by="day"), n, replace = T),
    PRODUCTO_ID = round(rnorm(n, 5), 0),
    TIENDA_ID = round(runif(n, min = 1, max = 11)),
    MEDIOPAGO_ID = round(runif(n, min = 1, max = 5)),
    FORMAPAGO_ID = round(runif(n, min = 1, max = 4)),
    CLIENTE_ID = round(runif(n, min = 1, max = clnNum)),
    CANTIDAD = round(rexp(n, rate = 0.6),0)
  )


  #Reemplazar 0 por 1 en CANTIDAD
  H_VENTAS$CANTIDAD[H_VENTAS$CANTIDAD == 0] <- 1


  #Crear datos aleatorios codificados para el vendedor en función de la tienda
  H_VENTAS_1 <- subset(H_VENTAS, TIENDA_ID == 1)
  H_VENTAS_1$VENDEDOR_ID <- round(runif(nrow(H_VENTAS_1), min = 1, max = 3))

  H_VENTAS_2 <- subset(H_VENTAS, TIENDA_ID == 2)
  H_VENTAS_2$VENDEDOR_ID <- round(runif(nrow(H_VENTAS_2), min = 4, max = 5))

  H_VENTAS_3 <- subset(H_VENTAS, TIENDA_ID == 3)
  H_VENTAS_3$VENDEDOR_ID <- round(runif(nrow(H_VENTAS_3), min = 6, max = 7))

  H_VENTAS_4 <- subset(H_VENTAS, TIENDA_ID == 4)
  H_VENTAS_4$VENDEDOR_ID <- round(runif(nrow(H_VENTAS_4), min = 8, max = 10))

  H_VENTAS_5 <- subset(H_VENTAS, TIENDA_ID == 5)
  H_VENTAS_5$VENDEDOR_ID <- 11

  H_VENTAS_6 <- subset(H_VENTAS, TIENDA_ID == 6)
  H_VENTAS_6$VENDEDOR_ID <- round(runif(nrow(H_VENTAS_6), min = 12, max = 14))

  H_VENTAS_7 <- subset(H_VENTAS, TIENDA_ID == 7)
  H_VENTAS_7$VENDEDOR_ID <- 15

  H_VENTAS_8 <- subset(H_VENTAS, TIENDA_ID == 8)
  H_VENTAS_8$VENDEDOR_ID <- round(runif(nrow(H_VENTAS_8), min = 16, max = 17))

  H_VENTAS_9 <- subset(H_VENTAS, TIENDA_ID == 9)
  H_VENTAS_9$VENDEDOR_ID <- 18

  H_VENTAS_10 <- subset(H_VENTAS, TIENDA_ID == 10)
  H_VENTAS_10$VENDEDOR_ID <- 19

  H_VENTAS_11 <- subset(H_VENTAS, TIENDA_ID == 11)
  H_VENTAS_11$VENDEDOR_ID <- round(runif(nrow(H_VENTAS_11), min = 20, max = 21))

  H_VENTAS <- rbind(H_VENTAS_1, H_VENTAS_2, H_VENTAS_3, H_VENTAS_4, H_VENTAS_5,
                    H_VENTAS_6, H_VENTAS_7, H_VENTAS_8, H_VENTAS_9, H_VENTAS_10,
                    H_VENTAS_11)

  rm(H_VENTAS_1, H_VENTAS_2, H_VENTAS_3, H_VENTAS_4, H_VENTAS_5,
     H_VENTAS_6, H_VENTAS_7, H_VENTAS_8, H_VENTAS_9, H_VENTAS_10,
     H_VENTAS_11)

  #Reemplazar datos aleatorios por códigos

  H_VENTAS$PRODUCTO_CODIGO <- factor(H_VENTAS$PRODUCTO_ID, levels = D_PRODUCTOS$PRODUCTO_ID, labels = D_PRODUCTOS$PRODUCTO_CODIGO)
  H_VENTAS$CATEGORIA_CODIGO <- factor(H_VENTAS$PRODUCTO_ID, levels = D_PRODUCTOS$PRODUCTO_ID, labels = D_PRODUCTOS$CATEGORIA_CODIGO)
  H_VENTAS$TIENDA_CODIGO <- factor(H_VENTAS$TIENDA_ID, levels = D_TIENDA$TIENDA_ID, labels = D_TIENDA$TIENDA_CODIGO)
  H_VENTAS$REGION_CODIGO <- factor(H_VENTAS$TIENDA_ID, levels = D_TIENDA$TIENDA_ID, labels = D_TIENDA$REGION_CODIGO)
  H_VENTAS$MEDIOPAGO_CODIGO <- factor(H_VENTAS$MEDIOPAGO_ID, levels = D_MEDIOPAGO$MEDIOPAGO_ID, labels = D_MEDIOPAGO$MEDIOPAGO_CODIGO)
  H_VENTAS$FORMAPAGO_CODIGO <- factor(H_VENTAS$FORMAPAGO_ID, levels = D_FORMAPAGO$FORMAPAGO_ID, labels = D_FORMAPAGO$FORMAPAGO_CODIGO)
  H_VENTAS$CLIENTE_CODIGO <- factor(H_VENTAS$CLIENTE_ID, levels = D_CLIENTE$CLIENTE_ID, labels = D_CLIENTE$CLIENTE_CODIGO)
  H_VENTAS$VENDEDOR_CODIGO <- factor(H_VENTAS$VENDEDOR_ID, levels = D_VENDEDOR$VENDEDOR_ID, labels = D_VENDEDOR$VENDEDOR_CODIGO)
  H_VENTAS$PRECIO_UNITARIO <- as.numeric(as.character(factor(H_VENTAS$PRODUCTO_CODIGO, levels = D_PRECIOS$PRODUCTO_CODIGO, labels = D_PRECIOS$PRECIO_UNITARIO)))

  #Reemplazar datos aleatorios por nombres
  H_VENTAS$PRODUCTO_NOMBRE <- factor(H_VENTAS$PRODUCTO_ID, levels = D_PRODUCTOS$PRODUCTO_ID, labels = D_PRODUCTOS$PRODUCTO_NOMBRE)
  H_VENTAS$CATEGORIA_NOMBRE <- factor(H_VENTAS$CATEGORIA_CODIGO, levels = D_CATEGORIA$CATEGORIA_CODIGO, labels = D_CATEGORIA$CATEGORIA_NOMBRE)
  H_VENTAS$TIENDA_NOMBRE <- factor(H_VENTAS$TIENDA_ID, levels = D_TIENDA$TIENDA_ID, labels = D_TIENDA$TIENDA_NOMBRE)
  H_VENTAS$REGION_NOMBRE <- factor(H_VENTAS$REGION_CODIGO, levels = D_REGION$REGION_CODIGO, labels = D_REGION$REGION_NOMBRE)
  H_VENTAS$MEDIOPAGO_DESCRIPCION <- factor(H_VENTAS$MEDIOPAGO_ID, levels = D_MEDIOPAGO$MEDIOPAGO_ID, labels = D_MEDIOPAGO$MEDIOPAGO_DESCRIPCION)
  H_VENTAS$FORMAPAGO_DESCRIPCION <- factor(H_VENTAS$FORMAPAGO_ID, levels = D_FORMAPAGO$FORMAPAGO_ID, labels = D_FORMAPAGO$FORMAPAGO_DESCRIPCION)
  H_VENTAS$CLIENTE_DENOMINACION <- factor(H_VENTAS$CLIENTE_ID, levels = D_CLIENTE$CLIENTE_ID, labels = D_CLIENTE$CLIENTE_DENOMINACION)
  H_VENTAS$VENDEDOR_NOMBRE <- factor(H_VENTAS$VENDEDOR_ID, levels = D_VENDEDOR$VENDEDOR_ID, labels = D_VENDEDOR$VENDEDOR_NOMBRE)

  #Agregar id para partir df
  H_VENTAS$AUX <- format(H_VENTAS$FECHA_VENTA, "%Y%m")


  #Formatear fecha
  H_VENTAS$FECHA_VENTA <- format(H_VENTAS$FECHA_VENTA, "%d/%m/%Y")


  #Subset ventas por código y por descripción
  selcolbase <- c("TRANSACCION", "FECHA_VENTA", "CANTIDAD", "PRECIO_UNITARIO", "AUX")
  selcolcod <- grep("_CODIGO$",colnames(H_VENTAS), value = T)
  selcoldec <- grep("_NOMBRE$|_DESCRIPCION$|_DENOMINACION$",colnames(H_VENTAS), value = T)

  H_VENTAS_COD <- H_VENTAS[,c(selcolbase, selcolcod)]
  H_VENTAS_DEC <- H_VENTAS[,c(selcolbase, selcoldec)]

  message("3 de 6: Tabla de hechos confeccionada...")

  # PASO 6: EXPORTACIONES A CSV ---------------------------------------------
  #En este paso las tablas de hechos y dimensiones son exportadas al directorio correspondiente.
  #Las tablas de hechos se dividen por mes y año para generar un archivo por cada uno y otro total.
  #____________


  #Exportar tabla de dimensiones
  TABLAS_DFR <- mget(objects(pattern = "^D_"))
  TABLAS_NOM <- as.list(objects(pattern = "^D_"))

  dimension <- 1
  for (dimension in 1:length(TABLAS_DFR)) {
    nombre <- TABLAS_NOM[dimension]
    tabdf <- TABLAS_DFR[[dimension]]
    write.csv2(tabdf, paste0(dataset_nombre,"/TABLAS_DIMENSIONES/",nombre,".csv"))
  }

  message("4 de 6: Tablas de dimensiones exportadas...")

  #Exportar ventas codificadas

  i <- min(H_VENTAS$AUX)

  for ( i in unique(H_VENTAS$AUX)) {
    EXPORT <- subset(H_VENTAS_COD, AUX == i)
    EXPORT <- subset(EXPORT, select = -c(AUX))
    write.csv2(EXPORT, paste0(dataset_nombre,"/VENTAS_CODIFICADO/",i,"_VENTAS.csv"))
  }

  H_VENTAS_COD <- subset(H_VENTAS_COD, select = -c(AUX))
  write.csv2(H_VENTAS_COD, paste0(dataset_nombre,"/VENTAS_CODIFICADO/TOTAL_VENTAS.csv"))

  message("5 de 6: Tablas de ventas codificadas exportadas...")

  #Exportar ventas decodificadas

  i <- min(H_VENTAS$AUX)

  for ( i in unique(H_VENTAS$AUX)) {
    EXPORT <- subset(H_VENTAS_DEC, AUX == i)
    EXPORT <- subset(EXPORT, select = -c(AUX))
    write.csv2(EXPORT, paste0(dataset_nombre,"/VENTAS_DECODIFICADO/",i,"_VENTAS.csv"))
  }

  H_VENTAS_DEC <- subset(H_VENTAS_DEC, select = -c(AUX))
  write.csv2(H_VENTAS_DEC, paste0(dataset_nombre,"/VENTAS_DECODIFICADO/TOTAL_VENTAS.csv"))
  message("6 de 6: Tablas de ventas decodificadas exportadas...")

  message("Fin: Archivos exportados con éxito :)")
}
