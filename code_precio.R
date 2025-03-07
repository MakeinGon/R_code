  
  install.packages("readxl")  
  
  library(readxl)
  
  
  Datos <- read_excel("E:/UNAS MAKEIN/R/Precios_Automoviles.xlsx")
  
  
  View(Datos)  
  head(Datos)  
  
  
  
  ######Variables Cualitativas#############
  tablafrec1 <- table(Transmision = factor(Datos$Transmision))
  
  tablafrec2 <- transform(as.data.frame(tablafrec1),
                          hi = round(prop.table(Freq), 3),
                          hiPorc = round(prop.table(Freq) * 100, 3))
  
  print(tablafrec2)
  
  
  # Gráfico de barras con ggplot2
  ggplot(tablafrec2, aes(x = Transmision, y = Freq, fill = Transmision)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(hiPorc, "%")), vjust = -0.5) +  
    labs(title = "Distribución de Tipos de Transmisión", x = "Tipo de Transmisión", y = "Frecuencia") +
    theme_minimal()
  
  
  
  ##########################################################################################
  
  ######Variables discretas#############
  
  
  
  tablafrec1 <- table(Propietarios = factor(Datos$Numero_propietarios))  
  tablafrec2 <- transform(tablafrec1,
                          Fi = cumsum(Freq),  
                          hi = round(prop.table(Freq), 3),  
                          Hi = round(cumsum(prop.table(Freq)), 3),  
                          hiPorc = round(prop.table(Freq) * 100, 3),
                          HiPorc = round(cumsum(prop.table(Freq) * 100), 3))  
  
  print(tablafrec2)
  
  
  
  # 📌 Crear el gráfico de pastel (Pie Chart)
  pie(tablafrec1, labels = paste0(names(tablafrec1), " (", round(prop.table(tablafrec1) * 100, 1), "%)"),
      col = rainbow(length(tablafrec1)), main = "Distribución de Número de Propietarios")
  
  
  
  
  
  
  
  
  
  
  ##########################################################################################
  #Variable cuantitativas continua
  # 📌 Cargar librerías necesarias
  library(readxl)
  library(ggplot2)
  
  # 📌 Leer los datos de la columna "Precio"
  datos <- sort(Datos$Precio)
  
  # 📌 Función para calcular el número óptimo de intervalos usando la Regla de Sturges
  calcular_numero_intervalos <- function(datos) {
    n <- length(datos)
    k <- 1 + 3.322 * log10(n)
    return(round(k))
  }
  
  # 📌 Función para calcular la tabla de frecuencias
  calcular_tabla_frecuencias <- function(datos) {
    num_intervalos <- calcular_numero_intervalos(datos)
    rango <- max(datos) - min(datos)
    ancho_intervalo <- rango / num_intervalos
    intervalos <- matrix(0, nrow = num_intervalos, ncol = 2)
    inicio <- min(datos)
    
    for (i in 1:num_intervalos) {
      fin <- inicio + ancho_intervalo
      intervalos[i, ] <- c(inicio, fin)
      inicio <- fin
    }
    
    frecuencias <- numeric(num_intervalos)
    for (i in 1:num_intervalos) {
      frecuencias[i] <- sum(datos >= intervalos[i, 1] & datos < intervalos[i, 2])
    }
    
    marcas_clase <- rowMeans(intervalos)
    Fi <- cumsum(frecuencias)
    hi <- frecuencias / length(datos)
    Hi <- cumsum(hi)
    hi_porcentaje <- hi * 100
    Hi_porcentaje <- Hi * 100
    
    return(list(intervalos = intervalos, marcas_clase = marcas_clase, frecuencias = frecuencias, 
                Fi = Fi, hi = hi, Hi = Hi, hi_porcentaje = hi_porcentaje, Hi_porcentaje = Hi_porcentaje))
  }
  
  # 📌 Función para mostrar la tabla de frecuencias en formato tabular
  mostrar_tabla_frecuencias <- function(tabla) {
    cat("\nTabla de Frecuencias para Precio:\n")
    cat("+------------+----------+----+-----+-----+-----+----+----+\n")
    cat("| [Li – Ls]  |   Xi    | fi |  Fi |  hi |  Hi | hi% | Hi% |\n")
    cat("+------------+----------+----+-----+-----+-----+----+----+\n")
    
    for (i in 1:nrow(tabla$intervalos)) {
      cat(sprintf("| %5.2f - %5.2f | %7.2f | %3d | %4d | %4.2f | %4.2f | %3d%% | %3d%% |\n",
                  tabla$intervalos[i, 1], tabla$intervalos[i, 2], tabla$marcas_clase[i], 
                  tabla$frecuencias[i], tabla$Fi[i], tabla$hi[i], tabla$Hi[i], 
                  round(tabla$hi_porcentaje[i]), round(tabla$Hi_porcentaje[i])))
    }
    
    cat("+------------+----------+----+-----+-----+-----+----+----+\n")
    cat(sprintf("| Total:     |          | %4d |     | 1.00 |     | 100%% |     |\n",
                sum(tabla$frecuencias)))
    cat("+------------+----------+----+-----+-----+-----+----+----+\n")
  }
  
  # 📌 Gráfico de Barras mejorado con ggplot2
  graficar_barras <- function(tabla) {
    intervalos <- apply(tabla$intervalos, 1, function(x) sprintf("[%5.2f – %5.2f)", x[1], x[2]))
    df <- data.frame(intervalos, frecuencias = tabla$frecuencias)
    
    ggplot(df, aes(x = intervalos, y = frecuencias, fill = intervalos)) +
      geom_col(show.legend = FALSE) +  # Usa geom_col() en lugar de geom_bar()
      geom_text(aes(label = frecuencias), vjust = -0.5, color = "black", size = 4) +  # Etiquetas en las barras
      labs(x = "Intervalos de Precio", y = "Frecuencia (fi)", 
           title = "Distribución de Precios de Automóviles") +
      theme_minimal() +  # Diseño más limpio
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))  # Mejor rotación de etiquetas
  }
  
  # 📌 Gráfico de Barras con Ojiva
  graficar_barras_con_ojiva <- function(tabla) {
    intervalos <- apply(tabla$intervalos, 1, function(x) sprintf("[%5.2f – %5.2f)", x[1], x[2]))
    df <- data.frame(intervalos, frecuencias = tabla$frecuencias, Fi = tabla$Fi)
    
    barras <- barplot(df$frecuencias, names.arg = df$intervalos, 
                      col = "azure", main = "Distribución de Precios con Ojiva",
                      xlab = "Intervalos de Precio", ylab = "Frecuencia",
                      sub = "Figura: Distribución de precios de automóviles.",
                      ylim = c(0, max(df$Fi) * 1.1))
    
    # 📌 Agregar etiquetas con los valores de las frecuencias en las barras
    text(x = barras, y = df$frecuencias, pos = 3, cex = 1.2, col = "red",
         label = round(df$frecuencias, 4))
    
    # 📌 Dibujar la ojiva (línea de frecuencia acumulada)
    lines(barras, df$Fi, type = "o", lwd = 3, col = "green")
  }
  
  # 📌 Función Principal para ejecutar todo el análisis
  main <- function() {
    tabla <- calcular_tabla_frecuencias(datos)
    mostrar_tabla_frecuencias(tabla)
    graficar_barras(tabla)  # Genera el gráfico de barras mejorado
    graficar_barras_con_ojiva(tabla)  # Genera el gráfico de barras con ojiva
  }
  
  # 📌 Ejecutar la función principal
  main()
  


