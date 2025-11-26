#  ==========================================================================
# MODELO DE TEORÍA DE JUEGOS PARA PENALTIS
  # =============================================================================

# Función para calcular equilibrio de Nash en estrategias mixtas
calcular_equilibrio_penalti <- function(matriz_pagos) {
  # matriz_pagos: matriz 2x2 con probabilidades de gol
  # Filas: Pateador (Arriba: Izquierda, Abajo: Derecha)
  # Columnas: Portero (Izquierda, Derecha)
  
  # Extraer los pagos
  a <- matriz_pagos[1, 1]  # Pateador I vs Portero I
  b <- matriz_pagos[1, 2]  # Pateador I vs Portero D  
  c <- matriz_pagos[2, 1]  # Pateador D vs Portero I
  d <- matriz_pagos[2, 2]  # Pateador D vs Portero D
  
  # ---------------------------------------------------------------------------
  # CALCULAR EQUILIBRIO DE NASH
  # ---------------------------------------------------------------------------
  
  # Probabilidad de que PORTERO se tire a la izquierda (q)
  q <- (d - b) / (a - b - c + d)
  
  # Probabilidad de que PATEADOR tire a la izquierda (p)
  p <- (d - c) / (a - b - c + d)
  
  # Validar que las probabilidades estén entre 0 y 1
  if (q < 0 | q > 1 | p < 0 | p > 1) {
    warning("No se encontró equilibrio en estrategias mixtas internas")
  }
  
  # ---------------------------------------------------------------------------
  # CALCULAR VALOR DEL JUEGO
  # ---------------------------------------------------------------------------
  
  valor_juego <- a * p * q + b * p * (1 - q) + c * (1 - p) * q + d * (1 - p) * (1 - q)
  
  # ---------------------------------------------------------------------------
  # RESULTADOS
  # ---------------------------------------------------------------------------
  
  resultados <- list(
    # Estrategias de equilibrio
    pateador = c(izquierda = p, derecha = 1 - p),
    portero = c(izquierda = q, derecha = 1 - q),
    
    # Valor del juego (probabilidad esperada de gol)
    valor_juego = valor_juego,
    
    # Matriz de pagos original
    matriz_pagos = matriz_pagos,
    
    # Verificación
    verificacion = list(
      pateador_indiferente = a * q + b * (1 - q) - (c * q + d * (1 - q)),
      portero_indiferente = a * p + c * (1 - p) - (b * p + d * (1 - p))
    )
  )
  
  return(resultados)
}

# =============================================================================
# EJEMPLO CON TUS DATOS
# =============================================================================

# Definir la matriz de pagos (tus datos)
matriz_ejemplo <- matrix(c(
  0.50, 0.80,   # Pateador Izquierda vs Portero (I, D)
  0.90, 0.20    # Pateador Derecha vs Portero (I, D)
), nrow = 2, byrow = TRUE,
dimnames = list(c("Pateador_Izq", "Pateador_Der"), 
                c("Portero_Izq", "Portero_Der")))

matriz_ejemplo
# Calcular el equilibrio
resultado <- calcular_equilibrio_penalti(matriz_ejemplo)
resultado

# =============================================================================
# VISUALIZACIÓN DE RESULTADOS
# =============================================================================

# Función para mostrar resultados bonitos
mostrar_resultados <- function(resultado) {
  cat("═" * 50, "\n")
  cat("EQUILIBRIO DE NASH - MODELO DE PENALTIS\n")
  cat("═" * 50, "\n\n")
  
  cat("MATRIZ DE PAGOS (Probabilidades de Gol):\n")
  print(round(resultado$matriz_pagos, 3))
  cat("\n")
  
  cat("ESTRATEGIAS ÓPTIMAS:\n")
  cat("PATEADOR:\n")
  cat(sprintf("  • Izquierda: %.1f%%\n", resultado$pateador["izquierda"] * 100))
  cat(sprintf("  • Derecha:  %.1f%%\n", resultado$pateador["derecha"] * 100))
  
  cat("PORTERO:\n")  
  cat(sprintf("  • Izquierda: %.1f%%\n", resultado$portero["izquierda"] * 100))
  cat(sprintf("  • Derecha:  %.1f%%\n", resultado$portero["derecha"] * 100))
  
  cat("\nVALOR DEL JUEGO:\n")
  cat(sprintf("Probabilidad esperada de gol: %.1f%%\n", resultado$valor_juego * 100))
  
  cat("\nVERIFICACIÓN (debe ser cercano a 0):\n")
  cat(sprintf("Indiferencia pateador: %.6f\n", resultado$verificacion$pateador_indiferente))
  cat(sprintf("Indiferencia portero: %.6f\n", resultado$verificacion$portero_indiferente))
}

# Mostrar resultados
mostrar_resultados(resultado)

# =============================================================================
# SIMULACIÓN DE PENALTIS
# =============================================================================

simular_penaltis <- function(resultado, n_simulaciones = 1000) {
  estrategia_pateador <- resultado$pateador
  estrategia_portero <- resultado$portero
  
  resultados <- data.frame(
    pateador = character(n_simulaciones),
    portero = character(n_simulaciones),
    gol = logical(n_simulaciones)
  )
  
  for (i in 1:n_simulaciones) {
    # Elecciones aleatorias según estrategias mixtas
    eleccion_pateador <- sample(c("Izquierda", "Derecha"), 1, 
                                prob = estrategia_pateador)
    eleccion_portero <- sample(c("Izquierda", "Derecha"), 1, 
                               prob = estrategia_portero)
    
    # Determinar si fue gol
    if (eleccion_pateador == "Izquierda" & eleccion_portero == "Izquierda") {
      prob_gol <- resultado$matriz_pagos[1, 1]
    } else if (eleccion_pateador == "Izquierda" & eleccion_portero == "Derecha") {
      prob_gol <- resultado$matriz_pagos[1, 2]
    } else if (eleccion_pateador == "Derecha" & eleccion_portero == "Izquierda") {
      prob_gol <- resultado$matriz_pagos[2, 1]
    } else {
      prob_gol <- resultado$matriz_pagos[2, 2]
    }
    
    gol <- runif(1) < prob_gol
    
    resultados[i, ] <- c(eleccion_pateador, eleccion_portero, gol)
  }
  
  return(resultados)
}

# Ejecutar simulación
cat("\n" + "═"*50 + "\n")
cat("SIMULACIÓN DE 1000 PENALTIS\n")
cat("═"*50 + "\n")

simulacion <- simular_penaltis(resultado, 1000)

# Resumen de la simulación
cat("RESUMEN DE SIMULACIÓN:\n")
cat(sprintf("Total goles: %d/1000 (%.1f%%)\n", 
            sum(simulacion$gol == TRUE), 
            mean(simulacion$gol == TRUE) * 100))

cat("Distribución de elecciones del pateador:\n")
print(prop.table(table(simulacion$pateador)))

cat("Distribución de elecciones del portero:\n")
print(prop.table(table(simulacion$portero)))
