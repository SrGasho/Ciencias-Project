# Proyecto Ciencias II — Asignación de salones con matching bipartito y coloreo de grafos
.libPaths(c("~/R/library", .libPaths()))

# Captura el directorio del script para guardar datasets junto a él,
# independientemente del directorio de trabajo activo al ejecutar.
script_dir <- tryCatch({
  args     <- commandArgs(trailingOnly = FALSE)
  file_arg <- Filter(function(x) grepl("--file=", x), args)
  if (length(file_arg) > 0) {
    dirname(normalizePath(sub("--file=", "", file_arg[1]), mustWork = FALSE))
  } else {
    getwd()
  }
}, error = function(e) getwd())

library(shiny)
library(bslib)
library(dplyr)
library(igraph)
library(DT)

if (!requireNamespace("visNetwork", quietly = TRUE)) {
  install.packages("visNetwork", repos = "https://cloud.r-project.org")
}
library(visNetwork)

# --- Generación de datos simulados ---

generar_salones <- function(num_salones, tipo_escenario) {

  # Demanda alta → capacidades más ajustadas para hacer el problema más difícil.
  if (tipo_escenario == "Demanda baja") {
    capacidades <- sample(c(35, 40, 45, 50), num_salones, replace = TRUE)
  } else if (tipo_escenario == "Demanda media") {
    capacidades <- sample(c(25, 30, 35, 40, 45), num_salones, replace = TRUE)
  } else {
    capacidades <- sample(c(20, 25, 30, 35, 40), num_salones, replace = TRUE)
  }

  tipo_salon <- sample(
    c("Aula", "Laboratorio"),
    num_salones,
    replace = TRUE,
    prob = c(0.65, 0.35)
  )

  salones <- data.frame(
    id_salon = paste0("S", 1:num_salones),
    salon = ifelse(
      tipo_salon == "Laboratorio",
      paste0("Lab ", 200 + seq_len(num_salones)),
      paste0("Aula ", 100 + seq_len(num_salones))
    ),
    capacidad = capacidades,
    tipo_salon = tipo_salon,
    stringsAsFactors = FALSE
  )

  return(salones)
}

generar_clases <- function(num_clases, nivel_cruces) {

  asignaturas_base <- c(
    "Cálculo I", "Cálculo II", "Álgebra Lineal", "Física I",
    "Programación I", "Programación II", "Bases de Datos",
    "Estructuras de Datos", "Redes", "Probabilidad",
    "Ingeniería de Software", "Sistemas Operativos",
    "Arquitectura de Computadores", "Métodos Numéricos",
    "Matemáticas Discretas", "Estadística", "Teoría de Grafos",
    "Investigación de Operaciones", "Seguridad Informática"
  )

  dias <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes")

  bloques <- data.frame(
    hora_inicio = c("06:00", "08:00", "10:00", "12:00", "14:00", "16:00", "18:00"),
    hora_fin    = c("08:00", "10:00", "12:00", "14:00", "16:00", "18:00", "20:00"),
    stringsAsFactors = FALSE
  )

  clases <- data.frame(
    id_clase = paste0("C", 1:num_clases),
    asignatura = paste(
      sample(asignaturas_base, num_clases, replace = TRUE),
      paste0("G", sample(1:4, num_clases, replace = TRUE))
    ),
    dia = character(num_clases),
    hora_inicio = character(num_clases),
    hora_fin = character(num_clases),
    estudiantes = sample(20:45, num_clases, replace = TRUE),
    tipo_requerido = sample(
      c("Aula", "Laboratorio"),
      num_clases,
      replace = TRUE,
      prob = c(0.7, 0.3)
    ),
    profesor = character(num_clases),
    semestre = integer(num_clases),
    stringsAsFactors = FALSE
  )

  if (nivel_cruces == "Sin cruces académicos") {

    # Distribuye clases en combinaciones únicas de día-bloque-semestre
    # para que el grafo de coloreo quede sin aristas.
    combinaciones <- expand.grid(
      dia = dias,
      bloque = 1:nrow(bloques),
      semestre = 1:10,
      stringsAsFactors = FALSE
    )

    combinaciones <- combinaciones[sample(nrow(combinaciones)), ]

    for (i in 1:num_clases) {
      fila <- combinaciones[i, ]

      clases$dia[i] <- fila$dia
      clases$hora_inicio[i] <- bloques$hora_inicio[fila$bloque]
      clases$hora_fin[i] <- bloques$hora_fin[fila$bloque]
      clases$profesor[i] <- paste0("P", i)
      clases$semestre[i] <- fila$semestre
    }

  } else if (nivel_cruces == "Cruces moderados") {

    # Cada cuarta clase comparte franja fija para garantizar
    # al menos algunos conflictos visibles en el grafo.
    for (i in 1:num_clases) {

      if (i %% 4 == 0) {
        clases$dia[i] <- "Lunes"
        clases$hora_inicio[i] <- "08:00"
        clases$hora_fin[i] <- "10:00"
        clases$profesor[i] <- paste0("P", sample(1:4, 1))
        clases$semestre[i] <- sample(1:4, 1)
      } else {
        b <- sample(1:nrow(bloques), 1)
        clases$dia[i] <- sample(dias, 1)
        clases$hora_inicio[i] <- bloques$hora_inicio[b]
        clases$hora_fin[i] <- bloques$hora_fin[b]
        clases$profesor[i] <- paste0("P", sample(1:8, 1))
        clases$semestre[i] <- sample(1:8, 1)
      }
    }

    # Refuerzo controlado para asegurar al menos un par por profesor
    # y un par por semestre en el escenario moderado.
    if (num_clases >= 4) {
      clases$dia[1:4] <- "Lunes"
      clases$hora_inicio[1:4] <- "08:00"
      clases$hora_fin[1:4] <- "10:00"
      clases$profesor[1:2] <- "P1"
      clases$semestre[3:4] <- 3
    }

  } else {

    # Concentra todas las clases en tres franjas para producir
    # un grafo denso que evalúe el coloreo bajo alta presión.
    franjas_concentradas <- data.frame(
      dia = c("Lunes", "Lunes", "Martes"),
      hora_inicio = c("08:00", "10:00", "08:00"),
      hora_fin = c("10:00", "12:00", "10:00"),
      stringsAsFactors = FALSE
    )

    for (i in 1:num_clases) {
      franja <- franjas_concentradas[sample(1:nrow(franjas_concentradas), 1), ]

      clases$dia[i] <- franja$dia
      clases$hora_inicio[i] <- franja$hora_inicio
      clases$hora_fin[i] <- franja$hora_fin
      clases$profesor[i] <- paste0("P", sample(1:4, 1))
      clases$semestre[i] <- sample(1:4, 1)
    }
  }

  return(clases)
}

# --- Funciones auxiliares ---

hora_a_minutos <- function(hora) {
  partes <- strsplit(hora, ":")[[1]]
  as.numeric(partes[1]) * 60 + as.numeric(partes[2])
}

hay_cruce_horario <- function(inicio1, fin1, inicio2, fin2) {
  # Dos intervalos [a,b) y [c,d) se solapan si y solo si a < d && c < b.
  i1 <- hora_a_minutos(inicio1)
  f1 <- hora_a_minutos(fin1)
  i2 <- hora_a_minutos(inicio2)
  f2 <- hora_a_minutos(fin2)

  return(i1 < f2 && i2 < f1)
}

crear_recursos <- function(clases, salones) {

  bloques <- clases %>%
    select(dia, hora_inicio, hora_fin) %>%
    distinct()

  # Producto cartesiano salones × bloques: cada recurso es
  # una combinación única de salón + día + hora.
  salones$key <- 1
  bloques$key <- 1

  recursos <- merge(salones, bloques, by = "key") %>%
    select(-key)

  recursos$id_recurso <- paste(
    recursos$id_salon,
    recursos$dia,
    recursos$hora_inicio,
    recursos$hora_fin,
    sep = "_"
  )

  recursos$nombre_recurso <- paste0(
    recursos$salon, "\n",
    recursos$dia, "\n",
    recursos$hora_inicio, "-", recursos$hora_fin
  )

  return(recursos)
}

# --- Grafo bipartito y matching ---

crear_aristas_compatibilidad <- function(clases, recursos, usar_capacidad, usar_tipo) {

  aristas <- data.frame(
    from = character(),
    to = character(),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(clases)) {
    for (j in 1:nrow(recursos)) {

      misma_franja <- clases$dia[i] == recursos$dia[j] &&
        clases$hora_inicio[i] == recursos$hora_inicio[j] &&
        clases$hora_fin[i] == recursos$hora_fin[j]

      capacidad_ok <- TRUE
      tipo_ok <- TRUE

      if (usar_capacidad) {
        capacidad_ok <- recursos$capacidad[j] >= clases$estudiantes[i]
      }

      if (usar_tipo) {
        tipo_ok <- recursos$tipo_salon[j] == clases$tipo_requerido[i]
      }

      if (misma_franja && capacidad_ok && tipo_ok) {
        aristas <- rbind(
          aristas,
          data.frame(
            from = clases$id_clase[i],
            to = recursos$id_recurso[j],
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  return(aristas)
}

matching_bipartito <- function(clases, recursos, aristas) {

  ids_clases <- clases$id_clase
  ids_recursos <- recursos$id_recurso

  adyacencia <- lapply(ids_clases, function(clase) {
    aristas$to[aristas$from == clase]
  })

  names(adyacencia) <- ids_clases

  # NA indica que el recurso está disponible.
  match_recurso <- rep(NA, length(ids_recursos))
  names(match_recurso) <- ids_recursos

  # Búsqueda de camino aumentante: si un recurso está ocupado,
  # intenta reubicar la clase asignada antes de rechazar la actual.
  buscar_camino <- function(clase, visitados) {

    for (recurso in adyacencia[[clase]]) {

      if (!visitados[recurso]) {

        visitados[recurso] <- TRUE

        if (is.na(match_recurso[recurso]) ||
            buscar_camino(match_recurso[recurso], visitados)) {

          match_recurso[recurso] <<- clase
          return(TRUE)
        }
      }
    }

    return(FALSE)
  }

  for (clase in ids_clases) {
    visitados <- rep(FALSE, length(ids_recursos))
    names(visitados) <- ids_recursos
    buscar_camino(clase, visitados)
  }

  asignaciones <- data.frame(
    id_recurso = names(match_recurso),
    id_clase = as.character(match_recurso),
    stringsAsFactors = FALSE
  ) %>%
    filter(!is.na(id_clase))

  resultado <- clases %>%
    left_join(asignaciones, by = "id_clase") %>%
    left_join(recursos, by = "id_recurso") %>%
    mutate(
      estado = ifelse(is.na(id_recurso), "No asignada", "Asignada")
    ) %>%
    select(
      id_clase,
      asignatura,
      dia = dia.x,
      hora_inicio = hora_inicio.x,
      hora_fin = hora_fin.x,
      estudiantes,
      tipo_requerido,
      profesor,
      semestre,
      id_recurso,
      salon,
      capacidad,
      tipo_salon,
      estado
    )

  return(resultado)
}

# --- Grafo simple no dirigido para coloreo ---

crear_aristas_coloreo <- function(clases, usar_profesor, usar_semestre) {

  aristas_coloreo <- data.frame(
    from = character(),
    to = character(),
    criterio = character(),
    stringsAsFactors = FALSE
  )

  if (nrow(clases) < 2) {
    return(aristas_coloreo)
  }

  for (i in 1:(nrow(clases) - 1)) {
    for (j in (i + 1):nrow(clases)) {

      mismo_dia <- clases$dia[i] == clases$dia[j]

      cruce <- hay_cruce_horario(
        clases$hora_inicio[i],
        clases$hora_fin[i],
        clases$hora_inicio[j],
        clases$hora_fin[j]
      )

      mismo_profesor <- usar_profesor && clases$profesor[i] == clases$profesor[j]
      mismo_semestre <- usar_semestre && clases$semestre[i] == clases$semestre[j]

      # Solo hay conflicto si se cruzan en horario Y comparten
      # profesor o semestre (criterio académico, no solo temporal).
      if (mismo_dia && cruce && (mismo_profesor || mismo_semestre)) {

        criterio <- ifelse(
          mismo_profesor && mismo_semestre,
          "Mismo profesor y mismo semestre",
          ifelse(mismo_profesor, "Mismo profesor", "Mismo semestre")
        )

        aristas_coloreo <- rbind(
          aristas_coloreo,
          data.frame(
            from = clases$id_clase[i],
            to = clases$id_clase[j],
            criterio = criterio,
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  return(aristas_coloreo)
}

crear_grafo_coloreo <- function(clases, aristas_coloreo) {

  vertices <- data.frame(
    name = clases$id_clase,
    label = paste0(clases$id_clase, "\n", clases$asignatura),
    stringsAsFactors = FALSE
  )

  # Sin aristas el grafo sigue necesitando todos los vértices
  # para que el coloreo asigne color 1 a todas las clases.
  if (nrow(aristas_coloreo) == 0) {
    aristas_vacias <- data.frame(
      from = character(),
      to = character(),
      stringsAsFactors = FALSE
    )

    grafo <- graph_from_data_frame(
      d = aristas_vacias,
      vertices = vertices,
      directed = FALSE
    )
  } else {
    grafo <- graph_from_data_frame(
      d = aristas_coloreo[, c("from", "to")],
      vertices = vertices,
      directed = FALSE
    )
  }

  return(grafo)
}

coloreo_greedy <- function(grafo) {

  nodos <- V(grafo)$name
  colores <- rep(NA, length(nodos))
  names(colores) <- nodos

  # Orden descendente por grado: los vértices más conectados
  # se colorean primero para minimizar el número de colores usados.
  grados <- degree(grafo)
  orden <- names(sort(grados, decreasing = TRUE))

  for (nodo in orden) {

    vecinos <- neighbors(grafo, nodo)
    nombres_vecinos <- V(grafo)[vecinos]$name
    colores_vecinos <- colores[nombres_vecinos]
    colores_usados <- colores_vecinos[!is.na(colores_vecinos)]

    color <- 1

    while (color %in% colores_usados) {
      color <- color + 1
    }

    colores[nodo] <- color
  }

  return(colores)
}

# --- Interfaz Shiny ---

ui <- page_sidebar(

  title = "Optimización de asignación de salones mediante grafos",

  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2C3E50",
    secondary = "#18BC9C"
  ),

  tags$head(
    tags$style(HTML("
      body {
        overflow-x: hidden;
        background-color: #F7F9FB;
      }

      .card {
        margin-bottom: 18px;
        border-radius: 16px;
      }

      .bslib-card {
        overflow: visible;
      }

      .shiny-plot-output {
        width: 100% !important;
        min-height: 620px;
      }

      .dataTables_wrapper {
        font-size: 14px;
      }

      .sidebar {
        font-size: 14px;
      }

      .btn {
        width: 100%;
        border-radius: 10px;
      }

      .form-control, .form-select {
        font-size: 14px;
      }
    "))
  ),

  sidebar = sidebar(
    width = 250,

    h4("Configuración del escenario"),

    selectInput(
      "tipo_escenario",
      "Tipo de escenario:",
      choices = c("Demanda baja", "Demanda media", "Demanda alta", "Personalizado"),
      selected = "Demanda media"
    ),

    selectInput(
      "nivel_cruces",
      "Nivel de cruces académicos:",
      choices = c("Sin cruces académicos", "Cruces moderados", "Cruces altos"),
      selected = "Cruces moderados"
    ),

    conditionalPanel(
      condition = "input.tipo_escenario == 'Personalizado'",

      numericInput(
        "num_clases",
        "Número de clases:",
        value = 15,
        min = 2,
        max = 60
      ),

      numericInput(
        "num_salones",
        "Número de salones:",
        value = 6,
        min = 1,
        max = 25
      )
    ),

    numericInput(
      "semilla",
      "Semilla para reproducibilidad:",
      value = 123,
      min = 1,
      max = 9999
    ),

    hr(),

    h5("Restricciones para matching"),

    checkboxInput(
      "usar_capacidad",
      "Considerar capacidad del salón",
      value = TRUE
    ),

    checkboxInput(
      "usar_tipo",
      "Considerar tipo de salón",
      value = TRUE
    ),

    hr(),

    h5("Criterios para coloreo"),

    checkboxInput(
      "usar_profesor",
      "Separar clases con el mismo profesor",
      value = TRUE
    ),

    checkboxInput(
      "usar_semestre",
      "Separar clases del mismo semestre",
      value = TRUE
    ),

    hr(),

    actionButton(
      "generar",
      "Generar escenario",
      class = "btn-success"
    ),

    br(), br(),

    actionButton(
      "ejecutar",
      "Ejecutar algoritmos",
      class = "btn-primary"
    ),

    hr(),

    p("Flujo recomendado: primero generar el escenario y luego ejecutar los algoritmos.")
  ),

  navset_card_tab(

    nav_panel(
      "Inicio",

      layout_columns(
        card(
          card_header("Problema elegido"),
          p("Se busca asignar salones a clases de la Facultad de Ingeniería considerando horario, capacidad y tipo de salón."),
          p("El proyecto usa técnicas de grafos para proponer una asignación y analizar la separación de clases que no deberían quedar en el mismo bloque académico.")
        ),

        card(
          card_header("Técnicas de grafos usadas"),
          tags$ul(
            tags$li("Grafo bipartito clase-recurso."),
            tags$li("Matching bipartito para asignación."),
            tags$li("Grafo simple no dirigido para coloreo."),
            tags$li("Coloreo greedy de vértices.")
          )
        )
      ),

      card(
        card_header("Interpretación"),
        p("El grafo bipartito conecta clases con recursos compatibles. Un recurso representa un salón en un día y bloque horario específico."),
        p("El grafo simple para coloreo conecta clases que deben separarse porque coinciden en horario y comparten profesor o semestre.")
      )
    ),

    nav_panel(
      "Datos",

      layout_columns(
        card(
          card_header("Clases generadas"),
          DTOutput("tabla_clases")
        ),

        card(
          card_header("Salones generados"),
          DTOutput("tabla_salones")
        ),
        col_widths = c(8, 4)
      )
    ),

    nav_panel(
      "Asignación",

      card(
        card_header("Resultado del matching bipartito"),
        p("Esta tabla muestra el salón asignado a cada clase. Cada clase puede quedar asignada a máximo un recurso."),
        DTOutput("tabla_asignacion")
      )
    ),

    nav_panel(
      "Métricas",

      layout_columns(
        value_box(
          title = "Total de clases",
          value = textOutput("vb_total"),
          showcase = "",
          theme = "primary"
        ),

        value_box(
          title = "Asignadas",
          value = textOutput("vb_asignadas"),
          showcase = "",
          theme = "success"
        ),

        value_box(
          title = "Sin asignar",
          value = textOutput("vb_no_asignadas"),
          showcase = "",
          theme = "warning"
        ),

        value_box(
          title = "Porcentaje",
          value = textOutput("vb_porcentaje"),
          showcase = "",
          theme = "info"
        )
      ),

      card(
        card_header("Resumen cuantitativo"),
        tableOutput("metricas")
      )
    ),

    nav_panel(
      "Grafo bipartito",

      card(
        card_header("Grafo bipartito clase-recurso"),

        p("Esta visualización corresponde al grafo usado para el matching bipartito."),

        radioButtons(
          "tipo_grafo_bipartito",
          "Tipo de visualización:",
          choices = c("Asignación final", "Compatibilidades posibles"),
          selected = "Asignación final",
          inline = TRUE
        ),

        visNetworkOutput("grafo_bipartito", height = "700px", width = "100%")
      )
    ),

    nav_panel(
      "Grafo para coloreo",

      navset_card_tab(

        nav_panel(
          "Visualización",

          card(
            card_header("Grafo simple no dirigido para coloreo"),
            p("Cada nodo representa una clase. Una arista indica que dos clases deben quedar con colores diferentes."),

            actionButton(
              "ver_grafo_coloreo_grande",
              "Ver grafo en grande",
              class = "btn-primary"
            ),

            br(), br(),

            visNetworkOutput("grafo_coloreo", height = "600px", width = "100%")
          )
        ),

        nav_panel(
          "Aristas",

          card(
            card_header("Aristas usadas para el coloreo"),
            p("Esta tabla muestra las parejas de clases que se conectan en el grafo simple no dirigido."),
            DTOutput("tabla_aristas_coloreo")
          )
        ),

        nav_panel(
          "Colores",

          card(
            card_header("Resultado del coloreo greedy"),
            p("Esta tabla muestra el color asignado a cada clase después de aplicar el algoritmo greedy."),
            DTOutput("tabla_colores")
          )
        )
      )
    ),

    nav_panel(
      "Matemáticas",

      card(
        card_header("Grafo bipartito"),
        p("El problema de asignación se modela como un grafo bipartito G = (C ∪ R, E)."),
        tags$ul(
          tags$li("C representa el conjunto de clases."),
          tags$li("R representa el conjunto de recursos: salón + día + hora."),
          tags$li("E representa las aristas de compatibilidad.")
        ),
        p("Existe una arista entre una clase y un recurso cuando el recurso cumple las restricciones de horario, capacidad y tipo de salón.")
      ),

      card(
        card_header("Matching bipartito"),
        p("El matching busca un subconjunto de aristas donde ningún nodo se repite."),
        p("En el proyecto esto significa que una clase no recibe dos salones y un recurso no se usa para dos clases al mismo tiempo."),
        p("El objetivo es maximizar la cantidad de clases asignadas.")
      ),

      card(
        card_header("Coloreo de grafos"),
        p("El segundo modelo es un grafo simple no dirigido sobre las clases."),
        p("Dos clases se conectan si deben separarse por coincidir en horario y compartir profesor o semestre."),
        p("El coloreo greedy asigna colores a los vértices procurando que vértices adyacentes tengan colores diferentes.")
      )
    )
  )
)

# --- Servidor ---

server <- function(input, output, session) {

  datos <- reactiveValues(
    clases = NULL,
    salones = NULL,
    resultados = NULL
  )

  observeEvent(input$generar, {

    if (is.na(input$semilla) || input$semilla < 1) {
      showNotification(
        "La semilla debe ser un número mayor o igual a 1.",
        type = "error"
      )
      return()
    }

    if (input$tipo_escenario == "Personalizado") {

      if (is.na(input$num_clases) || input$num_clases < 2) {
        showNotification(
          "El número de clases debe ser mayor o igual a 2.",
          type = "error"
        )
        return()
      }

      if (is.na(input$num_salones) || input$num_salones < 1) {
        showNotification(
          "El número de salones debe ser mayor o igual a 1.",
          type = "error"
        )
        return()
      }

      if (input$num_clases > 60) {
        showNotification(
          "Para esta demo, el número máximo recomendado de clases es 60.",
          type = "error"
        )
        return()
      }

      if (input$num_salones > 25) {
        showNotification(
          "Para esta demo, el número máximo recomendado de salones es 25.",
          type = "error"
        )
        return()
      }
    }

    set.seed(input$semilla)

    if (input$tipo_escenario == "Demanda baja") {
      num_clases <- 10
      num_salones <- 8
    } else if (input$tipo_escenario == "Demanda media") {
      num_clases <- 18
      num_salones <- 7
    } else if (input$tipo_escenario == "Demanda alta") {
      num_clases <- 30
      num_salones <- 6
    } else {
      num_clases <- input$num_clases
      num_salones <- input$num_salones
    }

    datos$clases <- generar_clases(
      num_clases = num_clases,
      nivel_cruces = input$nivel_cruces
    )

    datos$salones <- generar_salones(
      num_salones = num_salones,
      tipo_escenario = input$tipo_escenario
    )

    tryCatch({
      datasets_dir <- file.path(script_dir, "datasets")
      dir.create(datasets_dir, showWarnings = FALSE, recursive = TRUE)
      write.csv(datos$clases,  file.path(datasets_dir, "dataset_clases_simuladas.csv"),  row.names = FALSE)
      write.csv(datos$salones, file.path(datasets_dir, "dataset_salones_simulados.csv"), row.names = FALSE)
    }, error = function(e) NULL)

    datos$resultados <- NULL

    showNotification(
      paste("Escenario generado:", num_clases, "clases y", num_salones, "salones."),
      type = "message"
    )
  })

  observeEvent(input$ejecutar, {

    req(datos$clases)
    req(datos$salones)

    tiempo_inicio <- Sys.time()

    recursos <- crear_recursos(datos$clases, datos$salones)

    aristas <- crear_aristas_compatibilidad(
      clases = datos$clases,
      recursos = recursos,
      usar_capacidad = input$usar_capacidad,
      usar_tipo = input$usar_tipo
    )

    asignacion <- matching_bipartito(
      clases = datos$clases,
      recursos = recursos,
      aristas = aristas
    )

    aristas_coloreo <- crear_aristas_coloreo(
      clases = datos$clases,
      usar_profesor = input$usar_profesor,
      usar_semestre = input$usar_semestre
    )

    grafo_coloreo <- crear_grafo_coloreo(
      clases = datos$clases,
      aristas_coloreo = aristas_coloreo
    )

    colores <- coloreo_greedy(grafo_coloreo)

    tiempo_fin <- Sys.time()

    datos$resultados <- list(
      recursos = recursos,
      aristas = aristas,
      asignacion = asignacion,
      aristas_coloreo = aristas_coloreo,
      grafo_coloreo = grafo_coloreo,
      colores = colores,
      tiempo = round(as.numeric(tiempo_fin - tiempo_inicio, units = "secs"), 5)
    )

    showNotification(
      "Algoritmos ejecutados correctamente: matching bipartito y coloreo greedy.",
      type = "message"
    )
  })

  output$tabla_clases <- renderDT({
    req(datos$clases)
    datatable(datos$clases, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$tabla_salones <- renderDT({
    req(datos$salones)
    datatable(datos$salones, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$tabla_asignacion <- renderDT({
    req(datos$resultados)
    datatable(datos$resultados$asignacion, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })

  metricas_reactivas <- reactive({
    req(datos$resultados)

    asignacion <- datos$resultados$asignacion

    total_clases <- nrow(asignacion)
    clases_asignadas <- sum(asignacion$estado == "Asignada")
    clases_no_asignadas <- sum(asignacion$estado == "No asignada")
    porcentaje_asignacion <- round((clases_asignadas / total_clases) * 100, 2)
    aristas_coloreo <- nrow(datos$resultados$aristas_coloreo)
    colores_usados <- length(unique(datos$resultados$colores))
    tiempo <- datos$resultados$tiempo

    data.frame(
      Metrica = c(
        "Total de clases",
        "Clases asignadas",
        "Clases no asignadas",
        "Porcentaje de asignación",
        "Aristas en el grafo para coloreo",
        "Colores usados",
        "Tiempo de ejecución"
      ),
      Valor = c(
        total_clases,
        clases_asignadas,
        clases_no_asignadas,
        paste0(porcentaje_asignacion, "%"),
        aristas_coloreo,
        colores_usados,
        paste0(tiempo, " segundos")
      )
    )
  })

  output$vb_total <- renderText({
    req(metricas_reactivas())
    metricas_reactivas()$Valor[1]
  })

  output$vb_asignadas <- renderText({
    req(metricas_reactivas())
    metricas_reactivas()$Valor[2]
  })

  output$vb_no_asignadas <- renderText({
    req(metricas_reactivas())
    metricas_reactivas()$Valor[3]
  })

  output$vb_porcentaje <- renderText({
    req(metricas_reactivas())
    metricas_reactivas()$Valor[4]
  })

  output$metricas <- renderTable({
    req(metricas_reactivas())
    metricas_reactivas()
  })

  output$grafo_bipartito <- renderVisNetwork({

    req(datos$resultados)

    clases     <- datos$clases
    recursos   <- datos$resultados$recursos
    aristas    <- datos$resultados$aristas
    asignacion <- datos$resultados$asignacion

    if (input$tipo_grafo_bipartito == "Asignación final") {

      aristas_vis <- asignacion %>%
        filter(estado == "Asignada") %>%
        select(from = id_clase, to = id_recurso)

      if (nrow(aristas_vis) == 0) {
        return(visNetwork(
          nodes = data.frame(id = 1, label = "Sin asignaciones. Desactivar restricciones o aumentar salones.", shape = "text"),
          edges = data.frame(from = integer(), to = integer())
        ))
      }

      clases_vis   <- clases %>% filter(id_clase %in% aristas_vis$from)
      recursos_vis <- recursos %>% filter(id_recurso %in% aristas_vis$to)
      edge_color   <- "#2C3E50"
      edge_width   <- 2.5

    } else {

      if (nrow(aristas) == 0) {
        return(visNetwork(
          nodes = data.frame(id = 1, label = "Sin compatibilidades. Desactivar restricciones de capacidad o tipo.", shape = "text"),
          edges = data.frame(from = integer(), to = integer())
        ))
      }

      clases_vis   <- clases %>% filter(id_clase %in% aristas$from)
      recursos_vis <- recursos %>% filter(id_recurso %in% aristas$to)
      aristas_vis  <- aristas
      edge_color   <- "#aaaaaa"
      edge_width   <- 0.8
    }

    n_c <- nrow(clases_vis)
    n_r <- nrow(recursos_vis)

    nodes_clases <- data.frame(
      id    = clases_vis$id_clase,
      label = clases_vis$id_clase,
      title = paste0(
        "<b>", clases_vis$id_clase, "</b><br>",
        clases_vis$asignatura, "<br>",
        "Estudiantes: ", clases_vis$estudiantes, "<br>",
        "Tipo requerido: ", clases_vis$tipo_requerido, "<br>",
        "Profesor: ", clases_vis$profesor, "<br>",
        "Semestre: ", clases_vis$semestre, "<br>",
        clases_vis$dia, " ", clases_vis$hora_inicio, "–", clases_vis$hora_fin
      ),
      group = "Clase",
      x     = rep(-400, n_c),
      y     = seq(-n_c * 60, n_c * 60, length.out = n_c),
      stringsAsFactors = FALSE
    )

    nodes_recursos <- data.frame(
      id    = recursos_vis$id_recurso,
      label = recursos_vis$salon,
      title = paste0(
        "<b>", recursos_vis$salon, "</b><br>",
        recursos_vis$dia, " ", recursos_vis$hora_inicio, "–", recursos_vis$hora_fin, "<br>",
        "Capacidad: ", recursos_vis$capacidad, "<br>",
        "Tipo: ", recursos_vis$tipo_salon
      ),
      group = "Recurso",
      x     = rep(400, n_r),
      y     = seq(-n_r * 60, n_r * 60, length.out = n_r),
      stringsAsFactors = FALSE
    )

    nodes <- rbind(nodes_clases, nodes_recursos)
    edges <- data.frame(
      from  = aristas_vis$from,
      to    = aristas_vis$to,
      color = edge_color,
      width = edge_width,
      stringsAsFactors = FALSE
    )

    visNetwork(nodes, edges) %>%
      visGroups(groupname = "Clase",
                color = list(background = "#5DADE2", border = "#2980B9"),
                shape = "ellipse") %>%
      visGroups(groupname = "Recurso",
                color = list(background = "#58D68D", border = "#27AE60"),
                shape = "box") %>%
      visPhysics(enabled = FALSE) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)) %>%
      visLegend(position = "right", main = "Leyenda") %>%
      visInteraction(zoomView = TRUE, dragView = TRUE)
  })

  grafo_coloreo_vis <- reactive({
    req(datos$resultados)

    clases          <- datos$clases
    colores         <- datos$resultados$colores
    aristas_coloreo <- datos$resultados$aristas_coloreo

    num_colores <- max(colores, na.rm = TRUE)
    paleta      <- hcl.colors(num_colores, "Set 3")

    idx <- match(names(colores), clases$id_clase)

    nodes <- data.frame(
      id    = names(colores),
      label = names(colores),
      title = paste0(
        "<b>", names(colores), "</b><br>",
        clases$asignatura[idx], "<br>",
        "Profesor: ", clases$profesor[idx], "<br>",
        "Semestre: ", clases$semestre[idx], "<br>",
        clases$dia[idx], " ", clases$hora_inicio[idx], "–", clases$hora_fin[idx], "<br>",
        "Color asignado: ", colores
      ),
      color = paleta[colores],
      size  = 20,
      group = paste("Color", colores),
      stringsAsFactors = FALSE
    )

    edges <- if (nrow(aristas_coloreo) > 0) {
      data.frame(
        from  = aristas_coloreo$from,
        to    = aristas_coloreo$to,
        color = "#666666",
        width = 2,
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(from = character(), to = character(),
                 color = character(), width = numeric(),
                 stringsAsFactors = FALSE)
    }

    list(nodes = nodes, edges = edges, num_colores = num_colores, paleta = paleta)
  })

  construir_vis_coloreo <- function(d) {
    net <- visNetwork(d$nodes, d$edges) %>%
      visPhysics(solver = "forceAtlas2Based", stabilization = TRUE) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)) %>%
      visInteraction(zoomView = TRUE, dragView = TRUE)

    for (i in seq_len(d$num_colores)) {
      net <- net %>% visGroups(groupname = paste("Color", i), color = d$paleta[i])
    }

    net %>% visLegend(position = "right", main = "Colores")
  }

  output$grafo_coloreo <- renderVisNetwork({
    construir_vis_coloreo(grafo_coloreo_vis())
  })

  observeEvent(input$ver_grafo_coloreo_grande, {

    showModal(
      modalDialog(
        title = "Grafo simple no dirigido con coloreo greedy",

        visNetworkOutput(
          "grafo_coloreo_modal",
          height = "750px",
          width  = "100%"
        ),

        easyClose = TRUE,
        size = "xl",
        footer = modalButton("Cerrar")
      )
    )
  })

  output$grafo_coloreo_modal <- renderVisNetwork({
    construir_vis_coloreo(grafo_coloreo_vis())
  })

  output$tabla_aristas_coloreo <- renderDT({

    if (is.null(datos$resultados)) {
      return(
        datatable(
          data.frame(
            Estado = "Primero generar el escenario y luego ejecutar los algoritmos."
          ),
          options = list(dom = "t", scrollX = TRUE),
          rownames = FALSE
        )
      )
    }

    aristas <- datos$resultados$aristas_coloreo

    if (nrow(aristas) == 0) {
      aristas <- data.frame(
        Estado = "No hay aristas para coloreo con los criterios actuales.",
        Sugerencia = "Seleccionar cruces moderados o altos, y activar profesor o semestre."
      )
    }

    datatable(
      aristas,
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE
    )
  })

  output$tabla_colores <- renderDT({

    if (is.null(datos$resultados)) {
      return(
        datatable(
          data.frame(
            Estado = "Primero generar el escenario y luego ejecutar los algoritmos."
          ),
          options = list(dom = "t", scrollX = TRUE),
          rownames = FALSE
        )
      )
    }

    colores <- datos$resultados$colores

    tabla <- data.frame(
      id_clase = names(colores),
      color_asignado = colores,
      stringsAsFactors = FALSE
    ) %>%
      left_join(datos$clases, by = "id_clase") %>%
      select(
        id_clase,
        asignatura,
        profesor,
        semestre,
        dia,
        hora_inicio,
        hora_fin,
        color_asignado
      )

    datatable(
      tabla,
      options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE
    )
  })
}

# --- Ejecutar aplicación ---

shinyApp(ui = ui, server = server)
