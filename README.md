---
title: "Documento de especificación: Optimización de asignación de salones mediante grafos"
author: "Ciencias de la Computación II"
date: "`r Sys.Date()`"
output:
  html_document:
    toc: true
    toc_depth: 3
    number_sections: true
    theme: flatly
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE
)
```

# Resumen del proyecto

Este documento presenta la especificación del proyecto final de la materia **Ciencias de la Computación II**. El proyecto aborda un problema puntual de planificación académica: la **asignación de salones a clases** en una Facultad de Ingeniería, considerando restricciones de horario, capacidad, tipo de salón, profesor y semestre.

El proyecto se desarrolla mediante una aplicación interactiva en **R Shiny**, donde se pueden generar escenarios simulados, ejecutar algoritmos de grafos, visualizar resultados y comparar métricas. La solución se apoya en dos técnicas principales:

- **Matching bipartito**, para asignar clases a recursos compatibles.
- **Coloreo de grafos**, para separar clases que no deberían coincidir en el mismo bloque académico.

La aplicación permite evaluar el comportamiento de la solución en tres escenarios principales: demanda baja, demanda media y demanda alta.

# Problema elegido

## Descripción del problema

En una facultad, la planificación académica requiere asignar salones a clases considerando diferentes condiciones. Cada clase tiene un horario, una cantidad de estudiantes, un tipo de salón requerido y posiblemente restricciones asociadas al profesor o semestre. Por otro lado, cada salón tiene una capacidad y un tipo, por ejemplo aula tradicional o laboratorio.

El problema seleccionado consiste en:

> Asignar salones a clases programadas, de manera que cada clase reciba un recurso compatible y se reduzcan los cruces académicos entre clases que comparten profesor o semestre.

En este proyecto, un recurso no se entiende únicamente como un salón físico, sino como la combinación:

\[
recurso = salón + día + bloque\ horario
\]

Esto permite que un mismo salón pueda usarse varias veces durante la semana, siempre que no sea en el mismo bloque horario.

## Pregunta orientadora

La pregunta que guía el proyecto es:

> ¿Cómo asignar salones a clases en una Facultad de Ingeniería, considerando capacidad, tipo de salón y horario, usando técnicas de grafos como matching bipartito y coloreo?

## Alcance del proyecto

El proyecto no pretende resolver toda la planificación académica institucional. Se enfoca únicamente en la asignación de salones a clases y en el análisis de separación de clases mediante coloreo.

Quedan fuera del alcance:

- Creación automática de horarios desde cero.
- Preferencias individuales de estudiantes.
- Disponibilidad real de todos los profesores.
- Integración con bases de datos institucionales reales.
- Optimización multiobjetivo avanzada.

# Modelado formal con grafos

## Grafo bipartito clase-recurso

El primer modelo usado es un **grafo bipartito**:

\[
G_b = (C \cup R, E)
\]

donde:

- \(C\) es el conjunto de clases.
- \(R\) es el conjunto de recursos.
- \(E\) es el conjunto de aristas de compatibilidad.

Una arista \((c_i, r_j)\) existe si la clase \(c_i\) puede ser asignada al recurso \(r_j\).

Para que exista una arista, se deben cumplir las siguientes condiciones:

\[
dia(c_i) = dia(r_j)
\]

\[
hora(c_i) = hora(r_j)
\]

Si se activa la restricción de capacidad:

\[
capacidad(r_j) \geq estudiantes(c_i)
\]

Si se activa la restricción de tipo de salón:

\[
tipo(r_j) = tipo\_requerido(c_i)
\]

Por ejemplo, una clase de Programación que requiere laboratorio y tiene 30 estudiantes solo podrá conectarse con recursos que correspondan a laboratorios disponibles en el mismo bloque horario y con capacidad suficiente.

## Matching bipartito

Una vez construido el grafo bipartito, se aplica matching bipartito. El objetivo es seleccionar un subconjunto de aristas:

\[
M \subseteq E
\]

cumpliendo que ningún vértice se repita. En el contexto del proyecto esto significa:

- Una clase no puede quedar asignada a dos salones.
- Un recurso no puede ser usado por dos clases al mismo tiempo.

El objetivo principal es maximizar:

\[
\max |M|
\]

Es decir, asignar la mayor cantidad posible de clases.

## Grafo simple no dirigido para coloreo

El segundo modelo es un **grafo simple no dirigido**:

\[
G_c = (V, E_c)
\]

donde:

- \(V\) representa el conjunto de clases.
- \(E_c\) representa las aristas entre clases que deben separarse.

Cada clase es un vértice. Una arista entre dos clases indica que esas clases no deberían estar en el mismo bloque académico porque coinciden en horario y, además, comparten profesor o semestre.

Formalmente, se agrega una arista \((c_i, c_j)\) si:

\[
dia(c_i) = dia(c_j)
\]

\[
horario(c_i) \cap horario(c_j) \neq \emptyset
\]

y se cumple al menos una de estas condiciones:

\[
profesor(c_i) = profesor(c_j)
\]

\[
semestre(c_i) = semestre(c_j)
\]

## Coloreo greedy

Sobre el grafo simple no dirigido se aplica un algoritmo de coloreo greedy. El objetivo es asignar colores a los vértices:

\[
color: V \rightarrow \{1,2,3,\dots,k\}
\]

de forma que dos vértices adyacentes no tengan el mismo color:

\[
(c_i,c_j) \in E_c \Rightarrow color(c_i) \neq color(c_j)
\]

En este proyecto, los colores se interpretan como grupos de clases que pueden separarse para evitar choques académicos según los criterios seleccionados.

# Justificación de los algoritmos seleccionados

## Justificación del matching bipartito

El matching bipartito es adecuado porque el problema de asignación tiene naturalmente dos conjuntos separados:

- Clases.
- Recursos disponibles.

Las conexiones entre ambos conjuntos representan compatibilidad. El matching permite seleccionar asignaciones sin repetir clase ni recurso. Por esta razón, es una técnica pertinente para resolver el problema central del proyecto.

En la implementación se usa una estrategia basada en caminos aumentantes. La idea general es intentar asignar cada clase a un recurso compatible. Si el recurso ya está ocupado, el algoritmo intenta reubicar la clase que lo ocupa para abrir una nueva posibilidad de asignación.

## Justificación del coloreo greedy

El coloreo de grafos es adecuado para representar restricciones entre clases. Si dos clases no deberían coincidir, se conectan mediante una arista. El algoritmo de coloreo busca asignar colores diferentes a clases conectadas.

Se usa una versión greedy porque:

- Es sencilla de implementar.
- Es rápida para escenarios pequeños y medianos.
- Permite explicar claramente el funcionamiento del algoritmo.
- Es suficiente para una solución académica y demostrativa.

Sin embargo, se reconoce que el coloreo greedy es una heurística. Esto significa que no siempre garantiza usar el mínimo número posible de colores.

# Supuestos del proyecto

El proyecto trabaja con los siguientes supuestos:

1. Los datos usados son simulados.
2. Cada clase tiene un día, una hora de inicio, una hora de fin, cantidad de estudiantes, profesor, semestre y tipo de salón requerido.
3. Cada salón tiene una capacidad y un tipo.
4. Un recurso se define como salón + día + bloque horario.
5. Una clase solo puede asignarse a un recurso de su mismo día y bloque horario.
6. Si la restricción de capacidad está activa, el salón debe tener capacidad suficiente.
7. Si la restricción de tipo está activa, el tipo de salón debe coincidir con el requerido por la clase.
8. Para el coloreo, dos clases se conectan si coinciden en día, se cruzan en horario y comparten profesor o semestre.
9. La aplicación permite activar o desactivar algunas restricciones para observar el comportamiento de los algoritmos.

# Dataset usado

## Tipo de dataset

El proyecto usa datos **simulados** generados dentro de la aplicación Shiny. Esto se decidió porque los datos reales de planificación académica pueden ser privados o difíciles de conseguir. La simulación permite controlar el tamaño del problema y evaluar distintos escenarios.

## Dataset de clases

La tabla de clases contiene las siguientes columnas:

| Variable | Descripción |
|---|---|
| `id_clase` | Identificador único de la clase. |
| `asignatura` | Nombre simulado de la asignatura y grupo. |
| `dia` | Día de la semana. |
| `hora_inicio` | Hora de inicio de la clase. |
| `hora_fin` | Hora de finalización de la clase. |
| `estudiantes` | Número de estudiantes inscritos. |
| `tipo_requerido` | Tipo de salón requerido: Aula o Laboratorio. |
| `profesor` | Identificador simulado del profesor. |
| `semestre` | Semestre asociado a la clase. |

## Dataset de salones

La tabla de salones contiene:

| Variable | Descripción |
|---|---|
| `id_salon` | Identificador único del salón. |
| `salon` | Nombre simulado del salón. |
| `capacidad` | Capacidad máxima del salón. |
| `tipo_salon` | Tipo de salón: Aula o Laboratorio. |

## Descripción de la generación de datos

La aplicación genera los datos según el tipo de escenario seleccionado:

| Escenario | Clases | Salones | Descripción |
|---|---:|---:|---|
| Demanda baja | 10 | 8 | Pocas clases y disponibilidad suficiente. |
| Demanda media | 18 | 7 | Mayor cantidad de clases y disponibilidad más limitada. |
| Demanda alta | 30 | 6 | Muchas clases y pocos salones. |
| Personalizado | Definido en la interfaz | Definido en la interfaz | Permite experimentar con otros tamaños. |

También se puede seleccionar el nivel de cruces académicos:

| Nivel | Descripción |
|---|---|
| Sin cruces académicos | Distribuye las clases para evitar choques. |
| Cruces moderados | Genera algunas clases con coincidencia de horario, profesor o semestre. |
| Cruces altos | Concentra muchas clases en pocas franjas horarias. |

# Métricas de éxito

Las métricas usadas para evaluar la solución son:

| Métrica | Descripción |
|---|---|
| Total de clases | Número de clases generadas en el escenario. |
| Clases asignadas | Cantidad de clases que recibieron un recurso. |
| Clases no asignadas | Cantidad de clases que no pudieron asignarse. |
| Porcentaje de asignación | Proporción de clases asignadas respecto al total. |
| Aristas en el grafo para coloreo | Número de relaciones entre clases que deben separarse. |
| Colores usados | Número de colores usados por el algoritmo greedy. |
| Tiempo de ejecución | Tiempo computacional requerido para ejecutar los algoritmos. |

La métrica principal para el matching es el **porcentaje de asignación**. La métrica principal para el coloreo es el **número de colores usados**, junto con la cantidad de aristas del grafo simple no dirigido.

# Escenarios comparativos

## Escenario 1: Demanda baja

Configuración sugerida:

```text
Tipo de escenario: Demanda baja
Nivel de cruces académicos: Sin cruces académicos
Semilla: 123
Considerar capacidad del salón: activado
Considerar tipo de salón: activado
Separar clases con el mismo profesor: activado
Separar clases del mismo semestre: activado
```

### Qué muestra este escenario

Este escenario representa una situación favorable. Hay pocas clases y suficientes salones. Por eso se espera que el porcentaje de asignación sea alto.

También se espera que el grafo simple no dirigido tenga pocas aristas, porque las clases se generan evitando cruces académicos. En consecuencia, el algoritmo de coloreo debería usar pocos colores.

### Interpretación

Este caso sirve como referencia inicial. Muestra cómo se comportan los algoritmos cuando la demanda de recursos no es alta.

## Escenario 2: Demanda media

Configuración sugerida:

```text
Tipo de escenario: Demanda media
Nivel de cruces académicos: Cruces moderados
Semilla: 123
Considerar capacidad del salón: activado
Considerar tipo de salón: activado
Separar clases con el mismo profesor: activado
Separar clases del mismo semestre: activado
```

### Qué muestra este escenario

Este escenario representa una situación más cercana a una planificación académica normal. Hay más clases y menos salones disponibles en comparación con el escenario de demanda baja.

Se espera observar:

- Un porcentaje de asignación menor que en demanda baja.
- Algunas clases sin asignar si no hay suficientes recursos compatibles.
- Un grafo para coloreo con más aristas.
- Más colores usados que en el escenario fácil.

### Interpretación

Este caso permite observar cómo cambia la solución cuando aumenta la presión sobre los recursos. También permite mostrar que el coloreo empieza a ser más relevante cuando aparecen cruces académicos.

## Escenario 3: Demanda alta

Configuración sugerida:

```text
Tipo de escenario: Demanda alta
Nivel de cruces académicos: Cruces altos
Semilla: 123
Considerar capacidad del salón: activado
Considerar tipo de salón: activado
Separar clases con el mismo profesor: activado
Separar clases del mismo semestre: activado
```

### Qué muestra este escenario

Este escenario representa una situación difícil. Hay muchas clases y pocos salones. Además, las clases se concentran en pocas franjas horarias, por lo que el grafo para coloreo se vuelve más denso.

Se espera observar:

- Menor porcentaje de clases asignadas.
- Más clases sin asignar.
- Más aristas en el grafo para coloreo.
- Más colores usados.
- Mayor tiempo de ejecución, aunque debería seguir siendo bajo para los tamaños usados.

### Interpretación

Este caso permite evidenciar las limitaciones del sistema cuando la demanda supera la disponibilidad. También permite mostrar que el algoritmo no “inventa” salones, sino que respeta las restricciones del modelo.

# Comparación entre escenarios

La comparación esperada entre escenarios se resume así:

| Aspecto | Demanda baja | Demanda media | Demanda alta |
|---|---|---|---|
| Clases generadas | Baja | Media | Alta |
| Salones disponibles | Alta en relación con la demanda | Limitada | Muy limitada |
| Porcentaje de asignación | Alto | Medio | Menor |
| Clases sin asignar | Pocas o ninguna | Algunas | Varias |
| Aristas para coloreo | Pocas | Moderadas | Altas |
| Colores usados | Pocos | Intermedios | Más colores |
| Dificultad del problema | Baja | Media | Alta |

La comparación permite explicar que, a medida que aumenta la demanda y los cruces académicos, el problema se vuelve más difícil y los algoritmos muestran resultados más restrictivos.

# Visualizaciones

La aplicación incluye tres visualizaciones principales.

## Grafo bipartito de compatibilidad

Muestra todas las conexiones posibles entre clases y recursos compatibles. Puede ser denso, porque representa todas las alternativas disponibles.

## Grafo de asignación final

Muestra solamente las aristas seleccionadas por el matching. Esta visualización es más clara para la presentación, porque indica directamente qué recurso fue asignado a cada clase.

## Grafo simple no dirigido con coloreo

Muestra las clases como nodos y las aristas entre clases que deben separarse. Los colores representan la solución del algoritmo greedy.


# Conclusiones

El proyecto permite demostrar cómo las técnicas de grafos pueden aplicarse a un problema realista de planificación académica. El matching bipartito permite asignar clases a recursos compatibles respetando restricciones básicas. El coloreo de grafos permite analizar la separación de clases que no deberían coincidir por profesor o semestre.

Los escenarios muestran que, cuando la demanda aumenta, el número de clases sin asignar puede crecer y el grafo para coloreo puede volverse más denso. Esto permite comparar el comportamiento de los algoritmos bajo diferentes niveles de dificultad.

Como trabajo futuro, se podrían incorporar datos reales anonimizados, disponibilidad de profesores, preferencias de horarios, asignación de edificios o minimización de desplazamientos dentro de la facultad.


---

## Integrantes

- **David Santiago Lugo Piñeros** — 20222020180  
- **Luisa Fernanda Guerrero Ordoñez** — 20212020099
- **Juan David Quiroga Gomez** — 20222020206  

---
