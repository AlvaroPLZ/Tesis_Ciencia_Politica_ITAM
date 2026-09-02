<div align="center">

# Dando y dando

### Dádivas, reciprocidad y comportamiento electoral en México, 2000–2024

**Álvaro Pérez López**

Tesis para obtener el título de Licenciado en Ciencia Política
Instituto Tecnológico Autónomo de México · 2026

<br>

![R](https://img.shields.io/badge/R-4.3+-276DC3?style=flat-square&logo=r&logoColor=white)
![Método](https://img.shields.io/badge/método-WLS--FE%20%C2%B7%20PSM-555?style=flat-square)
![Licencia](https://img.shields.io/badge/licencia-CC%20BY%204.0-lightgrey?style=flat-square)

</div>

---

## Resumen

> Esta tesis estima el efecto causal de recibir dádivas partidistas sobre el voto por partido, la identificación partidista y la participación electoral en México entre 2000 y 2024. La estrategia empírica combina modelos de mínimos cuadrados ponderados con efectos fijos, emparejamiento por puntaje de propensión (PSM) y análisis de sensibilidad.
>
> Los resultados muestran que el clientelismo no opera como un mecanismo universal homogéneo de compra de voto. Bajo FE, el PAN y el PRD presentan efectos positivos sobre el voto propio (+7.3 pp y +6.4 pp); solo el PRD es robusto bajo PSM (+11.5 pp). La identificación partidista muestra asociaciones positivas bajo FE para los tres partidos históricos, pero PSM las confirma únicamente para el PRD (+6.7 pp). Las hipótesis de arraigo territorial e incumbencia no encuentran soporte empírico. La exclusión del intercambio clientelar reduce la probabilidad de votar (−3.5 pp bajo PSM), mientras que recibir una dádiva no genera movilización adicional.
>
> La contribución central es documentar que el clientelismo puede generar retornos en la identificación partidista —un horizonte de mediano plazo— y que dichos efectos dependen del partido, la competitividad electoral y la posición del votante respecto al intercambio.

**Palabras clave:** clientelismo electoral · compra de votos · identificación partidista · reciprocidad política · exclusión clientelar · Propensity Score Matching

**Códigos JEL:** D72 · C21 · D73

📄 **[Leer la tesis completa (PDF)](Tesis_final.pdf)**

---

## Contenido del repositorio

```
.
├── Tesis_final.pdf              Documento completo
├── Code/
│   └── Codigo_final_TESIS.R     Script unificado de análisis (3,987 líneas)
├── Data/                        Bases de datos (ver Data/README.md)
├── Plots/                       Figuras generadas por el script
└── docs/
    └── Polls/                   Cuestionarios de las encuestas, 2000–2024
```

---

## Estrategia empírica

| Componente | Especificación |
|---|---|
| **Tratamiento** | Recepción de dádiva partidista (`gift_bin`), desagregada por partido: PAN, PRI, PRD, MORENA |
| **Variables dependientes** | Voto por partido · Identificación partidista · Participación electoral |
| **Modelo principal** | Mínimos cuadrados ponderados con efectos fijos de año (`fixest::feols`), errores estándar agrupados a nivel municipal |
| **Ponderadores** | Normalizados por año: ω*ᵢ* ⁄ ω̄*ᵧ* |
| **Emparejamiento** | PSM 1:1, vecino más cercano con reemplazo, *caliper* 0.2, exacto por año (`MatchIt`) |
| **Diagnósticos de balance** | Diferencias estandarizadas, *love plots*, traslape de puntajes (`cobalt`) |
| **Sensibilidad** | Cotas de Rosenbaum (`rbounds`) · E-values (`EValue`) |

### Mapa del script

El análisis vive en un solo archivo, `Code/Codigo_final_TESIS.R`, organizado en secciones numeradas:

| Líneas | Sección | Contenido |
|---:|---|---|
| 1–79 | I–IV | Librerías, carga de datos, construcción del ponderador y diseño muestral |
| 81–372 | V | Datos externos: incumbencia municipal y estatal, población, arraigo partidista histórico |
| 374–437 | VI | Muestra analítica para la hipótesis de coincidencia con la incumbencia |
| 439–620 | VII–XI | Descriptivos: perfil sociodemográfico, participación, voto reportado, covariables |
| 622–845 | XII–XIV | Distribución del tratamiento, *targeting* por partido, correlaciones |
| 846–1500 | XV | Matriz de probabilidades condicionales y modelos base con efectos fijos |
| 1506–1760 | — | PSM global: emparejamiento, balance, ATT, cotas de Rosenbaum y E-values |
| 1790–2100 | — | PSM por partido: PAN, PRI, PRD, MORENA |
| 2189–2540 | — | Diagnósticos de balance y traslape por partido |
| 2548–2760 | — | Identificación partidista por partido |
| 2764–3080 | — | Participación electoral: dádiva, conocimiento del intercambio y exclusión |
| 3084–3600 | — | Heterogeneidad: competitividad, predisposición previa, exposición múltiple y presencia partidista |

---

## Reproducción

El script instala automáticamente las dependencias faltantes en su primera ejecución (líneas 28–29). Los paquetes centrales son:

`tidyverse` · `srvyr` · `survey` · `fixest` · `MatchIt` · `cobalt` · `rbounds` · `EValue` · `gt` · `modelsummary` · `ggplot2`

> [!NOTE]
> Las rutas de lectura y el `setwd()` de la línea 4 apuntan al directorio de trabajo del autor. Para ejecutar el script es necesario sustituirlas por las rutas locales correspondientes (líneas 4, 44, 87, 88 y 171). La tipografía Times New Roman se carga desde `/Library/Fonts/` (líneas 33–36); en sistemas sin esa fuente conviene omitir ese bloque.

Consulte [`Data/README.md`](Data/README.md) para la procedencia de cada base y la nota sobre los archivos no incluidos por límites de tamaño.

---

## Cómo citar

```bibtex
@thesis{perezlopez2026dando,
  author      = {Pérez López, Álvaro},
  title       = {Dando y dando: Dádivas, reciprocidad y comportamiento
                 electoral en México, 2000--2024},
  type        = {Tesis de licenciatura},
  institution = {Instituto Tecnológico Autónomo de México},
  year        = {2026},
  url         = {https://github.com/AlvaroPLZ/Tesis_Ciencia_Politica_ITAM}
}
```

---

## Créditos

Tesis dirigida por **Adrián Lucardi**. Los datos de encuesta en que se basa este trabajo fueron facilitados por **Joy Langston** (CIDE). Los datos electorales a nivel municipal y seccional provienen de la base de datos de **Eric Magar**.

## Licencia

El código se distribuye bajo licencia [MIT](LICENSE). El texto de la tesis y las figuras se distribuyen bajo [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/deed.es). Las bases de datos de terceros conservan las condiciones de uso de sus fuentes originales.
