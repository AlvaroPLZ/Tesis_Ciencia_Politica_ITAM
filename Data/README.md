# Datos

Procedencia y contenido de las bases utilizadas en `Code/Codigo_final_TESIS.R`.

---

## Archivos incluidos en el repositorio

### `clientelism_final.xlsx` · 3.6 MB

Base armonizada de encuestas postelectorales mexicanas, 2000–2024 (nueve olas). Unidad de observación: el individuo. Contiene el indicador de tratamiento (`gift`), las variables dependientes de voto, identificación partidista y participación, las covariables sociodemográficas (`edu`, `type`, `gen`, `eth`, `p_id`, `age`), los identificadores geográficos (`clave_mun`, `est`) y el ponderador muestral (`ponderador`).

Los datos fueron facilitados por **Joy Langston** (CIDE). Los cuestionarios de cada ola se encuentran en [`docs/Polls/`](../docs/Polls).

### `aymu1989-on.incumbents.csv` · 3.7 MB · 24,465 registros

Base de incumbentes municipales de México desde 1989. Incluye partido gobernante, candidato, margen de victoria (`mg`), partido en segundo lugar, y variables de reelección y legado familiar. Se emplea en la Sección V-A del script para construir la variable de incumbencia municipal en el periodo 2019–2025.

Fuente: **Eric Magar**, *The Mexican Municipal Elections Electoral Precinct-Level Database*.
🔗 https://github.com/emagar/The-Mexican-municipal-elections-electoral-precinct-level-database

---

## Archivos no incluidos

Los siguientes archivos son necesarios para ejecutar el script completo, pero no forman parte del repositorio. Deben descargarse por separado y colocarse en esta carpeta.

### `all_states_final.csv` · 190 MB · 456,502 registros

Resultados electorales municipales a nivel sección electoral. Contiene el voto y las participaciones sobre voto válido y lista nominal para PRI, PAN, PRD y MORENA, el partido incumbente municipal y estatal, el margen de victoria municipal y la participación electoral.

**No se incluye porque excede el límite de 100 MB por archivo de GitHub.** Se utiliza en la Sección V-A del script (línea 87) para construir la incumbencia municipal del periodo 1994–2019.

Fuente: **Eric Magar**, base de datos citada arriba.

### `pobproy_ggrupos.csv` · 22 MB

Proyecciones de población municipal por grupos de edad. Se utiliza en la Sección V-C del script (línea 171) para construir la variable `ln_pop`.

Fuente: **CONAPO**, Proyecciones de la Población de México.
🔗 https://www.gob.mx/conapo

---

> [!IMPORTANT]
> Las bases de terceros conservan las condiciones de uso de sus fuentes originales. Este repositorio las incluye o referencia únicamente con fines de replicación académica.
