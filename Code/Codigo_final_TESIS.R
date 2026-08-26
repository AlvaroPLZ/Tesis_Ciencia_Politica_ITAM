### Título: Tesis CPol — Script unificado
### Autor: Álvaro Pérez

setwd("/Users/alvaroperezlopez/Library/CloudStorage/OneDrive-Personal/Documentos/LICENCIATURA_ITAM/TESIS/Resultados")

# ******************************************************************************
#### I. Librerías ####
# ******************************************************************************

libs <- c(
  # Datos y manipulación
  "tidyverse", "readxl", "janitor", "forcats",
  # Encuesta
  "srvyr", "survey",
  # Tablas y visualización
  "scales", "gt", "gtsummary", "corrplot",
  "viridis", "showtext",
  # Estadística
  "broom", "wCorr", "naniar", "car", "psych", "rlang",
  # Modelos
  "lmtest", "sandwich", "lfe", "fixest",
  "modelsummary", "kableExtra", "stargazer",
  # Matching
  "MatchIt", "cobalt", "rbounds",
  # Misc
  "nnet"
)
to_inst <- setdiff(libs, rownames(installed.packages()))
if (length(to_inst)) install.packages(to_inst)
invisible(lapply(libs, library, character.only = TRUE))

# Tipografía
font_add("Times New Roman",
         regular = "/Library/Fonts/Times New Roman.ttf",
         bold    = "/Library/Fonts/Times New Roman Bold.ttf",
         italic  = "/Library/Fonts/Times New Roman Italic.ttf")
showtext_auto()
showtext_opts(dpi = 300)

# ******************************************************************************
#### II. Carga de datos ####
# ******************************************************************************

gift <- read_xlsx("/Users/alvaroperezlopez/Library/CloudStorage/OneDrive-Personal/Documentos/LICENCIATURA_ITAM/TESIS/Data/clientelism_final.xlsx")

# ******************************************************************************
#### III. Limpieza y construcción del ponderador ####
# ******************************************************************************

gift <- gift %>%
  mutate(
    # Tipos
    year      = as.factor(year),
    clave_mun = as.factor(clave_mun),
    est       = as.factor(est),
    across(c(edu, type, gen, eth, p_id, age), factor),
    # Variables derivadas
    ln_pop   = log(pop),
    margin   = margin / 100,
    # Tratamiento limpio
    gift_bin = suppressWarnings(as.integer(as.character(gift))),
    gift_bin = if_else(gift_bin %in% c(0L, 1L), gift_bin, NA_integer_)
  ) %>%
  # Filtrar pesos inválidos
  filter(!is.na(ponderador) & is.finite(ponderador) & ponderador > 0) %>%
  # Ponderador global 
  mutate(w_norm_global = ponderador / mean(ponderador, na.rm = TRUE)) %>%
  # Ponderador por año 
  group_by(year) %>%
  mutate(ponderador_norm = ponderador / mean(ponderador, na.rm = TRUE)) %>%
  ungroup()

# ******************************************************************************
#### IV. Diseños de encuesta ####
# ******************************************************************************

gift_svy <- survey::svydesign(ids = ~1, weights = ~ponderador_norm, data = gift)
gift_srv <- as_survey_design(gift, weights = ponderador_norm)

# ******************************************************************************
#### V. Datos externos y construcción de gift_master / gift_master2 ####
# ******************************************************************************
# Nota: añadir "stringr" y "slider" al vector libs de la Sección I

# ---- V-A. Incumbente municipal (1994–2025) ----------------------------------

incumbents_94_19 <- read_csv("/Users/alvaroperezlopez/Library/CloudStorage/OneDrive-Personal/Documentos/LICENCIATURA_ITAM/TESIS/Data/all_states_final.csv")
incumbents_19_25 <- read_csv("/Users/alvaroperezlopez/Library/CloudStorage/OneDrive-Personal/Documentos/LICENCIATURA_ITAM/TESIS/Data/aymu1989-on.incumbents.csv")

# Recode incumbent municipal 1994–2019
incumbents_94_19 <- incumbents_94_19 %>%
  mutate(
    incumbent_party_rec = case_when(
      str_detect(incumbent_party, "PAN") & str_detect(incumbent_party, "PRI") ~ 6,
      str_detect(incumbent_party, "PRI") & str_detect(incumbent_party, "PRD") ~ 7,
      str_detect(incumbent_party, "PAN") & str_detect(incumbent_party, "PRD") ~ 8,
      str_detect(incumbent_party, "PAN")    ~ 1,
      str_detect(incumbent_party, "PRI")    ~ 2,
      str_detect(incumbent_party, "PRD")    ~ 3,
      str_detect(incumbent_party, "MORENA") ~ 4,
      TRUE ~ 5
    )
  )

has_token <- function(x, token) {
  str_detect(x, str_c("(^|-)", token, "(-|$)"))
}

# Recode incumbent municipal 2019–2025
incumbents_19_25 <- incumbents_19_25 %>%
  mutate(
    part = str_to_lower(part),
    incumbent_party_rec = case_when(
      has_token(part, "pan") & has_token(part, "pri") ~ 6,
      has_token(part, "pri") & has_token(part, "prd") ~ 7,
      has_token(part, "pan") & has_token(part, "prd") ~ 8,
      has_token(part, "pan")    ~ 1,
      has_token(part, "pri")    ~ 2,
      has_token(part, "prd")    ~ 3,
      has_token(part, "morena") ~ 4,
      TRUE ~ 5
    )
  )

# Estandarizar y unir
incumb_94_19 <- incumbents_94_19 %>%
  mutate(clave_mun = str_pad(as.character(mun_code), width = 5, pad = "0")) %>%
  select(year, clave_mun, mun_winning_margin, incumbent_party_rec) %>%
  rename(margin = mun_winning_margin, inc_mun = incumbent_party_rec)

incumb_19_25 <- incumbents_19_25 %>%
  mutate(clave_mun = str_pad(as.character(ife), width = 5, pad = "0")) %>%
  select(yr, clave_mun, mg, incumbent_party_rec) %>%
  filter(yr > 2019) %>%
  rename(year = yr, margin = mg, inc_mun = incumbent_party_rec)

incumb_pre94 <- incumbents_19_25 %>%
  filter(yr < 1994) %>%
  mutate(clave_mun = str_pad(as.character(ife), width = 5, pad = "0")) %>%
  select(yr, clave_mun, mg, incumbent_party_rec) %>%
  rename(year = yr, margin = mg, inc_mun = incumbent_party_rec)

incumbentes <- bind_rows(incumb_pre94, incumb_94_19, incumb_19_25)
# Nota: margin ya viene en proporciones (0–1) en ambas fuentes, consistente
# con gift$margin (dividido por 100 en la Sección III)

# Helper: moda estadística
Mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

incumbentes_mun <- incumbentes %>%
  group_by(year, clave_mun) %>%
  summarise(
    margin        = if (n_distinct(margin, na.rm = TRUE) == 1)
      first(na.omit(margin))
    else
      median(margin, na.rm = TRUE),
    inc_mun       = Mode(inc_mun),
    n_rows        = n(),
    n_margin_vals = n_distinct(margin, na.rm = TRUE),
    n_inc_vals    = n_distinct(inc_mun, na.rm = TRUE),
    .groups = "drop"
  )

# ---- V-C. Población municipal -----------------------------------------------

pob <- read_csv("/Users/alvaroperezlopez/Library/CloudStorage/OneDrive-Personal/Documentos/LICENCIATURA_ITAM/TESIS/Data/pobproy_ggrupos.csv")

pob_mun <- pob %>%
  mutate(
    year      = as.integer(ANO),
    clave_mun = str_pad(as.character(CLAVE), width = 5, pad = "0")
  ) %>%
  group_by(year, clave_mun) %>%
  summarise(pob_total = sum(POB_TOTAL, na.rm = TRUE), .groups = "drop") %>%
  mutate(ln_pop_new = log(pob_total))

# ---- V-D. gift_master: gift + incumbentes + gobernador + población ----------
# gift ya tiene w_norm_global desde la Sección III; gift_master lo hereda.

gift_master <- gift %>%
  mutate(
    year      = as.integer(as.character(year)),
    clave_mun = as.character(clave_mun),
    est       = as.integer(as.character(est))
  ) %>%
  # Incumbente municipal
  rename(margin_old = margin, inc_mun_old = inc_mun) %>%
  left_join(
    incumbentes_mun %>%
      mutate(year = as.integer(year), clave_mun = as.character(clave_mun)) %>%
      select(year, clave_mun, margin, inc_mun),
    by = c("year", "clave_mun")
  ) %>%
  mutate(
    margin  = coalesce(margin,  margin_old),
    inc_mun = coalesce(inc_mun, inc_mun_old)
  ) %>%
  select(-margin_old, -inc_mun_old) %>%
  # Población
  rename(pop_old = pop, ln_pop_old = ln_pop) %>%
  left_join(
    pob_mun %>% select(year, clave_mun, pob_total, ln_pop_new),
    by = c("year", "clave_mun")
  ) %>%
  mutate(
    pop    = coalesce(pob_total, pop_old),
    ln_pop = coalesce(ln_pop_new, ln_pop_old)
  ) %>%
  select(-pob_total, -ln_pop_new, -pop_old, -ln_pop_old) %>%
  # Reconvertir a factor (se castearon a integer para los joins)
  mutate(
    year      = factor(year),
    clave_mun = factor(clave_mun),
    est       = factor(est)
  )

# ---- V-D.2 Variables de voto incondicional ----------------------------------
gift_master <- gift_master %>%
  mutate(
    voted = case_when(
      vote == 1L ~ 1L,
      vote == 0L ~ 0L,
      TRUE       ~ NA_integer_
    ),
    vote_party_known = if_else(
      voted == 1L & !is.na(vote_which), 1L, 0L, missing = 0L
    ),
    vote_PAN_tot = case_when(
      voted == 0L                              ~ 0L,
      voted == 1L & !is.na(vote_which)         ~ as.integer(vote_which == 1),
      voted == 1L &  is.na(vote_which)         ~ NA_integer_,
      TRUE                                     ~ NA_integer_
    ),
    vote_PRI_tot = case_when(
      voted == 0L                              ~ 0L,
      voted == 1L & !is.na(vote_which)         ~ as.integer(vote_which == 2),
      voted == 1L &  is.na(vote_which)         ~ NA_integer_,
      TRUE                                     ~ NA_integer_
    ),
    vote_PRD_tot = case_when(
      voted == 0L                              ~ 0L,
      voted == 1L & !is.na(vote_which)         ~ as.integer(vote_which == 3),
      voted == 1L &  is.na(vote_which)         ~ NA_integer_,
      TRUE                                     ~ NA_integer_
    ),
    vote_MRN_tot = case_when(
      voted == 0L                              ~ 0L,
      voted == 1L & !is.na(vote_which)         ~ as.integer(vote_which == 4),
      voted == 1L &  is.na(vote_which)         ~ NA_integer_,
      TRUE                                     ~ NA_integer_
    ),
    vote_OTH_tot = case_when(
      voted == 0L                              ~ 0L,
      voted == 1L & !is.na(vote_which)         ~ as.integer(vote_which == 5),
      voted == 1L &  is.na(vote_which)         ~ NA_integer_,
      TRUE                                     ~ NA_integer_
    ),
    vote_incum_mun_tot = case_when(
      voted == 0L                                                   ~ 0L,
      voted == 1L & !is.na(vote_which) & !is.na(inc_mun)           ~ as.integer(vote_which == inc_mun),
      voted == 1L                                                   ~ NA_integer_,
      TRUE                                                          ~ NA_integer_
    ),
    vote_incum_gob_tot = case_when(
      voted == 0L                                                   ~ 0L,
      voted == 1L & !is.na(vote_which) & !is.na(inc_gov)           ~ as.integer(vote_which == inc_gov),
      voted == 1L                                                   ~ NA_integer_,
      TRUE                                                          ~ NA_integer_
    ),
    vote_incum_pres_tot = case_when(
      voted == 0L                                                   ~ 0L,
      voted == 1L & !is.na(vote_which) & !is.na(inc_pre)           ~ as.integer(vote_which == inc_pre),
      voted == 1L                                                   ~ NA_integer_,
      TRUE                                                          ~ NA_integer_
    )
  )

# ---- V-E. Arraigo histórico partidista (ventana 12 años = 4 elecciones) ----

inc_panel <- incumbentes_mun %>%
  mutate(
    year      = as.integer(year),
    inc_mun   = as.integer(inc_mun),
    clave_mun = as.character(clave_mun),
    inc_pan   = as.integer(inc_mun == 1),
    inc_pri   = as.integer(inc_mun == 2),
    inc_prd   = as.integer(inc_mun == 3),
    inc_mrn   = as.integer(inc_mun == 4)
  ) %>%
  arrange(clave_mun, year) %>%
  group_by(clave_mun) %>%
  mutate(
    # pres_P_12: proporción de las 4 elecciones previas (~12 años) en que
    # el partido P gobernó el municipio.
    # lag() excluye el año t; .before = 3 cubre t-1, t-2, t-3, t-4.
    pres_pan_12 = slide_dbl(lag(inc_pan), ~mean(.x, na.rm = TRUE),
                            .before = 3, .complete = FALSE),
    pres_pri_12 = slide_dbl(lag(inc_pri), ~mean(.x, na.rm = TRUE),
                            .before = 3, .complete = FALSE),
    pres_prd_12 = slide_dbl(lag(inc_prd), ~mean(.x, na.rm = TRUE),
                            .before = 3, .complete = FALSE),
    pres_mrn_12 = slide_dbl(lag(inc_mrn), ~mean(.x, na.rm = TRUE),
                            .before = 3, .complete = FALSE),
    # n_elec_pres: número de elecciones con datos válidos en la ventana
    # (útil para filtrar municipios con historia incompleta en robustez)
    n_elec_pres = slide_int(as.integer(!is.na(lag(inc_pan))), ~sum(.x),
                            .before = 3, .complete = FALSE)
  ) %>%
  ungroup()

# Filtrar solo años de encuesta para el join con gift_master2 / gift4
# as.integer(as.character()) porque gift_master$year es factor
years_survey <- sort(unique(as.integer(as.character(gift_master$year))))

inc_pres_elec <- inc_panel %>%
  filter(year %in% years_survey) %>%
  select(clave_mun, year, pres_pan_12, pres_pri_12,
         pres_prd_12, pres_mrn_12, n_elec_pres)

# gift_master2: gift_master + presencia histórica (dataset para modelos H3)

gift_master2 <- gift_master %>%
  mutate(year      = as.integer(as.character(year)),
         clave_mun = as.character(clave_mun)) %>%
  left_join(
    inc_pres_elec %>%
      mutate(year = as.integer(year), clave_mun = as.character(clave_mun)),
    by = c("year", "clave_mun")
  ) %>%
  mutate(year = factor(year), clave_mun = factor(clave_mun))

# ---- V-E (corregido): join rodante — elección más reciente antes del año de encuesta ----

# Pares únicos (municipio, año_encuesta) en gift_master
gift_key <- gift_master %>%
  mutate(
    year_int  = as.integer(as.character(year)),
    clave_mun = as.character(clave_mun)
  ) %>%
  distinct(clave_mun, year_int)

# Para cada par, encontrar la elección municipal más reciente <= año de encuesta
inc_pres_roll <- gift_key %>%
  left_join(
    inc_panel %>%
      select(clave_mun, elec_year = year,
             pres_pan_12, pres_pri_12, pres_prd_12, pres_mrn_12, n_elec_pres) %>%
      mutate(clave_mun = as.character(clave_mun)),
    by = "clave_mun",
    relationship = "many-to-many"
  ) %>%
  filter(elec_year <= year_int) %>%          # solo elecciones anteriores al año de encuesta
  group_by(clave_mun, year_int) %>%
  slice_max(elec_year, n = 1, with_ties = FALSE) %>%   # la más reciente
  ungroup() %>%
  select(clave_mun, year = year_int,
         pres_pan_12, pres_pri_12, pres_prd_12, pres_mrn_12, n_elec_pres)

# gift_master2: gift_master + presencia histórica con join rodante
gift_master2 <- gift_master %>%
  mutate(
    year      = as.integer(as.character(year)),
    clave_mun = as.character(clave_mun)
  ) %>%
  left_join(inc_pres_roll, by = c("year", "clave_mun")) %>%
  mutate(year = factor(year), clave_mun = factor(clave_mun))

# ******************************************************************************
#### VI. Muestra analítica: gift → Incumbente (H2 baseline) ####
# ******************************************************************************

gift2 <- gift_master %>%
  mutate(
    gift_bin  = as.integer(gift_bin),
    gift_bin  = if_else(gift_bin %in% c(0L, 1L), gift_bin, NA_integer_),
    voted     = as.integer(vote == 1),
    year      = as.factor(year),
    est       = as.factor(est),
    clave_mun = as.factor(clave_mun),
    w         = ponderador_norm
  )

gift2 <- gift2 %>%
  mutate(last_party_vote = factor(last_party_vote))

# Modelos FE: efecto de gift_bin sobre voto al incumbente
fe_pres <- felm(
  vote_incum_pres_tot ~ gift_bin + margin + edu + age + gen +
    p_id + type + ln_pop + last_party_vote + eth
  | year + clave_mun | 0 | clave_mun,
  data = gift2, weights = gift2$w
)

fe_gob <- felm(
  vote_incum_gob_tot ~ gift_bin + margin + edu + age + gen +
    p_id + type + ln_pop + last_party_vote + eth
  | year + clave_mun | 0 | clave_mun,
  data = gift2, weights = gift2$w
)

fe_mun <- felm(
  vote_incum_mun_tot ~ gift_bin + margin + edu + age + gen +
    p_id + type + ln_pop + last_party_vote + eth
  | year + clave_mun | 0 | clave_mun,
  data = gift2, weights = gift2$w
)

models_inc <- list(
  "Presidencial" = fe_pres,
  "Gubernatura"  = fe_gob,
  "Municipal"    = fe_mun
)

modelsummary(
  models_inc,
  output    = "latex",
  fmt       = 3,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_omit = "^(edu|age|p_id|type|eth|gen)",
  gof_map   = c("nobs", "r.squared", "adj.r.squared"),
  add_rows  = data.frame(
    term         = c("Controles sociodemográficos", "FE año",
                     "FE municipio", "EE cluster municipio",
                     "Ponderación muestral"),
    Presidencial = rep("Sí", 5),
    Gubernatura  = rep("Sí", 5),
    Municipal    = rep("Sí", 5)
  ),
  title = "Modelos FE: efecto de recibir dádiva sobre voto incumbente"
)

# ******************************************************************************
#### VII. Funciones helper ####
# ******************************************************************************

eff_n  <- function(w) (sum(w, na.rm = TRUE)^2) / sum(w^2, na.rm = TRUE)
fmt_pct <- function(x) scales::percent(x, accuracy = 0.1)
fmt_num <- function(x) scales::number(x, accuracy = 0.1, big.mark = ",")

# Proporciones ponderadas (distribución completa de una variable categórica)
wt_props <- function(data_srv, var, by = NULL) {
  var_quo <- enquo(var)
  if (is.null(by)) {
    data_srv %>%
      group_by(!!var_quo) %>%
      summarise(prop = survey_prop(vartype = "ci", na.rm = TRUE)) %>%
      ungroup()
  } else {
    by_quo <- if (is.character(by)) sym(by) else enquo(by)
    data_srv %>%
      group_by(!!by_quo, !!var_quo) %>%
      summarise(prop = survey_prop(vartype = "ci", na.rm = TRUE)) %>%
      ungroup()
  }
}

# Resumen continuo ponderado
wt_cont <- function(data_srv, var, by = NULL) {
  var_quo <- enquo(var)
  if (is.null(by)) {
    data_srv %>%
      summarise(
        mean = survey_mean(!!var_quo,  vartype = "ci", na.rm = TRUE),
        q25  = survey_quantile(!!var_quo, 0.25, vartype = "ci", na.rm = TRUE),
        med  = survey_quantile(!!var_quo, 0.50, vartype = "ci", na.rm = TRUE),
        q75  = survey_quantile(!!var_quo, 0.75, vartype = "ci", na.rm = TRUE)
      )
  } else {
    by_quo <- if (is.character(by)) sym(by) else enquo(by)
    data_srv %>%
      group_by(!!by_quo) %>%
      summarise(
        mean = survey_mean(!!var_quo,  vartype = "ci", na.rm = TRUE),
        q25  = survey_quantile(!!var_quo, 0.25, vartype = "ci", na.rm = TRUE),
        med  = survey_quantile(!!var_quo, 0.50, vartype = "ci", na.rm = TRUE),
        q75  = survey_quantile(!!var_quo, 0.75, vartype = "ci", na.rm = TRUE)
      ) %>%
      ungroup()
  }
}

# ******************************************************************************
#### VIII. Resumen de la muestra ####
# ******************************************************************************

### Tamaños muestrales, n efectivo y design effect por año
size_tbl <- gift %>%
  group_by(year) %>%
  summarise(
    N_unweighted = n(),
    W_sum        = sum(ponderador_norm, na.rm = TRUE),
    n_eff        = eff_n(ponderador_norm),
    deff         = N_unweighted / n_eff
  ) %>%
  ungroup()

gt(size_tbl) |>
  fmt_number(columns = c(N_unweighted, W_sum, n_eff, deff), decimals = 2) |>
  tab_header("Tamaño muestral, n_eff y design effect, por año") |>
  gtsave(file.path("muestra_n_neff_deff_por_anio.tex"))

# ******************************************************************************
#### IX. Perfil sociodemográfico ####
# ******************************************************************************

flatten_srvyr <- function(df) {
  df %>% mutate(across(where(~inherits(.x, "srvyr_result")), as.numeric))
}

### Distribuciones categóricas por año
sex_tab  <- wt_props(gift_srv, gen,  by = "year") %>% flatten_srvyr()
edu_tab  <- wt_props(gift_srv, edu,  by = "year") %>% flatten_srvyr()
type_tab <- wt_props(gift_srv, type, by = "year") %>% flatten_srvyr()
pid_tab  <- wt_props(gift_srv, p_id, by = "year") %>% flatten_srvyr()

gt(sex_tab) %>%
  fmt_percent(columns = prop, decimals = 2) %>%
  tab_header("Distribución de sexo (ponderada) por año") %>%
  gtsave(file.path("dist_sexo_por_anio.tex"))

gt(edu_tab) %>%
  fmt_percent(columns = prop, decimals = 2) %>%
  tab_header("Distribución de educación (ponderada) por año") %>%
  gtsave(file.path("dist_educacion_por_anio.tex"))

gt(type_tab) %>%
  fmt_percent(columns = prop, decimals = 2) %>%
  tab_header("Distribución de tipo de municipio (ponderada) por año") %>%
  gtsave(file.path("dist_urbanidad_por_anio.tex"))

gt(pid_tab) %>%
  fmt_percent(columns = prop, decimals = 2) %>%
  tab_header("Distribución de afinidad partidista (ponderada) por año") %>%
  gtsave(file.path("dist_pid_por_anio.tex"))

### Edad (variable categórica: 4 grupos)
age_tab <- wt_props(gift_srv, age, by = "year") %>% flatten_srvyr()

gt(age_tab) %>%
  fmt_percent(columns = prop, decimals = 2) %>%
  tab_header("Distribución de grupo de edad (ponderada) por año") %>%
  gtsave(file.path("dist_edad_por_ano.tex"))

# ******************************************************************************
#### X. Turnout y voto reportado ####
# ******************************************************************************

turnout_tab <- gift_srv %>%
  group_by(year) %>%
  summarise(turnout = survey_mean(vote, vartype = "ci", na.rm = TRUE)) %>%
  ungroup() %>%
  select(-matches("(_se|_low|_upp)$"))

vote_tabs <- gift_srv %>%
  group_by(year) %>%
  summarise(
    vote_pan = survey_mean(vote_PAN, vartype = "ci", na.rm = TRUE),
    vote_pri = survey_mean(vote_PRI, vartype = "ci", na.rm = TRUE),
    vote_prd = survey_mean(vote_PRD, vartype = "ci", na.rm = TRUE),
    vote_mrn = survey_mean(vote_MRN, vartype = "ci", na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(-matches("(_se|_low|_upp)$"))

gt(turnout_tab) %>%
  fmt_percent(columns = where(is.numeric), decimals = 2) %>%
  tab_header("Tasa de participación (ponderada) por año") %>%
  gtsave(file.path("turnout_por_ano.tex"))

gt(vote_tabs) %>%
  fmt_percent(columns = where(is.numeric), decimals = 2) %>%
  tab_header("Voto por partido (ponderado) por año") %>%
  gtsave(file.path("voto_partido_por_ano.tex"))

# ******************************************************************************
#### XI. Tabla de resumen global de covariables ####
# ******************************************************************************

vars_cat <- c("age", "gen", "type", "edu", "eth", "p_id")

var_labels <- c(
  age  = "Edad (grupo)",
  gen  = "Sexo",
  type = "Urbanidad",
  edu  = "Educación",
  eth  = "Etnicidad",
  p_id = "Identificación partidista"
)

tab_global <- map_dfr(vars_cat, function(v) {
  gift_srv %>%
    mutate(categoria = as.character(.data[[v]])) %>%
    filter(!is.na(categoria)) %>%
    group_by(categoria) %>%
    summarise(
      n_unw = n(),
      p     = survey_prop(vartype = NULL, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(variable = var_labels[[v]] %||% v, .before = 1)
})

gt(tab_global, groupname_col = "variable") %>%
  cols_label(
    categoria = "Categoría",
    n_unw     = "N (sin ponderar)",
    p         = "% (ponderado)"
  ) %>%
  fmt_number(columns = n_unw,  decimals = 0) %>%
  fmt_percent(columns = p,     decimals = 1) %>%
  tab_header("Distribución global de covariables (ponderada)") %>%
  opt_row_striping() %>%
  gtsave("dist_global_covariables.tex")

# ******************************************************************************
#### XII. Distribución del tratamiento ####
# ******************************************************************************

### Prevalencia general de gift_bin (ponderada) por año
gift_prev <- gift_srv %>%
  group_by(year) %>%
  summarise(gift = survey_mean(gift_bin, vartype = "ci", na.rm = TRUE)) %>%
  ungroup()

gt(gift_prev) %>%
  fmt_percent(columns = where(is.numeric), decimals = 2) %>%
  tab_header("Prevalencia de dádivas (ponderada) por año") %>%
  gtsave(file.path("gift_prevalencia_por_ano.tex"))

### Prevalencia por partido (proporción del total de la muestra)
gift_party <- gift_srv %>%
  group_by(year) %>%
  summarise(
    PAN   = survey_mean(gift_PAN,   vartype = NULL, na.rm = TRUE),
    PRI   = survey_mean(gift_PRI,   vartype = NULL, na.rm = TRUE),
    PRD   = survey_mean(gift_PRD,   vartype = NULL, na.rm = TRUE),
    MRN   = survey_mean(gift_MRN,   vartype = NULL, na.rm = TRUE),
    Otros = survey_mean(gift_other, vartype = NULL, na.rm = TRUE)
  ) %>%
  ungroup()

gt(gift_party) %>%
  fmt_percent(columns = where(is.numeric), decimals = 2) %>%
  tab_header("Prevalencia de dádivas (ponderada) por partido") %>%
  gtsave(file.path("gift_prevalencia_por_partido.tex"))

### Gráfico apilado: distribución de gift por partido y año
gift_party %>%
  pivot_longer(cols = -year, names_to = "partido", values_to = "est") %>%
  mutate(partido = factor(partido,
                          levels = c("PAN", "PRI", "PRD", "MRN", "Otros"),
                          labels = c("PAN", "PRI", "PRD", "Morena", "Otros"))) %>%
  ggplot(aes(x = year, y = est, fill = partido)) +
  geom_col(position = "fill", color = "white") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(end = 0.9) +
  labs(x = "Año", y = "% dentro de año", fill = "Partido",
       title = "¿Quién entregó dádivas? Distribución ponderada por año") +
  theme_minimal(base_size = 12)

### Estadísticos globales (pool completo)

# A) Prevalencia global de gift_bin
pct_gift_global <- gift_srv %>%
  summarise(p = survey_mean(gift_bin, vartype = "ci", na.rm = TRUE)) %>%
  transmute(pct = 100 * p, lo = 100 * p_low, hi = 100 * p_upp)

pct_gift_global

# B) Prevalencia global por partido (% del total de la muestra)
party_vars <- c(
  "PAN"   = "gift_PAN",
  "PRI"   = "gift_PRI",
  "PRD"   = "gift_PRD",
  "MRN"   = "gift_MRN",
  "Otros" = "gift_other"
)

pct_gift_by_party <- imap_dfr(party_vars, \(v, partido) {
  gift_srv %>%
    summarise(p = survey_mean(.data[[v]], vartype = "ci", na.rm = TRUE)) %>%
    transmute(partido = partido,
              pct = 100 * p,
              lo  = 100 * p_low,
              hi  = 100 * p_upp)
})

pct_gift_by_party

# ******************************************************************************
#### XIII. Targeting sociodemográfico por partido ####
# ******************************************************************************

# Etiquetas de categorías (ajustar si la codificación original difiere)
cat_labels <- list(
  age  = c("1" = "18--25",
           "2" = "26--40",
           "3" = "40--60",
           "4" = "60+"),
  edu  = c("0" = "Sin esc./\nPrim. inc.",
           "1" = "Primaria",
           "2" = "Secundaria",
           "3" = "Media sup.",
           "4" = "Superior+"),
  type = c("1" = "Rural",
           "2" = "Mixto",
           "3" = "Urbana")
)

# Parámetros por partido
party_params <- list(
  PAN = list(var   = "gift_PAN",
             color = rgb(  5,  51, 141, maxColorValue = 255),
             label = "PAN",
             file  = "target_PAN.pdf"),
  PRI = list(var   = "gift_PRI",
             color = rgb(  0, 146,  63, maxColorValue = 255),
             label = "PRI",
             file  = "target_PRI.pdf"),
  PRD = list(var   = "gift_PRD",
             color = rgb(255, 203,   1, maxColorValue = 255),
             label = "PRD",
             file  = "target_PRD.pdf"),
  MRN = list(var   = "gift_MRN",
             color = rgb(181,  38,  30, maxColorValue = 255),
             label = "Morena",
             file  = "target_MRN.pdf")
)

# Función: summary ponderado con etiquetas de categoría
make_targeting_summary <- function(treat_var) {
  gift %>%
    mutate(treat = as.integer(.data[[treat_var]])) %>%
    pivot_longer(cols      = c(edu, type, age),
                 names_to  = "variable",
                 values_to = "categoria") %>%
    filter(!is.na(categoria), !is.na(treat), !is.na(ponderador_norm)) %>%
    # Recode numérico → etiqueta (antes de renombrar variable)
    mutate(categoria = case_when(
      variable == "age"  ~ cat_labels$age[as.character(categoria)],
      variable == "edu"  ~ cat_labels$edu[as.character(categoria)],
      variable == "type" ~ cat_labels$type[as.character(categoria)],
      TRUE ~ as.character(categoria)
    )) %>%
    group_by(variable, categoria) %>%
    summarise(
      perc_gift = 100 * weighted.mean(treat, w = ponderador_norm, na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    mutate(variable = dplyr::recode(as.character(variable),
                                    age  = "Edad",
                                    edu  = "Educación",
                                    type = "Entorno"))
}

# Construir todos los summaries
summaries <- lapply(party_params, function(p) make_targeting_summary(p$var))

# Medias globales ponderadas
global_means <- sapply(names(party_params), function(nm) {
  var <- party_params[[nm]]$var
  100 * weighted.mean(as.integer(gift[[var]]), w = gift$ponderador_norm, na.rm = TRUE)
})

# Escala común en eje Y
y_max <- ceiling(max(sapply(summaries, function(s) max(s$perc_gift, na.rm = TRUE)))) + 1

# Generar un gráfico por partido
for (nm in names(party_params)) {
  p     <- party_params[[nm]]
  summ  <- summaries[[nm]]
  gmean <- global_means[[nm]]
  
  p_plot <- ggplot(summ, aes(x = categoria, y = perc_gift)) +
    geom_col(fill = p$color) +
    facet_wrap(~variable, scales = "free_x") +
    geom_hline(yintercept = gmean, linetype = "dashed",
               color = "red", linewidth = 1) +
    coord_cartesian(ylim = c(0, y_max)) +
    labs(
      x        = "Categoría",
      y        = paste0("% que recibió regalo del ", p$label),
      #title    = paste0("Targeting del ", p$label, " (regalo reportado)"),
      subtitle = paste0("Promedio global (", p$label, "): ",
                        round(gmean, 1), "%")
    ) +
    theme_bw(base_family = "Times New Roman", base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(filename = p$file,
         plot     = p_plot,
         width    = 7.5,
         height   = 4.8,
         device   = "pdf")
}

# ******************************************************************************
#### XIV. Correlaciones ####
# ******************************************************************************

# Variables para la matriz: gift y turnout únicamente
# (los votos por partido son mutuamente excluyentes → correlaciones mecánicas,
#  su relación con el regalo se reporta en el heatmap de abajo)
corr_df <- gift %>%
  transmute(
    `Dádiva`       = as.numeric(gift_bin),
    `Participación` = as.integer(as.numeric(vote)),
    `Dádiva PAN`   = as.numeric(gift_PAN),
    `Dádiva PRI`   = as.numeric(gift_PRI),
    `Dádiva PRD`   = as.numeric(gift_PRD),
    `Dádiva MRN`   = as.numeric(gift_MRN)
  )

# Pesos globales
w    <- gift$ponderador_norm
keep <- complete.cases(corr_df) & !is.na(w)
corr_df <- corr_df[keep, ]
w       <- w[keep]

# Correlación ponderada
cor_mat <- cov.wt(corr_df, wt = w, cor = TRUE,
                  center = TRUE, method = "unbiased")$cor

# Guardar corrplot (base R graphics → pdf() directo, no ggsave)
pdf("corrplot_main.pdf", width = 7.5, height = 6)
par(family = "Times New Roman")
corrplot(
  cor_mat,
  method      = "circle",
  type        = "lower",
  diag        = FALSE,
  addCoef.col = "black",
  number.cex  = 0.85,
  tl.col      = "black",
  tl.srt      = 45
)
dev.off()

# ******************************************************************************
#### XV. Matriz de probabilidades condicionales ####
# ******************************************************************************

gift_vote <- gift_srv %>%
  mutate(
    gift_party = case_when(
      gift_PAN   == 1 ~ "PAN",
      gift_PRI   == 1 ~ "PRI",
      gift_PRD   == 1 ~ "PRD",
      gift_MRN   == 1 ~ "MRN",
      gift_other == 1 ~ "Otros",
      TRUE            ~ "Sin regalo"
    )
  ) %>%
  group_by(gift_party) %>%
  summarise(
    vote_PAN   = survey_mean(vote_PAN,   na.rm = TRUE),
    vote_PRI   = survey_mean(vote_PRI,   na.rm = TRUE),
    vote_PRD   = survey_mean(vote_PRD,   na.rm = TRUE),
    vote_MRN   = survey_mean(vote_MRN,   na.rm = TRUE),
    vote_Otros = survey_mean(vote_other, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols      = matches("^vote_(MRN|PAN|PRD|PRI|Otros)$"),
    names_to  = "vote_party",
    values_to = "p"
  ) %>%
  mutate(
    pct        = 100 * p,
    vote_party = dplyr::recode(vote_party,
                               vote_MRN   = "MRN",
                               vote_PAN   = "PAN",
                               vote_PRD   = "PRD",
                               vote_PRI   = "PRI",
                               vote_Otros = "Otros"),
    gift_party = factor(gift_party,
                        levels = c("Sin regalo", "PAN", "PRI", "PRD", "MRN", "Otros")),
    vote_party = factor(vote_party,
                        levels = c("Otros", "MRN", "PRD", "PRI", "PAN"))
  )

p_heat <- ggplot(gift_vote, aes(x = vote_party, y = gift_party, fill = pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.1f", pct)), size = 3) +
  scale_fill_gradient(low = "white", high = "steelblue",
                      labels = scales::label_percent(scale = 1)) +
  labs(x = "Voto reportado",
       y = "Regalo reportado (partido)",
       fill = "%") +
  theme_minimal(base_size = 12) +
  theme(text = element_text(family = "Times New Roman"))

ggsave("heat_map.pdf", p_heat, width = 7.5, height = 4.8, device = "pdf")

# ******************************************************************************
# ----- XVI. H1: Gift party -> Vote party -----
# ******************************************************************************

gift3 <- gift2 %>%
  mutate(
    yr = as.integer(as.character(year)),
    
    # 1) DVs incondicionales (totales)
    vote_PAN   = vote_PAN_tot,
    vote_PRI   = vote_PRI_tot,
    vote_PRD   = vote_PRD_tot,
    vote_MRN   = vote_MRN_tot,
    vote_other = vote_OTH_tot,
    
    # 2) Variables de coalición
    gift_panprd   = if_else(yr == 2018 & (gift_PAN == 1L | gift_PRD == 1L), 1L, 0L),
    gift_opos2024 = if_else(yr == 2024 & (gift_PAN == 1L | gift_PRI == 1L | gift_PRD == 1L), 1L, 0L),
    
    vote_panprd   = if_else(yr == 2018 & (vote_PAN_tot == 1L | vote_PRD_tot == 1L), 1L, 0L),
    vote_opos2024 = if_else(yr == 2024 & (vote_PAN_tot == 1L | vote_PRI_tot == 1L | vote_PRD_tot == 1L), 1L, 0L),
    
    # 3) Apagar dummies individuales en años de coalición (evitar colinealidad)
    gift_PAN = if_else(yr %in% c(2018, 2024), 0L, gift_PAN),
    gift_PRI = if_else(yr == 2024,            0L, gift_PRI),
    gift_PRD = if_else(yr %in% c(2018, 2024), 0L, gift_PRD),
    
    vote_PAN = if_else(yr %in% c(2018, 2024), 0L, vote_PAN),
    vote_PRI = if_else(yr == 2024,            0L, vote_PRI),
    vote_PRD = if_else(yr %in% c(2018, 2024), 0L, vote_PRD)
  )

# ******************************************************************************
### Modelo final: FE año + municipio, EE cluster municipio ###
# ******************************************************************************

pan_fe <- felm(
  vote_PAN ~ gift_PAN + gift_PRI + gift_PRD + gift_MRN + gift_other +
    gift_panprd + gift_opos2024 +
    margin + edu + age + gen + p_id + type + ln_pop + last_party_vote + eth
  | year + clave_mun | 0 | clave_mun,
  data = gift3, weights = gift3$ponderador_norm
)
summary(pan_fe)

pri_fe <- felm(
  vote_PRI ~ gift_PAN + gift_PRI + gift_PRD + gift_MRN + gift_other +
    gift_panprd + gift_opos2024 +
    margin + edu + age + gen + p_id + type + ln_pop + last_party_vote + eth
  | year + clave_mun | 0 | clave_mun,
  data = gift3, weights = gift3$ponderador_norm
)
summary(pri_fe)

prd_fe <- felm(
  vote_PRD ~ gift_PAN + gift_PRI + gift_PRD + gift_MRN + gift_other +
    gift_panprd + gift_opos2024 +
    margin + edu + age + gen + p_id + type + ln_pop + last_party_vote + eth
  | year + clave_mun | 0 | clave_mun,
  data = gift3, weights = gift3$ponderador_norm
)
summary(prd_fe)

mrn_fe <- felm(
  vote_MRN ~ gift_PAN + gift_PRI + gift_PRD + gift_MRN + gift_other +
    gift_panprd + gift_opos2024 +
    margin + edu + age + gen + p_id + type + ln_pop + last_party_vote + eth
  | year + clave_mun | 0 | clave_mun,
  data = gift3, weights = gift3$ponderador_norm
)
summary(mrn_fe)

mods_party <- list(
  "PAN"    = pan_fe,
  "PRI"    = pri_fe,
  "PRD"    = prd_fe,
  "Morena" = mrn_fe
)

coef_map <- c(
  "gift_PAN"      = "Dádiva PAN",
  "gift_PRI"      = "Dádiva PRI",
  "gift_PRD"      = "Dádiva PRD",
  "gift_MRN"      = "Dádiva Morena",
  "gift_other"    = "Dádiva Otros",
  "gift_panprd"   = "Dádiva coalición PAN-PRD (2018)",
  "gift_opos2024" = "Dádiva coalición opositora (2024)"
)

tab_party_short <- modelsummary(
  mods_party,
  output    = "latex",
  fmt       = 3,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map  = coef_map,
  gof_map   = c("nobs","r.squared","adj.r.squared"),
  add_rows  = data.frame(
    term   = c("Controles sociodemográficos","FE año","FE municipio",
               "EE cluster municipio","Ponderación muestral"),
    PAN    = rep("Sí", 5),
    PRI    = rep("Sí", 5),
    PRD    = rep("Sí", 5),
    Morena = rep("Sí", 5)
  ),
  title = "Efecto de recibir dádivas (por partido) sobre el voto por partido"
)

# Coefplot (modelsummary::modelplot, compatible con felm)
modelplot(
  mods_party,
  coef_map = coef_map,
  conf_level = 0.95
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Efecto de recibir dádiva (por partido) sobre voto por partido") +
  theme_bw(base_family = "Times New Roman")


# ******************************************************************************
# ---- XVII. H2: Coincidencia dádiva × incumbente municipal → voto (WLS-FE) ----
# ******************************************************************************
# Especificación correcta: efectos principales de dádiva e incumbencia
# + interacción (match_*). El coeficiente de match_P es el "coincidence bonus"
# neto — efecto ADICIONAL de la dádiva cuando el partido dador es incumbente.

gift3 <- gift3 %>%
  mutate(
    # Interacciones (H2)
    match_pan = if_else(gift_PAN == 1L & inc_mun == 1L, 1L, 0L, missing = 0L),
    match_pri = if_else(gift_PRI == 1L & inc_mun == 2L, 1L, 0L, missing = 0L),
    match_prd = if_else(gift_PRD == 1L & inc_mun == 3L, 1L, 0L, missing = 0L),
    match_mrn = if_else(gift_MRN == 1L & inc_mun == 4L, 1L, 0L, missing = 0L),
    # Efectos principales de incumbencia (términos constitutivos)
    inc_pan   = as.integer(inc_mun == 1L),
    inc_pri   = as.integer(inc_mun == 2L),
    inc_prd   = as.integer(inc_mun == 3L),
    inc_mrn   = as.integer(inc_mun == 4L)
  )

# --- WLS-LPM con FE año + municipio, cluster municipio ----------------------

m_pan_h2 <- felm(
  vote_PAN_tot ~
    gift_PAN + gift_PRI + gift_PRD + gift_MRN + gift_other + gift_panprd + gift_opos2024 +       # ef. principal dádiva
    inc_pan  + inc_pri  + inc_prd  + inc_mrn  +        # ef. principal incumbencia
    match_pan + match_pri + match_prd + match_mrn +    # interacción: H2
    margin + edu + age + gen + p_id + type + ln_pop + last_party_vote + eth |
    year + clave_mun | 0 | clave_mun,
  data    = gift3,
  weights = gift3$ponderador_norm
)
summary(m_pan_h2)

m_pri_h2 <- felm(
  vote_PRI_tot ~
    gift_PAN + gift_PRI + gift_PRD + gift_MRN + gift_other + gift_panprd + gift_opos2024 +
    inc_pan  + inc_pri  + inc_prd  + inc_mrn  +
    match_pan + match_pri + match_prd + match_mrn +
    margin + edu + age + gen + p_id + type + ln_pop + last_party_vote + eth |
    year + clave_mun | 0 | clave_mun,
  data    = gift3,
  weights = gift3$ponderador_norm
)
summary(m_pri_h2)

m_prd_h2 <- felm(
  vote_PRD_tot ~
    gift_PAN + gift_PRI + gift_PRD + gift_MRN + gift_other + gift_panprd + gift_opos2024 +
    inc_pan  + inc_pri  + inc_prd  + inc_mrn  +
    match_pan + match_pri + match_prd + match_mrn +
    margin + edu + age + gen + p_id + type + ln_pop + last_party_vote + eth |
    year + clave_mun | 0 | clave_mun,
  data    = gift3,
  weights = gift3$ponderador_norm
)
summary(m_prd_h2)

m_mrn_h2 <- felm(
  vote_MRN_tot ~
    gift_PAN + gift_PRI + gift_PRD + gift_MRN + gift_other + gift_panprd + gift_opos2024 +
    inc_pan  + inc_pri  + inc_prd  + inc_mrn  +
    match_pan + match_pri + match_prd + match_mrn +
    margin + edu + age + gen + p_id + type + ln_pop + last_party_vote + eth |
    year + clave_mun | 0 | clave_mun,
  data    = gift3,
  weights = gift3$ponderador_norm
)
summary(m_mrn_h2)

# --- Tabla de resultados (reporta solo match_*; el resto queda en apéndice) --

mods_h2 <- list(
  "PAN"    = m_pan_h2,
  "PRI"    = m_pri_h2,
  "PRD"    = m_prd_h2,
  "Morena" = m_mrn_h2
)

coef_map_h2 <- c(
  # Bono de coincidencia (H2) — coeficientes principales de la tabla
  "match_pan" = "D\\'{a}diva PAN $\\times$ incumbente PAN",
  "match_pri" = "D\\'{a}diva PRI $\\times$ incumbente PRI",
  "match_prd" = "D\\'{a}diva PRD $\\times$ incumbente PRD",
  "match_mrn" = "D\\'{a}diva Morena $\\times$ incumbente Morena",
  # Efectos principales dádiva (referencia H1)
  "gift_PAN"  = "D\\'{a}diva PAN (sin incumbencia)",
  "gift_PRI"  = "D\\'{a}diva PRI (sin incumbencia)",
  "gift_PRD"  = "D\\'{a}diva PRD (sin incumbencia)",
  "gift_MRN"  = "D\\'{a}diva Morena (sin incumbencia)"
)

tab_h2 <- modelsummary(
  mods_h2,
  output    = "latex",
  fmt       = 3,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map  = coef_map_h2,      # inc_* omitidos aquí; aparecen en apéndice
  gof_map   = list(
    list(raw = "nobs",          clean = "N\\'{u}m.\\ observaciones", fmt = 0),
    list(raw = "r.squared",     clean = "$R^2$",                     fmt = 3),
    list(raw = "adj.r.squared", clean = "$R^2$ ajustado",            fmt = 3)
  ),
  add_rows = data.frame(
    term   = c("Ef. principales incumbencia", "Controles socio-demogr\\'{a}ficos",
               "Efectos fijos: a\\~{n}o",     "Efectos fijos: municipio",
               "EE: cl\\'{u}ster municipio",  "Ponderadores"),
    PAN    = rep("S\\'{i}", 6),
    PRI    = rep("S\\'{i}", 6),
    PRD    = rep("S\\'{i}", 6),
    Morena = rep("S\\'{i}", 6),
    check.names = FALSE
  )
)

tab_h2

# ******************************************************************************
# ---- XVIII. H3: Arraigo histórico × dádiva → voto (WLS-FE) -----------------
# ******************************************************************************
# pres_P_12 = proporción de elecciones en la ventana previa de 12 años en que
# el partido P gobernó el municipio (construido en Sección V-E, inc_pres_elec).
#
# Coeficiente clave de H3: gift_P:pres_P_12 > 0
# (la dádiva tiene mayor efecto donde el partido tiene más arraigo histórico).
#
# NOTA: cada modelo es por partido (PAN, PRI, PRD, Morena) y solo incluye la
# interacción focal; los efectos principales y dádivas de otros partidos se
# controlan pero no se reportan en la tabla principal.

# --- Muestra analítica para H3: gift2 + pres_*_12 ---
# Se une inc_pres_elec (construido en V-E) sobre la muestra analítica gift2,
# usando claves enteras para evitar conflictos de factor/character.

gift4 <- gift2 %>%
  mutate(year_k = as.integer(as.character(year)),
         mun_k  = as.character(clave_mun)) %>%
  left_join(
    inc_pres_roll %>%                          # <-- cambiar aquí
      transmute(year_k      = as.integer(year),
                mun_k       = as.character(clave_mun),
                pres_pan_12, pres_pri_12,
                pres_prd_12, pres_mrn_12, n_elec_pres),
    by = c("year_k", "mun_k")
  ) %>%
  select(-year_k, -mun_k)

# --- WLS-LPM con FE año + municipio, cluster municipio ----------------------
# Dádivas de otros partidos incluidas como controles de competencia.

m_pan_h3 <- felm(
  vote_PAN_tot ~ gift_PAN * pres_pan_12 +
    gift_PRI + gift_PRD + gift_MRN + gift_other +
    margin + edu + age + gen + p_id + type + ln_pop + last_party_vote + eth |
    year + clave_mun | 0 | clave_mun,
  data    = gift4,
  weights = gift4$ponderador_norm
)
summary(m_pan_h3)

m_pri_h3 <- felm(
  vote_PRI_tot ~ gift_PRI * pres_pri_12 +
    gift_PAN + gift_PRD + gift_MRN + gift_other +
    margin + edu + age + gen + p_id + type + ln_pop + last_party_vote + eth |
    year + clave_mun | 0 | clave_mun,
  data    = gift4,
  weights = gift4$ponderador_norm
)
summary(m_pri_h3)

m_prd_h3 <- felm(
  vote_PRD_tot ~ gift_PRD * pres_prd_12 +
    gift_PAN + gift_PRI + gift_MRN + gift_other +
    margin + edu + age + gen + p_id + type + ln_pop + last_party_vote + eth |
    year + clave_mun | 0 | clave_mun,
  data    = gift4,
  weights = gift4$ponderador_norm
)
summary(m_prd_h3)

m_mrn_h3 <- felm(
  vote_MRN_tot ~ gift_MRN * pres_mrn_12 +
    gift_PAN + gift_PRI + gift_PRD + gift_other +
    margin + edu + age + gen + p_id + type + ln_pop + last_party_vote + eth |
    year + clave_mun | 0 | clave_mun,
  data    = gift4,
  weights = gift4$ponderador_norm
)
summary(m_mrn_h3)

# --- Tabla de resultados (solo interacciones + efectos principales) ---

mods_h3 <- list(
  "PAN"    = m_pan_h3,
  "PRI"    = m_pri_h3,
  "PRD"    = m_prd_h3,
  "Morena" = m_mrn_h3
)

# Tabla compacta: solo los 3 coeficientes de interés por partido
# Cada columna muestra únicamente las filas de su partido; las demás quedan vacías.
coef_map_h3 <- c(
  "gift_PAN"             = "D\\'{a}diva PAN",
  "pres_pan_12"          = "Arraigo PAN (12 a\\~{n}os)",
  "gift_PAN:pres_pan_12" = "D\\'{a}diva $\\times$ Arraigo (PAN)",
  "gift_PRI"             = "D\\'{a}diva PRI",
  "pres_pri_12"          = "Arraigo PRI (12 a\\~{n}os)",
  "gift_PRI:pres_pri_12" = "D\\'{a}diva $\\times$ Arraigo (PRI)",
  "gift_PRD"             = "D\\'{a}diva PRD",
  "pres_prd_12"          = "Arraigo PRD (12 a\\~{n}os)",
  "gift_PRD:pres_prd_12" = "D\\'{a}diva $\\times$ Arraigo (PRD)",
  "gift_MRN"             = "D\\'{a}diva Morena",
  "pres_mrn_12"          = "Arraigo Morena (12 a\\~{n}os)",
  "gift_MRN:pres_mrn_12" = "D\\'{a}diva $\\times$ Arraigo (Morena)"
)

tab_h3 <- modelsummary(
  mods_h3,
  output    = "latex",
  fmt       = 3,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map  = coef_map_h3,
  gof_map   = list(
    list(raw = "nobs",          clean = "N\\'{u}m.\\ observaciones", fmt = 0),
    list(raw = "r.squared",     clean = "$R^2$",                     fmt = 3),
    list(raw = "adj.r.squared", clean = "$R^2$ ajustado",            fmt = 3)
  ),
  add_rows = data.frame(
    term   = c("D\\'{a}divas otros partidos", "Controles socio-demogr\\'{a}ficos",
               "Efectos fijos: a\\~{n}o",      "Efectos fijos: municipio",
               "EE: cl\\'{u}ster municipio",   "Ponderadores"),
    PAN    = rep("S\\'{i}", 6),
    PRI    = rep("S\\'{i}", 6),
    PRD    = rep("S\\'{i}", 6),
    Morena = rep("S\\'{i}", 6),
    check.names = FALSE
  )
)

tab_h3

# ******************************************************************************
# ---- XIX. H4: Dádiva por partido → identificación partidista (WLS-FE) ------
# ******************************************************************************
# pid_P = 1 si el encuestado se identifica con el partido P.
# Nota: p_id NO se incluye como control (es la variable dependiente).

gift3 <- gift3 %>%
  mutate(
    pid_PAN = as.integer(as.character(p_id) == "1"),
    pid_PRI = as.integer(as.character(p_id) == "2"),
    pid_PRD = as.integer(as.character(p_id) == "3"),
    pid_MRN = as.integer(as.character(p_id) == "4"),
    pid_none = as.integer(as.character(p_id) == "0"),
    pid_other = as.integer(as.character(p_id) == "5")
  )

# --- WLS-LPM con FE año + municipio, cluster municipio ----------------------
# Se incluyen dádivas de todos los partidos + variables de coalición (2018, 2024)
# para consistencia con H1. Controles sociodemográficos completos sin p_id.

m_pan_h4 <- felm(
  pid_PAN ~ gift_PAN + gift_PRI + gift_PRD + gift_MRN + gift_other +
    gift_panprd + gift_opos2024 +
    margin + edu + age + gen + type + ln_pop + eth + last_party_vote |
    year + clave_mun | 0 | clave_mun,
  data    = gift3,
  weights = gift3$ponderador_norm
)
summary(m_pan_h4)

m_pri_h4 <- felm(
  pid_PRI ~ gift_PAN + gift_PRI + gift_PRD + gift_MRN + gift_other +
    gift_panprd + gift_opos2024 +
    margin + edu + age + gen + type + ln_pop + eth + last_party_vote |
    year + clave_mun | 0 | clave_mun,
  data    = gift3,
  weights = gift3$ponderador_norm
)
summary(m_pri_h4)

m_prd_h4 <- felm(
  pid_PRD ~ gift_PAN + gift_PRI + gift_PRD + gift_MRN + gift_other +
    gift_panprd + gift_opos2024 +
    margin + edu + age + gen + type + ln_pop + eth + last_party_vote |
    year + clave_mun | 0 | clave_mun,
  data    = gift3,
  weights = gift3$ponderador_norm
)
summary(m_prd_h4)

m_mrn_h4 <- felm(
  pid_MRN ~ gift_PAN + gift_PRI + gift_PRD + gift_MRN + gift_other +
    gift_panprd + gift_opos2024 +
    margin + edu + age + gen + type + ln_pop + eth + last_party_vote |
    year + clave_mun | 0 | clave_mun,
  data    = gift3,
  weights = gift3$ponderador_norm
)
summary(m_mrn_h4)

# --- Tabla de resultados ---

mods_h4 <- list(
  "PID PAN"    = m_pan_h4,
  "PID PRI"    = m_pri_h4,
  "PID PRD"    = m_prd_h4,
  "PID Morena" = m_mrn_h4
)

coef_map_h4 <- c(
  "gift_PAN"          = "D\\'{a}diva PAN",
  "gift_PRI"          = "D\\'{a}diva PRI",
  "gift_PRD"          = "D\\'{a}diva PRD",
  "gift_MRN"          = "D\\'{a}diva Morena",
  "gift_coal_PAN_PRD" = "D\\'{a}diva coalici\\'{o}n PAN-PRD (2018)",
  "gift_coal_OPO"     = "D\\'{a}diva coalici\\'{o}n opositora (2024)"
)

tab_h4 <- modelsummary(
  mods_h4,
  output    = "latex",
  fmt       = 3,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map  = coef_map_h4,
  gof_map   = list(
    list(raw = "nobs",          clean = "N\\'{u}m.\\ observaciones", fmt = 0),
    list(raw = "r.squared",     clean = "$R^2$",                     fmt = 3),
    list(raw = "adj.r.squared", clean = "$R^2$ ajustado",            fmt = 3)
  ),
  add_rows = data.frame(
    term         = c("Controles socio-demogr\\'{a}ficos",
                     "Efectos fijos: a\\~{n}o",
                     "Efectos fijos: municipio",
                     "EE: cl\\'{u}ster municipio",
                     "Ponderadores"),
    `PID PAN`    = rep("S\\'{i}", 5),
    `PID PRI`    = rep("S\\'{i}", 5),
    `PID PRD`    = rep("S\\'{i}", 5),
    `PID Morena` = rep("S\\'{i}", 5),
    check.names  = FALSE
  )
)

tab_h4

# ******************************************************************************
# ---- XX. H5: Dádiva / conocimiento / exclusión → participación (WLS-FE) ----
# ******************************************************************************
# gift_bin  = recibió cualquier dádiva (0/1)
# know_bin  = conoce distribución de dádivas en su municipio (0/1)
# excluded  = conoce dádivas pero no recibió (know_bin=1 & gift_bin=0)
#
# Hipótesis H5: recibir una dádiva no tiene efecto significativo sobre turnout.
# Los modelos 2 y 3 prueban mecanismos adicionales: conocimiento y exclusión.

# --- Construir variables de H5 sobre gift2 -----------------------------------

gift2 <- gift2 %>%
  mutate(
    # gift_bin: recibió alguna dádiva de cualquier partido
    # (si ya existe como variable raw en gift_master, omitir esta línea)
    gift_bin = if_else(
      rowSums(is.na(select(., gift_PAN, gift_PRI, gift_PRD, gift_MRN, gift_other))) == 5L,
      NA_integer_,
      as.integer(gift_PAN == 1L | gift_PRI == 1L |
                   gift_PRD == 1L | gift_MRN == 1L | gift_other == 1L)
    ),
    # know_bin: sabe de la distribución de dádivas en su municipio
    know_bin = case_when(
      is.na(know)     ~ NA_integer_,
      know == TRUE    ~ 1L,
      TRUE            ~ 0L
    ),
    # excluded: sabe de dádivas pero no recibió ninguna
    excluded = if_else(
      !is.na(gift_bin) & !is.na(know_bin),
      as.integer(gift_bin == 0L & know_bin == 1L),
      NA_integer_
    )
  )

# --- WLS-LPM con FE año + municipio, cluster municipio ----------------------

# (1) Efecto directo de recibir dádiva sobre turnout
m_gift_h5 <- felm(
  vote ~ gift_bin +
    margin + edu + age + gen + p_id + type + ln_pop + last_party_vote + eth |
    year + clave_mun | 0 | clave_mun,
  data    = gift2,
  weights = gift2$ponderador_norm
)
summary(m_gift_h5)

# (2) Efecto adicional de conocer la distribución de dádivas
m_know_h5 <- felm(
  vote ~ know_bin + gift_bin +
    margin + edu + age + gen + p_id + type + ln_pop + last_party_vote + eth |
    year + clave_mun | 0 | clave_mun,
  data    = gift2,
  weights = gift2$ponderador_norm
)
summary(m_know_h5)

# (3) Efecto de la exclusión: sabe pero no recibió
m_exclu_h5 <- felm(
  vote ~ excluded + gift_bin + know_bin +
    margin + edu + age + gen + p_id + type + ln_pop + last_party_vote + eth |
    year + clave_mun | 0 | clave_mun,
  data    = gift2,
  weights = gift2$ponderador_norm
)
summary(m_exclu_h5)

# Submuestra con información de conocimiento clientelar (igual que M2 y M3)
gift2_sub <- gift2 %>% filter(!is.na(know_bin))

# (4) Efecto de recibir dádiva solo en la submuestra (sin controles de know/exclusión)
m_gift_sub_h5 <- felm(
  vote ~ gift_bin +
    margin + edu + age + gen + p_id + type + ln_pop + last_party_vote + eth |
    year + clave_mun | 0 | clave_mun,
  data    = gift2_sub,
  weights = gift2_sub$ponderador_norm
)
summary(m_gift_sub_h5)

# --- Tabla de resultados -----------------------------------------------------

mods_h5 <- list(
  "(1) D\\'{a}diva"   = m_gift_h5,
  "(2) Conocimiento"  = m_know_h5,
  "(3) Exclusi\\'{o}n" = m_exclu_h5
)

coef_map_h5 <- c(
  "gift_bin"  = "Recibi\\'{o} d\\'{a}diva",
  "know_bin"  = "Conoce distribuci\\'{o}n de d\\'{a}divas",
  "excluded"  = "Excluido (conoce, no recibi\\'{o})"
)

tab_h5 <- modelsummary(
  mods_h5,
  output    = "latex",
  fmt       = 3,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map  = coef_map_h5,
  gof_map   = list(
    list(raw = "nobs",          clean = "N\\'{u}m.\\ observaciones", fmt = 0),
    list(raw = "r.squared",     clean = "$R^2$",                     fmt = 3),
    list(raw = "adj.r.squared", clean = "$R^2$ ajustado",            fmt = 3)
  ),
  add_rows = data.frame(
    term               = c("Controles socio-demogr\\'{a}ficos",
                           "Efectos fijos: a\\~{n}o",
                           "Efectos fijos: municipio",
                           "EE: cl\\'{u}ster municipio",
                           "Ponderadores"),
    `(1) Dádiva`       = rep("S\\'{i}", 5),
    `(2) Conocimiento` = rep("S\\'{i}", 5),
    `(3) Exclusión`    = rep("S\\'{i}", 5),
    check.names        = FALSE
  )
)

tab_h5

# ******************************************************************************
# ---- XXI. PSM: emparejamiento por puntaje de propensión --------------------
# ******************************************************************************
# Complemento a WLS-FE como triangulación. Tratamiento binario: gift_bin
# (recibió cualquier dádiva). ATT estimado sobre muestra emparejada.
# Matching: NN 1:1 con reemplazo, caliper 0.2 (std), exacto por año,
# ponderado por ponderador_norm

# ---- XXI-A. Muestra y matching principal (con reemplazo) -------------------

gift_psm <- gift2 %>%
  filter(complete.cases(gift_bin, vote, margin, edu, age, gen,
                        p_id, type, ln_pop, last_party_vote,
                        year, ponderador_norm, eth))

m_psm <- matchit(
  gift_bin ~ margin + edu + age + gen + p_id + type +
    last_party_vote + eth + ln_pop,
  data        = gift_psm,
  method      = "nearest",
  distance    = "glm",
  link        = "logit",
  exact       = ~ year,
  s.weights   = gift_psm$ponderador_norm,
  ratio       = 1,
  replace     = TRUE,
  caliper     = 0.2,
  std.caliper = TRUE
)

matched_data <- match.data(m_psm)

# ---- XXI-B. Balance ---------------------------------------------------------

bal.tab(m_psm, un = TRUE, m.threshold = 0.1)

p_love <- love.plot(
  m_psm,
  threshold = 0.1,
  abs       = TRUE,
  var.order = "unadjusted",
  line      = FALSE,
  stars     = "raw"
) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 11),
    axis.text  = element_text(size = 10)
  ) +
  labs(x = "Diferencia media estandarizada |SMD|", y = NULL)

ggsave("love_plot_psm.pdf", p_love,
       width = 6, height = 5, device = "pdf")

# ---- XXI-C. Overlap antes y después del matching ---------------------------

col_treated <- "#106254"
col_control <- "grey80"

p_overlap_pre <- ggplot(gift_psm,
                        aes(x = m_psm$distance, fill = factor(gift_bin))) +
  geom_density(alpha = 0.45, color = "black", linewidth = 0.3) +
  scale_fill_manual(
    values = c("0" = col_control, "1" = col_treated),
    labels = c("0" = "Control",  "1" = "Tratados"),
    name   = "Dádiva"
  ) +
  labs(x = "Propensity score", y = "Densidad",
       subtitle = "Antes del emparejamiento") +
  theme_classic(base_family = "Times New Roman") +
  theme(legend.position = "right")

p_overlap_post <- ggplot(matched_data,
                         aes(x = distance, fill = factor(gift_bin), weight = weights)) +
  geom_density(alpha = 0.45, color = "black", linewidth = 0.3) +
  scale_fill_manual(
    values = c("0" = col_control, "1" = col_treated),
    labels = c("0" = "Control",  "1" = "Tratados"),
    name   = "Dádiva"
  ) +
  labs(x = "Propensity score", y = "Densidad",
       subtitle = "Despu\u00e9s del emparejamiento") +
  theme_classic(base_family = "Times New Roman") +
  theme(legend.position = "right")

ggsave("overlap_pre_psm.pdf",  p_overlap_pre,
       width = 5, height = 4, device = "pdf")
ggsave("overlap_post_psm.pdf", p_overlap_post,
       width = 5, height = 4, device = "pdf")

# ---- XXI-D. H5 PSM: dádiva → participación --------------------------------

md_vote <- matched_data %>% filter(!is.na(vote))

psm_att_vote <- feols(
  vote ~ gift_bin,
  data    = md_vote,
  weights = ~ weights,
  cluster = ~ clave_mun
)
summary(psm_att_vote)

# ATT por año (exploratorio)
effects_by_year <- matched_data %>%
  filter(!is.na(vote)) %>%
  group_by(year) %>%
  nest() %>%
  mutate(
    fit    = map(data, ~lm(vote ~ gift_bin, data = .x, weights = .x$weights)),
    tidied = map(fit,  ~tidy(.x, conf.int = TRUE))
  ) %>%
  unnest(tidied) %>%
  filter(term == "gift_bin") %>%
  select(year, estimate, std.error, conf.low, conf.high, p.value) %>%
  ungroup()

p_att_year <- ggplot(effects_by_year,
                     aes(x = as.integer(as.character(year)), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(x = "A\u00f1o", y = "ATT (d\u00e1diva \u2192 participaci\u00f3n)") +
  theme_classic(base_family = "Times New Roman")

# ---- XXI-E. Voto por incumbente (PSM) --------------------------------------

md_inc_pres <- matched_data %>% filter(!is.na(vote_incum_pres_tot))
psm_att_inc_pres <- feols(vote_incum_pres_tot ~ gift_bin,
                          data = md_inc_pres, weights = ~ weights, cluster = ~ clave_mun)

md_inc_gob <- matched_data %>% filter(!is.na(vote_incum_gob_tot))
psm_att_inc_gob <- feols(vote_incum_gob_tot ~ gift_bin,
                         data = md_inc_gob, weights = ~ weights, cluster = ~ clave_mun)

md_inc_mun <- matched_data %>% filter(!is.na(vote_incum_mun_tot))
psm_att_inc_mun <- feols(vote_incum_mun_tot ~ gift_bin,
                         data = md_inc_mun, weights = ~ weights, cluster = ~ clave_mun)

mods_psm_incumb <- list(
  "Inc. presidencial" = psm_att_inc_pres,
  "Inc. gubernatura"  = psm_att_inc_gob,
  "Inc. municipal"    = psm_att_inc_mun
)

tab_psm_incumb <- modelsummary(
  mods_psm_incumb,
  output    = "latex",
  fmt       = 3,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map  = c("gift_bin" = "Recibi\\'{o} d\\'{a}diva"),
  gof_map   = list(
    list(raw = "nobs",          clean = "N\\'{u}m.\\ observaciones", fmt = 0),
    list(raw = "r.squared",     clean = "$R^2$",                     fmt = 3),
    list(raw = "adj.r.squared", clean = "$R^2$ ajustado",            fmt = 3)
  ),
  add_rows = data.frame(
    term                = c("PSM: NN 1:1 con reemplazo",
                            "Pesos de matching",
                            "EE: cl\\'{u}ster municipio"),
    `Inc. presidencial` = rep("S\\'{i}", 3),
    `Inc. gubernatura`  = rep("S\\'{i}", 3),
    `Inc. municipal`    = rep("S\\'{i}", 3),
    check.names = FALSE
  )
)
tab_psm_incumb

# ---- XXI-F. Voto por partido (PSM — triangulación H1) ---------------------

md_pan <- matched_data %>% filter(!is.na(vote_PAN_tot))
psm_pan <- feols(vote_PAN_tot ~ gift_bin,
                 data = md_pan, weights = ~ weights, cluster = ~ clave_mun)

md_pri <- matched_data %>% filter(!is.na(vote_PRI_tot))
psm_pri <- feols(vote_PRI_tot ~ gift_bin,
                 data = md_pri, weights = ~ weights, cluster = ~ clave_mun)

md_prd <- matched_data %>% filter(!is.na(vote_PRD_tot))
psm_prd <- feols(vote_PRD_tot ~ gift_bin,
                 data = md_prd, weights = ~ weights, cluster = ~ clave_mun)

md_mrn <- matched_data %>% filter(!is.na(vote_MRN_tot))
psm_mrn <- feols(vote_MRN_tot ~ gift_bin,
                 data = md_mrn, weights = ~ weights, cluster = ~ clave_mun)

mods_psm_partido <- list(
  "PAN"    = psm_pan,
  "PRI"    = psm_pri,
  "PRD"    = psm_prd,
  "Morena" = psm_mrn
)

tab_psm_partido <- modelsummary(
  mods_psm_partido,
  output    = "latex",
  fmt       = 3,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map  = c("gift_bin" = "Recibi\\'{o} d\\'{a}diva"),
  gof_map   = list(
    list(raw = "nobs",          clean = "N\\'{u}m.\\ observaciones", fmt = 0),
    list(raw = "r.squared",     clean = "$R^2$",                     fmt = 3),
    list(raw = "adj.r.squared", clean = "$R^2$ ajustado",            fmt = 3)
  ),
  add_rows = data.frame(
    term   = c("PSM: NN 1:1 con reemplazo",
               "Pesos de matching",
               "EE: cl\\'{u}ster municipio"),
    PAN    = rep("S\\'{i}", 3),
    PRI    = rep("S\\'{i}", 3),
    PRD    = rep("S\\'{i}", 3),
    Morena = rep("S\\'{i}", 3),
    check.names = FALSE
  )
)
tab_psm_partido

# ---- XXI-G. Análisis de sensibilidad ----------------------------------------

# G1: E-values (EValue::evalues.OLS, sin cargar el paquete completo)
ct     <- summary(psm_att_inc_mun)$coeftable
b_psm  <- ct["gift_bin", "Estimate"]
se_psm <- ct["gift_bin", "Std. Error"]

wtd_sd <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  m  <- sum(w[ok] * x[ok]) / sum(w[ok])
  sqrt(sum(w[ok] * (x[ok] - m)^2) / sum(w[ok]))
}

sd_y   <- sd(md_inc_mun$vote_incum_mun_tot,            na.rm = TRUE)
sd_y_w <- wtd_sd(md_inc_mun$vote_incum_mun_tot, md_inc_mun$weights)

EValue::evalues.OLS(est = b_psm, se = se_psm, sd = sd_y,   delta = 1, true = 0)
EValue::evalues.OLS(est = b_psm, se = se_psm, sd = sd_y_w, delta = 1, true = 0)

# G2: Rosenbaum bounds (matching SIN reemplazo — requerido por rbounds)
m_psm_nr <- matchit(
  gift_bin ~ margin + edu + age + gen + p_id + type +
    last_party_vote + eth + ln_pop,
  data        = gift_psm,
  method      = "nearest",
  distance    = "glm",
  link        = "logit",
  exact       = ~ year,
  s.weights   = gift_psm$ponderador_norm,
  ratio       = 1,
  replace     = FALSE,          # SIN reemplazo (requerido por rbounds)
  caliper     = 0.2,
  std.caliper = TRUE
)

md_nr <- match.data(m_psm_nr) %>%
  filter(!is.na(vote_incum_mun_tot), !is.na(subclass))

pairs <- md_nr %>%
  group_by(subclass) %>%
  summarise(
    y_t = vote_incum_mun_tot[gift_bin == 1][1],
    y_c = vote_incum_mun_tot[gift_bin == 0][1],
    .groups = "drop"
  ) %>%
  filter(!is.na(y_t), !is.na(y_c))

psens(x  = pairs$y_t, y = pairs$y_c, Gamma = 2, GammaInc = 0.1)
hlsens(x = pairs$y_t, y = pairs$y_c, Gamma = 2, GammaInc = 0.1)

# ==============================================================================
# ---- XXII. PSM por partido ----
# ==============================================================================
#
#  Tratamiento:   gift_P  (P ∈ {PAN, PRI, PRD, MRN})
#  Outcome:       vote_P_tot
#  Control:       todos los no-tratados en gift2, incluidos receptores de
#                 dádivas de otros partidos, pareados por año exacto
#  Función auxiliar one_att_row() definida en Sección XXI (no se redefine)


# -- 0b. Distribución de receptores por fuente de dádiva ---------------------
gift2 %>%
  summarise(
    pct_PAN   = round(100 * mean(gift_PAN   == 1L, na.rm = TRUE), 2),
    pct_PRI   = round(100 * mean(gift_PRI   == 1L, na.rm = TRUE), 2),
    pct_PRD   = round(100 * mean(gift_PRD   == 1L, na.rm = TRUE), 2),
    pct_MRN   = round(100 * mean(gift_MRN   == 1L, na.rm = TRUE), 2),
    pct_other = round(100 * mean(gift_other == 1L, na.rm = TRUE), 2),
    pct_any   = round(100 * mean(gift_bin   == 1L, na.rm = TRUE), 2)
  )

# -- 0c. Vector de variables para complete.cases (único, reutilizable) -------
psm_vars_party <- c(
  "gift_PAN", "gift_PRI", "gift_PRD", "gift_MRN",
  "gift_other", "gift_bin",
  "margin", "edu", "age", "gen", "p_id", "type",
  "ln_pop", "last_party_vote", "year", "ponderador_norm", "eth"
)

#### 1) PAN ####

pan_psm <- gift2 %>%
  mutate(year = factor(year)) %>%
  filter(complete.cases(across(all_of(psm_vars_party))))

m_pan_psm <- matchit(
  gift_PAN ~ gift_PRI + gift_PRD + gift_MRN + gift_other +
    margin + edu + age + gen + p_id + type +
    last_party_vote + eth + ln_pop,
  data        = pan_psm,
  method      = "nearest",
  distance    = "glm",
  link        = "logit",
  exact       = ~ year,
  s.weights   = pan_psm$ponderador_norm,
  ratio       = 1,
  replace     = TRUE,
  caliper     = 0.2,
  std.caliper = TRUE
)
pan_matched <- match.data(m_pan_psm)

# Balance
bal.tab(m_pan_psm, un = TRUE, m.threshold = 0.1)
lp_pan <- love.plot(m_pan_psm, threshold = 0.1, abs = TRUE,
                    title = "Balance — PSM PAN") +
  theme_classic(base_family = "Times New Roman")
ggsave("fig_love_pan.pdf", lp_pan, width = 7, height = 5, device = "pdf")

# Añadir p-score al dataset original para el plot "antes"
pan_psm <- pan_psm %>% mutate(distance = m_pan_psm$distance)

# Overlap antes del emparejamiento
p_ov_pan_pre <- ggplot(pan_psm,
                       aes(x = distance, fill = factor(gift_PAN))) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("grey60", "#003f88"),
                    labels = c("Control", "Tratamiento (dádiva PAN)")) +
  labs(fill = NULL, x = "P-score", y = "Densidad",
       title = "Solapamiento antes del emparejamiento — PAN") +
  theme_classic(base_family = "Times New Roman")
ggsave("fig_overlap_pan_pre.pdf", p_ov_pan_pre,
       width = 6, height = 4, device = "pdf")

# Overlap después del emparejamiento
p_ov_pan_post <- ggplot(pan_matched,
                        aes(x = distance, fill = factor(gift_PAN),
                            weight = weights)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("grey60", "#003f88"),
                    labels = c("Control", "Tratamiento (dádiva PAN)")) +
  labs(fill = NULL, x = "P-score", y = "Densidad",
       title = "Solapamiento después del emparejamiento — PAN") +
  theme_classic(base_family = "Times New Roman")
ggsave("fig_overlap_pan_post.pdf", p_ov_pan_post,
       width = 6, height = 4, device = "pdf")

# ATT
pan_vote    <- pan_matched %>% filter(!is.na(vote_PAN_tot))
psm_att_pan <- feols(
  vote_PAN_tot ~ gift_PAN,
  data    = pan_vote,
  weights = ~ weights,
  cluster = ~ clave_mun
)
summary(psm_att_pan)


#### 2) PRI ####

pri_psm <- gift2 %>%
  mutate(year = factor(year)) %>%
  filter(complete.cases(across(all_of(psm_vars_party))))

m_pri_psm <- matchit(
  gift_PRI ~ gift_PAN + gift_PRD + gift_MRN + gift_other +
    margin + edu + age + gen + p_id + type +
    last_party_vote + eth + ln_pop,
  data        = pri_psm,
  method      = "nearest",
  distance    = "glm",
  link        = "logit",
  exact       = ~ year,
  s.weights   = pri_psm$ponderador_norm,
  ratio       = 1,
  replace     = TRUE,
  caliper     = 0.2,
  std.caliper = TRUE
)
pri_matched <- match.data(m_pri_psm)

# Balance
bal.tab(m_pri_psm, un = TRUE, m.threshold = 0.1)
lp_pri <- love.plot(m_pri_psm, threshold = 0.1, abs = TRUE,
                    title = "Balance — PSM PRI") +
  theme_classic(base_family = "Times New Roman")
ggsave("fig_love_pri.pdf", lp_pri, width = 7, height = 5, device = "pdf")

pri_psm <- pri_psm %>% mutate(distance = m_pri_psm$distance)

p_ov_pri_pre <- ggplot(pri_psm,
                       aes(x = distance, fill = factor(gift_PRI))) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("grey60", "#c8102e"),
                    labels = c("Control", "Tratamiento (dádiva PRI)")) +
  labs(fill = NULL, x = "P-score", y = "Densidad",
       title = "Solapamiento antes del emparejamiento — PRI") +
  theme_classic(base_family = "Times New Roman")
ggsave("fig_overlap_pri_pre.pdf", p_ov_pri_pre,
       width = 6, height = 4, device = "pdf")

p_ov_pri_post <- ggplot(pri_matched,
                        aes(x = distance, fill = factor(gift_PRI),
                            weight = weights)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("grey60", "#c8102e"),
                    labels = c("Control", "Tratamiento (dádiva PRI)")) +
  labs(fill = NULL, x = "P-score", y = "Densidad",
       title = "Solapamiento después del emparejamiento — PRI") +
  theme_classic(base_family = "Times New Roman")
ggsave("fig_overlap_pri_post.pdf", p_ov_pri_post,
       width = 6, height = 4, device = "pdf")

# ATT
pri_vote    <- pri_matched %>% filter(!is.na(vote_PRI_tot))
psm_att_pri <- feols(
  vote_PRI_tot ~ gift_PRI,
  data    = pri_vote,
  weights = ~ weights,
  cluster = ~ clave_mun
)
summary(psm_att_pri)

#### 3) PRD #####

prd_psm <- gift2 %>%
  mutate(year = factor(year)) %>%
  filter(complete.cases(across(all_of(psm_vars_party))))

m_prd_psm <- matchit(
  gift_PRD ~ gift_PAN + gift_PRI + gift_MRN + gift_other +
    margin + edu + age + gen + p_id + type +
    last_party_vote + eth + ln_pop,
  data        = prd_psm,
  method      = "nearest",
  distance    = "glm",
  link        = "logit",
  exact       = ~ year,
  s.weights   = prd_psm$ponderador_norm,
  ratio       = 1,
  replace     = TRUE,
  caliper     = 0.2,
  std.caliper = TRUE
)
prd_matched <- match.data(m_prd_psm)

# Balance
bal.tab(m_prd_psm, un = TRUE, m.threshold = 0.1)
lp_prd <- love.plot(m_prd_psm, threshold = 0.1, abs = TRUE,
                    title = "Balance — PSM PRD") +
  theme_classic(base_family = "Times New Roman")
ggsave("fig_love_prd.pdf", lp_prd, width = 7, height = 5, device = "pdf")

prd_psm <- prd_psm %>% mutate(distance = m_prd_psm$distance)

p_ov_prd_pre <- ggplot(prd_psm,
                       aes(x = distance, fill = factor(gift_PRD))) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("grey60", "#f5c518"),
                    labels = c("Control", "Tratamiento (dádiva PRD)")) +
  labs(fill = NULL, x = "P-score", y = "Densidad",
       title = "Solapamiento antes del emparejamiento — PRD") +
  theme_classic(base_family = "Times New Roman")
ggsave("fig_overlap_prd_pre.pdf", p_ov_prd_pre,
       width = 6, height = 4, device = "pdf")

p_ov_prd_post <- ggplot(prd_matched,
                        aes(x = distance, fill = factor(gift_PRD),
                            weight = weights)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("grey60", "#f5c518"),
                    labels = c("Control", "Tratamiento (dádiva PRD)")) +
  labs(fill = NULL, x = "P-score", y = "Densidad",
       title = "Solapamiento después del emparejamiento — PRD") +
  theme_classic(base_family = "Times New Roman")
ggsave("fig_overlap_prd_post.pdf", p_ov_prd_post,
       width = 6, height = 4, device = "pdf")

# ATT
prd_vote    <- prd_matched %>% filter(!is.na(vote_PRD_tot))
psm_att_prd <- feols(
  vote_PRD_tot ~ gift_PRD,
  data    = prd_vote,
  weights = ~ weights,
  cluster = ~ clave_mun
)
summary(psm_att_prd)


#### 4) Morena #####

mrn_psm <- gift2 %>%
  mutate(year = factor(year)) %>%
  filter(complete.cases(across(all_of(psm_vars_party))))

m_mrn_psm <- matchit(
  gift_MRN ~ gift_PAN + gift_PRI + gift_PRD + gift_other +
    margin + edu + age + gen + p_id + type +
    last_party_vote + eth + ln_pop,
  data        = mrn_psm,
  method      = "nearest",
  distance    = "glm",
  link        = "logit",
  exact       = ~ year,
  s.weights   = mrn_psm$ponderador_norm,
  ratio       = 1,
  replace     = TRUE,
  caliper     = 0.2,
  std.caliper = TRUE
)
mrn_matched <- match.data(m_mrn_psm)

# Balance
bal.tab(m_mrn_psm, un = TRUE, m.threshold = 0.1)
lp_mrn <- love.plot(m_mrn_psm, threshold = 0.1, abs = TRUE,
                    title = "Balance — PSM Morena") +
  theme_classic(base_family = "Times New Roman")
ggsave("fig_love_mrn.pdf", lp_mrn, width = 7, height = 5, device = "pdf")

mrn_psm <- mrn_psm %>% mutate(distance = m_mrn_psm$distance)

p_ov_mrn_pre <- ggplot(mrn_psm,
                       aes(x = distance, fill = factor(gift_MRN))) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("grey60", "#6B1A2B"),
                    labels = c("Control", "Tratamiento (dádiva Morena)")) +
  labs(fill = NULL, x = "P-score", y = "Densidad",
       title = "Solapamiento antes del emparejamiento — Morena") +
  theme_classic(base_family = "Times New Roman")
ggsave("fig_overlap_mrn_pre.pdf", p_ov_mrn_pre,
       width = 6, height = 4, device = "pdf")

p_ov_mrn_post <- ggplot(mrn_matched,
                        aes(x = distance, fill = factor(gift_MRN),
                            weight = weights)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("grey60", "#6B1A2B"),
                    labels = c("Control", "Tratamiento (dádiva Morena)")) +
  labs(fill = NULL, x = "P-score", y = "Densidad",
       title = "Solapamiento después del emparejamiento — Morena") +
  theme_classic(base_family = "Times New Roman")
ggsave("fig_overlap_mrn_post.pdf", p_ov_mrn_post,
       width = 6, height = 4, device = "pdf")

# ATT
mrn_vote    <- mrn_matched %>% filter(!is.na(vote_MRN_tot))
psm_att_mrn <- feols(
  vote_MRN_tot ~ gift_MRN,
  data    = mrn_vote,
  weights = ~ weights,
  cluster = ~ clave_mun
)
summary(psm_att_mrn)

# *********************************************
# Tabla consolidada de ATT por partido

# -- Tabla de inspección rápida (consola) ---
results_psm_gift_party <- bind_rows(
  one_att_row(psm_att_pan, "gift_PAN", "Regalo PAN",    "Voto PAN (total)"),
  one_att_row(psm_att_pri, "gift_PRI", "Regalo PRI",    "Voto PRI (total)"),
  one_att_row(psm_att_prd, "gift_PRD", "Regalo PRD",    "Voto PRD (total)"),
  one_att_row(psm_att_mrn, "gift_MRN", "Regalo Morena", "Voto Morena (total)")
) %>%
  mutate(
    stars = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.10 ~ "*",
      TRUE           ~ ""
    ),
    `ATT (EE)` = sprintf("%.3f%s (%.3f)", estimate, stars, std.error)
  ) %>%
  select(Outcome, Tratamiento, `ATT (EE)`, N)

print(results_psm_gift_party)

# -- Tabla LaTeX (modelsummary) ---
results_gift_party <- list(
  "Voto PAN"    = psm_att_pan,
  "Voto PRI"    = psm_att_pri,
  "Voto PRD"    = psm_att_prd,
  "Voto Morena" = psm_att_mrn
)

modelsummary(
  results_gift_party,
  output    = "psm_att_vote_party.tex",
  coef_map  = c(
    "gift_PAN" = "Recibió dádiva del partido",
    "gift_PRI" = "Recibió dádiva del partido",
    "gift_PRD" = "Recibió dádiva del partido",
    "gift_MRN" = "Recibió dádiva del partido"
  ),
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  stars     = c("*" = .10, "**" = .05, "***" = .01),
  gof_map   = list(
    list(raw = "nobs", clean = "N", fmt = 0)
  )
)

# ******************************************************************************
# ---- XXIII. PSM coincidence ----
# ******************************************************************************

library(dplyr)
library(MatchIt)
library(cobalt)
library(ggplot2)
library(fixest)

# 1) Crear tratamiento "regalo del incumbente municipal"
# FIX 3: gift_inc_mun se crea ANTES de inc_mun = factor(), evitando
#         comparación frágil factor == integer
gift_tmp <- gift2 %>%                                   
  mutate(
    gift_inc_mun = as.integer(
      (gift_PAN == 1 & inc_mun == 1) |
        (gift_PRI == 1 & inc_mun == 2) |
        (gift_PRD == 1 & inc_mun == 3) |
        (gift_MRN == 1 & inc_mun == 4)
    ),
    year    = factor(year),
    inc_mun = factor(inc_mun),
    gen     = factor(gen)
  )

# 2) Preparar muestra
inc_psm <- gift_tmp %>%
  filter(complete.cases(
    gift_inc_mun,
    gift_PAN, gift_PRI, gift_PRD, gift_MRN, gift_other, gift_bin,
    margin, edu, age, gen, p_id, type, ln_pop, last_party_vote, eth,
    year, ponderador_norm, inc_mun,                                     
    vote_incum_mun_tot
  ))

# 3) Matching
m_inc_psm <- matchit(
  gift_inc_mun ~
    gift_PAN + gift_PRI + gift_PRD + gift_MRN + gift_other +
    margin + edu + age + gen + p_id + type + ln_pop +                  # FIX 6: p_id
    last_party_vote + eth + inc_mun,
  data        = inc_psm,
  method      = "nearest",
  distance    = "glm",
  link        = "logit",
  exact       = ~ year,
  s.weights   = inc_psm$ponderador_norm,                                 # FIX 2
  ratio       = 1,
  replace     = TRUE,
  caliper     = 0.2,
  std.caliper = TRUE
)

inc_matched <- match.data(m_inc_psm)

# 4) Balance
bal.tab(m_inc_psm, un = TRUE, m.threshold = 0.1)
love.plot(m_inc_psm, threshold = 0.1, abs = TRUE)

# 5) Overlap
# FIX 4: añadir distance al df para no referenciar m_inc_psm$distance en aes()
inc_psm <- inc_psm %>% mutate(distance = m_inc_psm$distance)

ggplot(inc_psm, aes(x = distance, fill = factor(gift_inc_mun))) +
  geom_density(alpha = 0.4) +
  labs(fill = "Gift incumbente", x = "Pscore", y = "Densidad",
       title = "Overlap (antes del matching)") +
  theme_minimal()

ggplot(inc_matched, aes(x = distance, fill = factor(gift_inc_mun), weight = weights)) +
  geom_density(alpha = 0.4) +
  labs(fill = "Gift incumbente", x = "Pscore", y = "Densidad",
       title = "Overlap (después del matching)") +
  theme_minimal()

# 6) ATT
att_inc_mun <- feols(
  vote_incum_mun_tot ~ gift_inc_mun,
  data    = inc_matched,
  weights = ~ weights,
  cluster = ~ clave_mun
)
summary(att_inc_mun)


#### 1) PAN ####


pan_inc_psm <- gift2 %>%                                               # FIX 1
  mutate(year = factor(year), gen = factor(gen)) %>%                   # FIX 5: sin est
  filter(inc_mun == 1) %>%
  filter(complete.cases(
    gift_PAN, gift_PRI, gift_PRD, gift_MRN, gift_other,
    margin, edu, age, gen, p_id, type, ln_pop, last_party_vote, eth,
    year, ponderador_norm, vote_PAN_tot                                   # FIX 2
  ))

m_pan_inc <- matchit(
  gift_PAN ~ gift_PRI + gift_PRD + gift_MRN + gift_other +
    margin + edu + age + gen + p_id + type + ln_pop + last_party_vote + eth,
  data        = pan_inc_psm,
  method      = "nearest",
  distance    = "glm",
  link        = "logit",
  exact       = ~ year,
  s.weights   = pan_inc_psm$ponderador_norm,                             # FIX 2
  ratio       = 1,
  replace     = TRUE,
  caliper     = 0.2,
  std.caliper = TRUE
)

pan_inc_matched <- match.data(m_pan_inc)

bal.tab(m_pan_inc, un = TRUE, m.threshold = 0.1)
love.plot(m_pan_inc, threshold = 0.1, abs = TRUE)

att_pan_inc <- feols(
  vote_PAN_tot ~ gift_PAN,
  data    = pan_inc_matched,
  weights = ~ weights,
  cluster = ~ clave_mun
)
summary(att_pan_inc)


### E-values para PAN (incumbente) ###

ct_pan     <- summary(att_pan_inc)$coeftable
b_pan      <- ct_pan["gift_PAN", "Estimate"]
se_pan     <- ct_pan["gift_PAN", "Std. Error"]

sd_y_pan   <- sd(pan_inc_matched$vote_PAN_tot, na.rm = TRUE)

wtd_sd <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  m <- sum(w * x) / sum(w)
  sqrt(sum(w * (x - m)^2) / sum(w))
}
sd_y_pan_w <- wtd_sd(pan_inc_matched$vote_PAN_tot, pan_inc_matched$weights)

library(EValue)
ev_pan_unw <- evalues.OLS(est = b_pan, se = se_pan, sd = sd_y_pan,   delta = 1, true = 0)
ev_pan_w   <- evalues.OLS(est = b_pan, se = se_pan, sd = sd_y_pan_w, delta = 1, true = 0)
ev_pan_unw
ev_pan_w


#### 2) PRI ####

pri_inc_psm <- gift2 %>%                                               # FIX 1
  mutate(year = factor(year), gen = factor(gen)) %>%                   # FIX 5
  filter(inc_mun == 2) %>%
  filter(complete.cases(
    gift_PAN, gift_PRI, gift_PRD, gift_MRN, gift_other,
    margin, edu, age, gen, p_id, type, ln_pop, last_party_vote, eth,   # FIX 6: p_id
    year, ponderador_norm, vote_PRI_tot                                   # FIX 2
  ))

m_pri_inc <- matchit(
  gift_PRI ~ gift_PAN + gift_PRD + gift_MRN + gift_other +
    margin + edu + age + gen + p_id + type + ln_pop + last_party_vote + eth, # FIX 6: p_id
  data        = pri_inc_psm,
  method      = "nearest",
  distance    = "glm",
  link        = "logit",
  exact       = ~ year,
  s.weights   = pri_inc_psm$ponderador_norm,                             # FIX 2
  ratio       = 1,
  replace     = TRUE,
  caliper     = 0.2,
  std.caliper = TRUE
)

pri_inc_matched <- match.data(m_pri_inc)

bal.tab(m_pri_inc, un = TRUE, m.threshold = 0.1)
love.plot(m_pri_inc, threshold = 0.1, abs = TRUE)

att_pri_inc <- feols(
  vote_PRI_tot ~ gift_PRI,
  data    = pri_inc_matched,
  weights = ~ weights,
  cluster = ~ clave_mun
)
summary(att_pri_inc)


#### 3) PRD ####

prd_inc_psm <- gift2 %>%                                               # FIX 1
  mutate(year = factor(year), gen = factor(gen)) %>%                   # FIX 5
  filter(inc_mun == 3) %>%
  filter(complete.cases(
    gift_PAN, gift_PRI, gift_PRD, gift_MRN, gift_other,
    margin, edu, age, gen, p_id, type, ln_pop, last_party_vote, eth,   # FIX 6: p_id
    year, ponderador_norm, vote_PRD_tot                                   # FIX 2
  ))

m_prd_inc <- matchit(
  gift_PRD ~ gift_PAN + gift_PRI + gift_MRN + gift_other +
    margin + edu + age + gen + p_id + type + ln_pop + last_party_vote + eth, # FIX 6: p_id
  data        = prd_inc_psm,
  method      = "nearest",
  distance    = "glm",
  link        = "logit",
  exact       = ~ year,
  s.weights   = prd_inc_psm$ponderador_norm,                             # FIX 2
  ratio       = 1,
  replace     = TRUE,
  caliper     = 0.2,
  std.caliper = TRUE
)

prd_inc_matched <- match.data(m_prd_inc)

bal.tab(m_prd_inc, un = TRUE, m.threshold = 0.1)
love.plot(m_prd_inc, threshold = 0.1, abs = TRUE)

att_prd_inc <- feols(
  vote_PRD_tot ~ gift_PRD,
  data    = prd_inc_matched,
  weights = ~ weights,
  cluster = ~ clave_mun
)
summary(att_prd_inc)


#### 4) MRN ####


mrn_inc_psm <- gift2 %>%                                               # FIX 1
  mutate(year = factor(year), gen = factor(gen)) %>%                   # FIX 5
  filter(inc_mun == 4) %>%
  filter(complete.cases(
    gift_PAN, gift_PRI, gift_PRD, gift_MRN, gift_other,
    margin, edu, age, gen, p_id, type, ln_pop, last_party_vote, eth,   # FIX 6: p_id
    year, ponderador_norm, vote_MRN_tot                                   # FIX 2
  ))

m_mrn_inc <- matchit(
  gift_MRN ~ gift_PAN + gift_PRI + gift_PRD + gift_other +
    margin + edu + age + gen + p_id + type + ln_pop + last_party_vote + eth, # FIX 6: p_id
  data        = mrn_inc_psm,
  method      = "nearest",
  distance    = "glm",
  link        = "logit",
  exact       = ~ year,
  s.weights   = mrn_inc_psm$ponderador_norm,                             # FIX 2
  ratio       = 1,
  replace     = TRUE,
  caliper     = 0.2,
  std.caliper = TRUE
)

mrn_inc_matched <- match.data(m_mrn_inc)

bal.tab(m_mrn_inc, un = TRUE, m.threshold = 0.1)
love.plot(m_mrn_inc, threshold = 0.1, abs = TRUE)

att_mrn_inc <- feols(
  vote_MRN_tot ~ gift_MRN,
  data    = mrn_inc_matched,
  weights = ~ weights,
  cluster = ~ clave_mun
)
summary(att_mrn_inc)

# ******************************************************************************
# ---- XXIV. PSM arraigo  ----
# ******************************************************************************

##### a. PAN #####
pan_psm_pres <- gift4 %>%                              # FIX: gift4 (tiene pres_*_12 + factor fix)
  filter(pres_pan_12 >= 0.5) %>%
  filter(complete.cases(
    gift_PAN, gift_PRI, gift_PRD, gift_MRN, gift_other, # FIX: multi-regalo
    pres_pan_12,
    margin, edu, age, gen, p_id, type, ln_pop,          # FIX: p_id
    last_party_vote, eth,
    year, ponderador_norm, vote_PAN_tot                   # FIX: w_norm_global
  ))

m_pan_psm_pres <- matchit(
  gift_PAN ~ gift_PRI + gift_PRD + gift_MRN + gift_other + # FIX: multi-regalo
    margin + edu + age + gen + p_id +         # FIX: p_id
    type + ln_pop + last_party_vote + eth,
  data        = pan_psm_pres,
  method      = "nearest",
  distance    = "glm",
  link        = "logit",
  exact       = ~ year,
  s.weights   = pan_psm_pres$ponderador_norm,            # FIX
  ratio       = 1,
  replace     = TRUE,
  caliper     = 0.2,
  std.caliper = TRUE
)

pan_matched_pres <- match.data(m_pan_psm_pres)
bal.tab(m_pan_psm_pres, un = TRUE, m.threshold = 0.1)
love.plot(m_pan_psm_pres, threshold = 0.1, abs = TRUE)

att_pan_pres <- feols(
  vote_PAN_tot ~ gift_PAN,
  data    = pan_matched_pres,
  weights = ~ weights,
  cluster = ~ clave_mun
)
summary(att_pan_pres)

##### b. PRI #####
pri_psm_pres <- gift4 %>%                              # FIX
  filter(pres_pri_12 >= 0.5) %>%
  filter(complete.cases(
    gift_PAN, gift_PRI, gift_PRD, gift_MRN, gift_other,
    pres_pri_12,
    margin, edu, age, gen, p_id, type, ln_pop,
    last_party_vote, eth,
    year, ponderador_norm, vote_PRI_tot
  ))

m_pri_psm_pres <- matchit(
  gift_PRI ~ gift_PAN + gift_PRD + gift_MRN + gift_other +
    margin + edu + age + gen + p_id +
    type + ln_pop + last_party_vote + eth,
  data        = pri_psm_pres,
  method      = "nearest",
  distance    = "glm",
  link        = "logit",
  exact       = ~ year,
  s.weights   = pri_psm_pres$ponderador_norm,
  ratio       = 1,
  replace     = TRUE,
  caliper     = 0.2,
  std.caliper = TRUE
)

pri_matched_pres <- match.data(m_pri_psm_pres)
bal.tab(m_pri_psm_pres, un = TRUE, m.threshold = 0.1)
love.plot(m_pri_psm_pres, threshold = 0.1, abs = TRUE)

att_pri_pres <- feols(
  vote_PRI_tot ~ gift_PRI,
  data    = pri_matched_pres,
  weights = ~ weights,
  cluster = ~ clave_mun
)
summary(att_pri_pres)

##### c. PRD #####
prd_psm_pres <- gift4 %>%                              # FIX
  filter(pres_prd_12>=0.5) %>%
  filter(complete.cases(
    gift_PAN, gift_PRI, gift_PRD, gift_MRN, gift_other,
    pres_prd_12,
    margin, edu, age, gen, p_id, type, ln_pop,
    last_party_vote, eth,
    year, ponderador_norm, vote_PRD_tot
  ))

m_prd_psm_pres <- matchit(
  gift_PRD ~ gift_PAN + gift_PRI + gift_MRN + gift_other +
    margin + edu + age + gen + p_id +
    type + ln_pop + last_party_vote + eth,
  data        = prd_psm_pres,
  method      = "nearest",
  distance    = "glm",
  link        = "logit",
  exact       = ~ year,
  s.weights   = prd_psm_pres$ponderador_norm,
  ratio       = 1,
  replace     = TRUE,
  caliper     = 0.2,
  std.caliper = TRUE
)

prd_matched_pres <- match.data(m_prd_psm_pres)
bal.tab(m_prd_psm_pres, un = TRUE, m.threshold = 0.1)
love.plot(m_prd_psm_pres, threshold = 0.1, abs = TRUE)

att_prd_pres <- feols(
  vote_PRD_tot ~ gift_PRD,
  data    = prd_matched_pres,
  weights = ~ weights,
  cluster = ~ clave_mun
)
summary(att_prd_pres)

##### d. MRN #####
mrn_psm_pres <- gift4 %>%                              # FIX
  filter(!is.na(pres_mrn_12)) %>%
  filter(complete.cases(
    gift_PAN, gift_PRI, gift_PRD, gift_MRN, gift_other, # FIX: añadir todos
    pres_mrn_12,
    margin, edu, age, gen, p_id, type, ln_pop,
    last_party_vote, eth,
    year, ponderador_norm, vote_MRN_tot
  ))

m_mrn_psm_pres <- matchit(
  gift_MRN ~ gift_PAN + gift_PRI + gift_PRD + gift_other + # FIX: añadir todos
    pres_mrn_12 + margin + edu + age + gen + p_id +
    type + ln_pop + last_party_vote + eth,
  data        = mrn_psm_pres,
  method      = "nearest",
  distance    = "glm",
  link        = "logit",
  exact       = ~ year,
  s.weights   = mrn_psm_pres$ponderador_norm,
  ratio       = 1,
  replace     = TRUE,
  caliper     = 0.2,
  std.caliper = TRUE
)

mrn_matched_pres <- match.data(m_mrn_psm_pres)
bal.tab(m_mrn_psm_pres, un = TRUE, m.threshold = 0.1)
love.plot(m_mrn_psm_pres, threshold = 0.1, abs = TRUE)

att_mrn_pres <- feols(
  vote_MRN_tot ~ gift_MRN * pres_mrn_12,
  data    = mrn_matched_pres,
  weights = ~ weights,
  cluster = ~ clave_mun
)
summary(att_mrn_pres)

# ******************************************************************************
# ---- XXV. PSM Party_ID ----
# ******************************************************************************

# Añadir a gift2 junto con las demás:
gift2 <- gift2 %>%
  mutate(
    pid_PAN   = as.integer(as.character(p_id) == "1"),
    pid_PRI   = as.integer(as.character(p_id) == "2"),
    pid_PRD   = as.integer(as.character(p_id) == "3"),
    pid_MRN   = as.integer(as.character(p_id) == "4"),
    pid_none  = as.integer(as.character(p_id) == "0"),
    pid_other = as.integer(as.character(p_id) == "5")
  )

#### 1) PAN_ID ####

pid_pan_psm <- gift2 %>%
  mutate(year = factor(year)) %>%
  filter(complete.cases(
    gift_PAN, gift_PRI, gift_PRD, gift_MRN, gift_other, gift_bin,
    margin, edu, age, gen, type, ln_pop, last_party_vote, eth,
    year, ponderador_norm,
    pid_PAN, pid_PRI, pid_PRD, pid_MRN, pid_none, pid_other   # añadir todos para complete.cases
  ))

m_pan_pid <- matchit(
  gift_PAN ~ gift_MRN + gift_PRI + gift_PRD + gift_other +
    margin + edu + age + gen + type + last_party_vote + eth + ln_pop +
    pid_PRI + pid_PRD + pid_MRN + pid_none + pid_other,          # FIX: otros pid, NO pid_PAN
  data        = pid_pan_psm,
  method      = "nearest",
  distance    = "glm",
  link        = "logit",
  exact       = ~ year,
  s.weights   = pid_pan_psm$ponderador_norm,
  ratio       = 1,
  replace     = TRUE,
  caliper     = 0.2,
  std.caliper = TRUE
)

pan_pid_matched <- match.data(m_pan_pid)
bal.tab(m_pan_pid, un = TRUE, m.threshold = 0.1)
love.plot(m_pan_pid, threshold = 0.1, abs = TRUE)

pid_pan_psm <- pid_pan_psm %>% mutate(distance = m_pan_pid$distance)

ggplot(pid_pan_psm, aes(x = distance, fill = factor(gift_PAN))) +
  geom_density(alpha = 0.4) +
  labs(fill = "Gift_PAN", x = "Pscore", y = "Densidad",
       title = "Overlap (antes del matching)") +
  theme_minimal()

ggplot(pan_pid_matched, aes(x = distance, fill = factor(gift_PAN), weight = weights)) +
  geom_density(alpha = 0.4) +
  labs(fill = "Gift_PAN", x = "Pscore", y = "Densidad",
       title = "Overlap (después del matching)") +
  theme_minimal()

att_pid_pan <- feols(
  pid_PAN ~ gift_PAN,
  data    = pan_pid_matched,
  weights = ~ weights,
  cluster = ~ clave_mun
)
summary(att_pid_pan)

#### 2) PRI_ID ####

pid_pri_psm <- gift2 %>%
  mutate(year = factor(year)) %>%
  filter(complete.cases(
    gift_PAN, gift_PRI, gift_PRD, gift_MRN, gift_other, gift_bin,
    margin, edu, age, gen, type, ln_pop, last_party_vote, eth,
    year, ponderador_norm,
    pid_PAN, pid_PRI, pid_PRD, pid_MRN, pid_none, pid_other
  ))

m_pri_pid <- matchit(
  gift_PRI ~ gift_MRN + gift_PAN + gift_PRD + gift_other +
    margin + edu + age + gen + type + last_party_vote + eth + ln_pop +
    pid_PAN + pid_PRD + pid_MRN + pid_none + pid_other,          # FIX: otros pid, NO pid_PRI
  data        = pid_pri_psm,
  method      = "nearest",
  distance    = "glm",
  link        = "logit",
  exact       = ~ year,
  s.weights   = pid_pri_psm$ponderador_norm,
  ratio       = 1,
  replace     = TRUE,
  caliper     = 0.2,
  std.caliper = TRUE
)

pri_pid_matched <- match.data(m_pri_pid)
bal.tab(m_pri_pid, un = TRUE, m.threshold = 0.1)
love.plot(m_pri_pid, threshold = 0.1, abs = TRUE)

pid_pri_psm <- pid_pri_psm %>% mutate(distance = m_pri_pid$distance)

ggplot(pid_pri_psm, aes(x = distance, fill = factor(gift_PRI))) +
  geom_density(alpha = 0.4) +
  labs(fill = "Gift_PRI", x = "Pscore", y = "Densidad",
       title = "Overlap (antes del matching)") +
  theme_minimal()

ggplot(pri_pid_matched, aes(x = distance, fill = factor(gift_PRI), weight = weights)) +
  geom_density(alpha = 0.4) +
  labs(fill = "Gift_PRI", x = "Pscore", y = "Densidad",
       title = "Overlap (después del matching)") +
  theme_minimal()

att_pid_pri <- feols(
  pid_PRI ~ gift_PRI,
  data    = pri_pid_matched,
  weights = ~ weights,
  cluster = ~ clave_mun
)
summary(att_pid_pri)

#### 3) PRD_ID ####

pid_prd_psm <- gift2 %>%
  mutate(year = factor(year)) %>%
  filter(complete.cases(
    gift_PAN, gift_PRI, gift_PRD, gift_MRN, gift_other, gift_bin,
    margin, edu, age, gen, type, ln_pop, last_party_vote, eth,
    year, ponderador_norm,
    pid_PAN, pid_PRI, pid_PRD, pid_MRN, pid_none, pid_other
  ))

m_prd_pid <- matchit(
  gift_PRD ~ gift_MRN + gift_PAN + gift_PRI + gift_other +
    margin + edu + age + gen + type + last_party_vote + eth + ln_pop +
    pid_PAN + pid_PRI + pid_MRN + pid_none + pid_other,          # FIX: otros pid, NO pid_PRD
  data        = pid_prd_psm,
  method      = "nearest",
  distance    = "glm",
  link        = "logit",
  exact       = ~ year,
  s.weights   = pid_prd_psm$ponderador_norm,
  ratio       = 1,
  replace     = TRUE,
  caliper     = 0.2,
  std.caliper = TRUE
)

prd_pid_matched <- match.data(m_prd_pid)
bal.tab(m_prd_pid, un = TRUE, m.threshold = 0.1)
love.plot(m_prd_pid, threshold = 0.1, abs = TRUE)

pid_prd_psm <- pid_prd_psm %>% mutate(distance = m_prd_pid$distance)

ggplot(pid_prd_psm, aes(x = distance, fill = factor(gift_PRD))) +
  geom_density(alpha = 0.4) +
  labs(fill = "Gift_PRD", x = "Pscore", y = "Densidad",
       title = "Overlap (antes del matching)") +
  theme_minimal()

ggplot(prd_pid_matched, aes(x = distance, fill = factor(gift_PRD), weight = weights)) +
  geom_density(alpha = 0.4) +
  labs(fill = "Gift_PRD", x = "Pscore", y = "Densidad",
       title = "Overlap (después del matching)") +
  theme_minimal()

att_pid_prd <- feols(
  pid_PRD ~ gift_PRD,
  data    = prd_pid_matched,
  weights = ~ weights,
  cluster = ~ clave_mun
)
summary(att_pid_prd)

#### 4) MRN_ID ####

pid_mrn_psm <- gift2 %>%
  mutate(year = factor(year)) %>%
  filter(complete.cases(
    gift_PAN, gift_PRI, gift_PRD, gift_MRN, gift_other, gift_bin,
    margin, edu, age, gen, type, ln_pop, last_party_vote, eth,
    year, ponderador_norm,
    pid_PAN, pid_PRI, pid_PRD, pid_MRN, pid_none, pid_other
  ))

m_mrn_pid <- matchit(
  gift_MRN ~ gift_PRD + gift_PAN + gift_PRI + gift_other +
    margin + edu + age + gen + type + last_party_vote + eth + ln_pop +
    pid_PAN + pid_PRI + pid_PRD + pid_none + pid_other,          # FIX: otros pid, NO pid_MRN
  data        = pid_mrn_psm,
  method      = "nearest",
  distance    = "glm",
  link        = "logit",
  exact       = ~ year,
  s.weights   = pid_mrn_psm$ponderador_norm,
  ratio       = 1,
  replace     = TRUE,
  caliper     = 0.2,
  std.caliper = TRUE
)

mrn_pid_matched <- match.data(m_mrn_pid)
bal.tab(m_mrn_pid, un = TRUE, m.threshold = 0.1)
love.plot(m_mrn_pid, threshold = 0.1, abs = TRUE)

pid_mrn_psm <- pid_mrn_psm %>% mutate(distance = m_mrn_pid$distance)

ggplot(pid_mrn_psm, aes(x = distance, fill = factor(gift_MRN))) +
  geom_density(alpha = 0.4) +
  labs(fill = "Gift_MRN", x = "Pscore", y = "Densidad",
       title = "Overlap (antes del matching)") +
  theme_minimal()

ggplot(mrn_pid_matched, aes(x = distance, fill = factor(gift_MRN), weight = weights)) +
  geom_density(alpha = 0.4) +
  labs(fill = "Gift_MRN", x = "Pscore", y = "Densidad",
       title = "Overlap (después del matching)") +
  theme_minimal()

att_pid_mrn <- feols(
  pid_MRN ~ gift_MRN,
  data    = mrn_pid_matched,
  weights = ~ weights,
  cluster = ~ clave_mun
)
summary(att_pid_mrn)

# ******************************************************************************
# ---- XXVI. H5 PSM: Dádiva / conocimiento / exclusión → turnout --------------
# ******************************************************************************

#### 1) gift_bin → turnout ####

gift_psm <- gift2 %>%                                          # FIX 1
  mutate(year = factor(year)) %>%
  filter(complete.cases(
    gift_bin, margin, edu, age, gen, p_id, type, ln_pop,
    last_party_vote, eth, year, ponderador_norm                  # FIX 2
  ))

m_psm <- matchit(
  gift_bin ~ margin + edu + age + gen + p_id + type + last_party_vote + eth + ln_pop,
  data        = gift_psm,
  method      = "nearest",
  distance    = "glm",
  link        = "logit",
  exact       = ~ year,
  s.weights   = gift_psm$ponderador_norm,                        # FIX 2
  ratio       = 1,
  replace     = TRUE,
  caliper     = 0.2,
  std.caliper = TRUE
)

matched_data <- match.data(m_psm)
bal.tab(m_psm, un = TRUE, m.threshold = 0.1)
love.plot(m_psm, threshold = 0.1, abs = TRUE)

gift_psm <- gift_psm %>% mutate(distance = m_psm$distance)    # FIX 3

p_gift_pre <- ggplot(gift_psm,
                     aes(x = distance, fill = factor(gift_bin))) +
  geom_density(alpha = 0.45, color = "black", linewidth = 0.3) +
  scale_fill_manual(
    values = c("0" = col_control, "1" = col_treated),
    labels = c("0" = "Control",   "1" = "Tratados"),
    name   = "Dádiva") +
  labs(x = "Propensity score", y = "Densidad") +
  theme_classic(base_family = "Times New Roman") +
  theme(legend.position = "right")
p_gift_pre

ggsave("gift_overlap_pre.pdf", plot = p_gift_pre,
       width = 5, height = 4, device = "pdf")

# ---- Gift bin: después ----
p_gift_post <- ggplot(matched_data,
                      aes(x = distance, fill = factor(gift_bin), weight = weights)) +
  geom_density(alpha = 0.45, color = "black", linewidth = 0.3) +
  scale_fill_manual(
    values = c("0" = col_control, "1" = col_treated),
    labels = c("0" = "Control",   "1" = "Tratados"),
    name   = "Dádiva") +
  labs(x = "Propensity score", y = "Densidad") +
  theme_classic(base_family = "Times New Roman") +
  theme(legend.position = "right")
p_gift_post

ggsave("gift_overlap_post.pdf", plot = p_gift_post,
       width = 5, height = 4, device = "pdf")

md_vote <- matched_data %>% filter(!is.na(vote))

psm_att_vote <- feols(
  vote ~ gift_bin,
  data    = md_vote,
  weights = ~ weights,
  cluster = ~ clave_mun
)
summary(psm_att_vote)


#### 2) know_bin → turnout ####

know_psm <- gift2 %>%                                          # FIX 1
  mutate(year = factor(year)) %>%
  filter(complete.cases(
    know_bin, gift_bin, margin, edu, age, gen, p_id, type, ln_pop,
    last_party_vote, eth, year, ponderador_norm                  # FIX 2
  ))

k_psm <- matchit(
  know_bin ~ gift_bin + margin + edu + age + gen + p_id +
    type + last_party_vote + eth + ln_pop,
  data        = know_psm,
  method      = "nearest",
  distance    = "glm",
  link        = "logit",
  exact       = ~ year,
  s.weights   = know_psm$ponderador_norm,                        # FIX 2
  ratio       = 1,
  replace     = TRUE,
  caliper     = 0.2,
  std.caliper = TRUE
)

know_matched_data <- match.data(k_psm)
bal.tab(k_psm, un = TRUE, m.threshold = 0.1)
love.plot(k_psm, threshold = 0.1, abs = TRUE)

know_psm <- know_psm %>% mutate(distance = k_psm$distance)    # FIX 3

p_know_pre <- ggplot(know_psm,
                     aes(x = distance, fill = factor(know_bin))) +
  geom_density(alpha = 0.45, color = "black", linewidth = 0.3) +
  scale_fill_manual(
    values = c("0" = col_control, "1" = col_treated),
    labels = c("0" = "Control",   "1" = "Tratados"),
    name   = "Observó") +
  labs(x = "Propensity score", y = "Densidad") +
  theme_classic(base_family = "Times New Roman") +
  theme(legend.position = "right")
p_know_pre

ggsave("know_overlap_pre.pdf", plot = p_know_pre,
       width = 5, height = 4, device = "pdf")

# ---- Know bin: después ----
p_know_post <- ggplot(know_matched_data,
                      aes(x = distance, fill = factor(know_bin), weight = weights)) +
  geom_density(alpha = 0.45, color = "black", linewidth = 0.3) +
  scale_fill_manual(
    values = c("0" = col_control, "1" = col_treated),
    labels = c("0" = "Control",   "1" = "Tratados"),
    name   = "Observó") +
  labs(x = "Propensity score", y = "Densidad") +
  theme_classic(base_family = "Times New Roman") +
  theme(legend.position = "right")
p_know_post

ggsave("know_overlap_post.pdf", plot = p_know_post,
       width = 5, height = 4, device = "pdf")

know_vote <- know_matched_data %>% filter(!is.na(vote))

psm_att_know <- feols(
  vote ~ know_bin,
  data    = know_vote,
  weights = ~ weights,
  cluster = ~ clave_mun
)
summary(psm_att_know)


#### 3) excluded → turnout ####

exclu_psm <- gift2 %>%                                         # FIX 1
  mutate(year = factor(year)) %>%
  filter(complete.cases(
    excluded, know_bin, margin, edu, age, gen, p_id, type, ln_pop,
    last_party_vote, eth, year, ponderador_norm, vote            # FIX 2
  ))

ex_psm <- matchit(
  excluded ~ know_bin +                                        # FIX 4: añadir know_bin
    margin + edu + age + gen + p_id + type + last_party_vote + eth + ln_pop,
  # NOTA: gift_bin NO se incluye — entre tratados siempre es 0 → separación perfecta
  data        = exclu_psm,
  method      = "nearest",
  distance    = "glm",
  link        = "logit",
  exact       = ~ year,
  s.weights   = exclu_psm$ponderador_norm,                       # FIX 2
  ratio       = 1,
  replace     = TRUE,
  caliper     = 0.2,
  std.caliper = TRUE
)

exclu_matched_data <- match.data(ex_psm)
bal.tab(ex_psm, un = TRUE, m.threshold = 0.1)
love.plot(ex_psm, threshold = 0.1, abs = TRUE)

exclu_psm <- exclu_psm %>% mutate(distance = ex_psm$distance) # FIX 3

p_exclu_pre <- ggplot(exclu_psm,
                      aes(x = distance, fill = factor(excluded))) +
  geom_density(alpha = 0.45, color = "black", linewidth = 0.3) +
  scale_fill_manual(
    values = c("0" = col_control, "1" = col_treated),
    labels = c("0" = "Control",   "1" = "Tratados"),
    name   = "Excluido") +
  labs(x = "Propensity score", y = "Densidad") +
  theme_classic(base_family = "Times New Roman") +
  theme(legend.position = "right")
p_exclu_pre

ggsave("exclu_overlap_pre.pdf", plot = p_exclu_pre,
       width = 5, height = 4, device = "pdf")

# ---- Excluido: después ----
p_exclu_post <- ggplot(exclu_matched_data,
                       aes(x = distance, fill = factor(excluded), weight = weights)) +
  geom_density(alpha = 0.45, color = "black", linewidth = 0.3) +
  scale_fill_manual(
    values = c("0" = col_control, "1" = col_treated),
    labels = c("0" = "Control",   "1" = "Tratados"),
    name   = "Excluido") +
  labs(x = "Propensity score", y = "Densidad") +
  theme_classic(base_family = "Times New Roman") +
  theme(legend.position = "right")
p_exclu_post

ggsave("exclu_overlap_post.pdf", plot = p_exclu_post,
       width = 5, height = 4, device = "pdf")

psm_att_exclu <- feols(
  vote ~ excluded,
  data    = exclu_matched_data,
  weights = ~ weights,
  cluster = ~ clave_mun
)
summary(psm_att_exclu)

# ******************************************************************************
# ---- XXVII. Heterogeneidad ----
# ******************************************************************************

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

ess <- function(w) (sum(w, na.rm = TRUE)^2) / sum(w^2, na.rm = TRUE)

# ---- Helper principal: PSM + ATT en un subconjunto --------------------------

run_psm_att <- function(df,
                        treat, outcome,
                        other_gifts = character(0),
                        controls = c("margin","edu","age","gen","type","ln_pop",
                                     "last_party_vote","eth","p_id"),
                        year_var = "year",
                        wvar     = "ponderador_norm",          # FIX 2
                        cluster  = "clave_mun",
                        caliper  = 0.2,
                        min_n    = 30) {
  
  df <- df %>%
    mutate(
      year = factor(.data[[year_var]]),
      gen  = factor(gen)
    )
  
  need <- unique(c(treat, outcome, other_gifts, controls, "year", wvar, cluster))
  df   <- df %>% filter(complete.cases(across(all_of(need))))
  
  n_t <- sum(df[[treat]] == 1, na.rm = TRUE)
  n_c <- sum(df[[treat]] == 0, na.rm = TRUE)
  
  if (n_t < min_n || n_c < min_n) {
    return(tibble(
      ATT = NA_real_, SE = NA_real_, p = NA_real_,
      N_matched_C = NA_integer_, N_matched_T = NA_integer_,
      ESS_C = NA_real_, ESS_T = NA_real_,
      SMD_pscore_pre = NA_real_, SMD_pscore_post = NA_real_,
      Nota = paste0("Sin potencia (T=", n_t, ", C=", n_c, ")")
    ))
  }
  
  fml_match <- as.formula(
    paste(treat, "~", paste(c(other_gifts, controls), collapse = " + "))
  )
  
  m <- MatchIt::matchit(
    fml_match,
    data        = df,
    method      = "nearest",
    distance    = "glm",
    link        = "logit",
    exact       = ~ year,
    s.weights   = df[[wvar]],
    ratio       = 1,
    replace     = TRUE,
    caliper     = caliper,
    std.caliper = TRUE
  )
  
  md <- MatchIt::match.data(m)
  if (nrow(md) == 0) {
    return(tibble(
      ATT = NA_real_, SE = NA_real_, p = NA_real_,
      N_matched_C = NA_integer_, N_matched_T = NA_integer_,
      ESS_C = NA_real_, ESS_T = NA_real_,
      SMD_pscore_pre = NA_real_, SMD_pscore_post = NA_real_,
      Nota = "No units were matched"
    ))
  }
  
  att <- fixest::feols(
    as.formula(paste(outcome, "~", treat)),
    data    = md,
    weights = ~ weights,
    cluster = as.formula(paste0("~", cluster))
  )
  tt <- broom::tidy(att) %>% dplyr::filter(term == treat)
  
  # SMD pscore robusto
  bt  <- cobalt::bal.tab(m, un = TRUE)
  b   <- bt$Balance
  rn  <- tolower(rownames(b))
  i   <- which(rn == "distance")
  smd_pre  <- if (length(i) == 0) NA_real_ else as.numeric(b[i, "Diff.Un"])
  smd_post <- if (length(i) == 0) NA_real_ else as.numeric(b[i, "Diff.Adj"])
  
  tabN <- table(md[[treat]])
  wC   <- md$weights[md[[treat]] == 0]
  wT   <- md$weights[md[[treat]] == 1]
  
  tibble(
    ATT = tt$estimate,
    SE  = tt$std.error,
    p   = tt$p.value,
    N_matched_C     = as.integer(tabN["0"] %||% 0),
    N_matched_T     = as.integer(tabN["1"] %||% 0),
    ESS_C           = ess(wC),
    ESS_T           = ess(wT),
    SMD_pscore_pre  = smd_pre,
    SMD_pscore_post = smd_post,
    Nota            = ""
  )
}

# ******************************************************************************
#### (1) Heterogeneidad por COMPETITIVIDAD: vote buying ####
# ******************************************************************************

add_comp_strata <- function(df, margin_var = "margin") {
  df %>%
    mutate(
      comp_strata = case_when(
        .data[[margin_var]] <= 0.10 ~ "Alta competitividad",
        .data[[margin_var]] <= 0.30 ~ "Competitividad media",
        .data[[margin_var]]  > 0.30 ~ "Baja competitividad",
        TRUE ~ NA_character_
      ),
      comp_strata = factor(
        comp_strata,
        levels = c("Baja competitividad", "Competitividad media", "Alta competitividad")
      )
    )
}

run_by_strata <- function(data, strata_var, strata_levels,
                          treat, outcome, other_gifts, controls) {
  purrr::map_dfr(strata_levels, function(s) {
    df_s <- data %>% filter(.data[[strata_var]] == s)
    run_psm_att(
      df          = df_s,
      treat       = treat,
      outcome     = outcome,
      other_gifts = other_gifts,
      controls    = controls
    ) %>% mutate(Estrato = s)
  })
}

controls_base <- c("margin","edu","age","gen","type","ln_pop",
                   "last_party_vote","eth","p_id")

spec_vote_buying <- tibble::tribble(
  ~party, ~treat,     ~outcome,       ~other_gifts,
  "PAN",  "gift_PAN", "vote_PAN_tot", c("gift_PRI","gift_PRD","gift_MRN","gift_other"),
  "PRI",  "gift_PRI", "vote_PRI_tot", c("gift_PAN","gift_PRD","gift_MRN","gift_other"),
  "PRD",  "gift_PRD", "vote_PRD_tot", c("gift_PAN","gift_PRI","gift_MRN","gift_other"),
  "MRN",  "gift_MRN", "vote_MRN_tot", c("gift_PAN","gift_PRI","gift_PRD","gift_other")
)

gift_comp <- gift2 %>%                                   # FIX 1
  mutate(year = as.integer(as.character(year))) %>%
  add_comp_strata("margin")

levels_comp <- levels(gift_comp$comp_strata)

results_comp_vote_buying <- purrr::pmap_dfr(
  list(spec_vote_buying$party, spec_vote_buying$treat,
       spec_vote_buying$outcome, spec_vote_buying$other_gifts),
  function(party, treat, outcome, other_gifts) {
    run_by_strata(
      data          = gift_comp,
      strata_var    = "comp_strata",
      strata_levels = levels_comp,
      treat         = treat,
      outcome       = outcome,
      other_gifts   = other_gifts,
      controls      = controls_base
    ) %>%
      mutate(Partido = party, Tratamiento = treat, Outcome = outcome)
  }
) %>%
  mutate(
    stars = case_when(
      p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ ""
    ),
    `ATT (EE)` = ifelse(
      is.na(ATT), NA_character_,
      sprintf("%.3f%s (%.3f)", ATT, stars, SE)
    ),
    SMD_pscore_pre_post = ifelse(
      is.na(SMD_pscore_pre), NA_character_,
      sprintf("%.3f -> %.3f", SMD_pscore_pre, abs(SMD_pscore_post))
    )
  ) %>%
  select(Partido, Estrato, Outcome, Tratamiento, `ATT (EE)`,
         SMD_pscore_pre_post, N_matched_C, N_matched_T, ESS_C, ESS_T, Nota)

results_comp_vote_buying


# ******************************************************************************
#### (2) Heterogeneidad por COMPETITIVIDAD: turnout ####
# ******************************************************************************

spec_turnout <- tibble::tribble(
  ~party, ~treat,     ~outcome, ~other_gifts,
  "PAN",  "gift_PAN", "vote",   c("gift_PRI","gift_PRD","gift_MRN","gift_other"),
  "PRI",  "gift_PRI", "vote",   c("gift_PAN","gift_PRD","gift_MRN","gift_other"),
  "PRD",  "gift_PRD", "vote",   c("gift_PAN","gift_PRI","gift_MRN","gift_other"),
  "MRN",  "gift_MRN", "vote",   c("gift_PAN","gift_PRI","gift_PRD","gift_other")
)

gift_turn <- gift2 %>%                                   # FIX 1
  mutate(year = as.integer(as.character(year))) %>%
  add_comp_strata("margin")

results_comp_turnout <- purrr::pmap_dfr(
  list(spec_turnout$party, spec_turnout$treat,
       spec_turnout$outcome, spec_turnout$other_gifts),
  function(party, treat, outcome, other_gifts) {
    run_by_strata(
      data          = gift_turn,
      strata_var    = "comp_strata",
      strata_levels = levels_comp,
      treat         = treat,
      outcome       = outcome,
      other_gifts   = other_gifts,
      controls      = controls_base
    ) %>%
      mutate(Partido = party, Tratamiento = treat, Outcome = outcome)
  }
) %>%
  mutate(
    stars = case_when(
      p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ ""
    ),
    `ATT (EE)` = ifelse(
      is.na(ATT), NA_character_,
      sprintf("%.3f%s (%.3f)", ATT, stars, SE)
    ),
    SMD_pscore_pre_post = ifelse(
      is.na(SMD_pscore_pre), NA_character_,
      sprintf("%.3f -> %.3f", SMD_pscore_pre, abs(SMD_pscore_post))
    )
  ) %>%
  select(Partido, Estrato, Outcome, Tratamiento, `ATT (EE)`,
         SMD_pscore_pre_post, N_matched_C, N_matched_T, ESS_C, ESS_T, Nota)

results_comp_turnout


# ******************************************************************************
#### (3) Heterogeneidad por predisposición previa (last_party_vote) ####
# ******************************************************************************

add_lastvote_strata <- function(df, party_code,
                                last_vote_var = "last_party_vote") {
  df %>%
    mutate(
      last_vote_num = as.integer(as.character(.data[[last_vote_var]])),
      lastvote_strata = case_when(
        is.na(last_vote_num)          ~ "Sin historial / missing",
        last_vote_num == 0            ~ "Sin historial / missing",
        last_vote_num == party_code   ~ "Leal al partido j",
        TRUE                          ~ "Opositor"
      ),
      lastvote_strata = factor(
        lastvote_strata,
        levels = c("Leal al partido j", "Opositor", "Sin historial / missing")
      )
    )
}

run_psm_by_lastvote_strata <- function(data,
                                       treat, outcome, other_gifts,
                                       party_code,
                                       controls = c("edu","age","gen","type",
                                                    "ln_pop","eth","margin","p_id"),
                                       year_var      = "year",
                                       wvar          = "ponderador_norm",  # FIX 2
                                       cluster       = "clave_mun",
                                       caliper       = 0.2,
                                       min_n         = 30,
                                       last_vote_var = "last_party_vote") {
  
  df <- data %>%
    mutate(
      year = factor(.data[[year_var]]),
      gen  = factor(gen)
    ) %>%
    add_lastvote_strata(party_code    = party_code,
                        last_vote_var = last_vote_var)
  
  # last_vote_var excluida de complete.cases (NAs van al estrato "Sin historial")
  need <- c(treat, outcome, other_gifts, controls,
            "year", wvar, cluster, "lastvote_strata")
  df   <- df %>% filter(complete.cases(across(all_of(need))))
  
  out_list <- lapply(levels(df$lastvote_strata), function(s) {
    
    d_s <- df %>% filter(lastvote_strata == s)
    n_t <- sum(d_s[[treat]] == 1, na.rm = TRUE)
    n_c <- sum(d_s[[treat]] == 0, na.rm = TRUE)
    
    if (n_t < min_n || n_c < min_n) {
      return(tibble(
        Tratamiento = treat, Outcome = outcome, Estrato = s,
        ATT = NA_real_, SE = NA_real_, p = NA_real_,
        SMD_pscore_pre = NA_real_, SMD_pscore_post = NA_real_,
        N_matched_C = NA_integer_, N_matched_T = NA_integer_,
        ESS_C = NA_real_, ESS_T = NA_real_,
        Nota = paste0("Sin potencia (T=", n_t, ", C=", n_c, ")")
      ))
    }
    
    fml_match <- as.formula(
      paste(treat, "~", paste(c(other_gifts, controls), collapse = " + "))
    )
    
    m <- MatchIt::matchit(
      fml_match,
      data        = d_s,
      method      = "nearest",
      distance    = "glm",
      link        = "logit",
      exact       = ~ year,
      s.weights   = d_s[[wvar]],
      ratio       = 1,
      replace     = TRUE,
      caliper     = caliper,
      std.caliper = TRUE
    )
    
    md <- MatchIt::match.data(m)
    if (nrow(md) == 0) {
      return(tibble(
        Tratamiento = treat, Outcome = outcome, Estrato = s,
        ATT = NA_real_, SE = NA_real_, p = NA_real_,
        SMD_pscore_pre = NA_real_, SMD_pscore_post = NA_real_,
        N_matched_C = NA_integer_, N_matched_T = NA_integer_,
        ESS_C = NA_real_, ESS_T = NA_real_,
        Nota = "No units were matched"
      ))
    }
    
    att <- fixest::feols(
      as.formula(paste(outcome, "~", treat)),
      data    = md,
      weights = ~ weights,
      cluster = as.formula(paste0("~", cluster))
    )
    tt <- broom::tidy(att) %>% dplyr::filter(term == treat)
    
    bt  <- cobalt::bal.tab(m, un = TRUE)
    b   <- bt$Balance
    rn  <- tolower(rownames(b))
    i   <- which(rn == "distance")
    smd_pre  <- if (length(i) == 0) NA_real_ else as.numeric(b[i, "Diff.Un"])
    smd_post <- if (length(i) == 0) NA_real_ else as.numeric(b[i, "Diff.Adj"])
    
    tabN <- table(md[[treat]])
    wC   <- md$weights[md[[treat]] == 0]
    wT   <- md$weights[md[[treat]] == 1]
    
    tibble(
      Tratamiento     = treat,
      Outcome         = outcome,
      Estrato         = s,
      ATT             = tt$estimate,
      SE              = tt$std.error,
      p               = tt$p.value,
      SMD_pscore_pre  = smd_pre,
      SMD_pscore_post = smd_post,
      N_matched_C     = as.integer(tabN["0"] %||% 0),
      N_matched_T     = as.integer(tabN["1"] %||% 0),
      ESS_C           = ess(wC),
      ESS_T           = ess(wT),
      Nota            = ""
    )
  })
  
  dplyr::bind_rows(out_list)
}

party_code_map <- c(PAN = 1, PRI = 2, PRD = 3, MRN = 4)

spec_lastvote <- tibble::tribble(
  ~party, ~treat,     ~outcome,       ~other_gifts,
  "PAN",  "gift_PAN", "vote_PAN_tot", c("gift_PRI","gift_PRD","gift_MRN","gift_other"),
  "PRI",  "gift_PRI", "vote_PRI_tot", c("gift_PAN","gift_PRD","gift_MRN","gift_other"),
  "PRD",  "gift_PRD", "vote_PRD_tot", c("gift_PAN","gift_PRI","gift_MRN","gift_other"),
  "MRN",  "gift_MRN", "vote_MRN_tot", c("gift_PAN","gift_PRI","gift_PRD","gift_other")
)

results_lastvote <- purrr::pmap_dfr(
  list(spec_lastvote$party, spec_lastvote$treat,
       spec_lastvote$outcome, spec_lastvote$other_gifts),
  function(party, treat, outcome, other_gifts) {
    run_psm_by_lastvote_strata(
      data        = gift2,                       # FIX 1
      treat       = treat,
      outcome     = outcome,
      other_gifts = other_gifts,
      party_code  = party_code_map[[party]]
    ) %>% mutate(Partido = party)
  }
) %>%
  mutate(
    stars = case_when(
      p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ ""
    ),
    `ATT (EE)` = ifelse(
      is.na(ATT), NA_character_,
      sprintf("%.3f%s (%.3f)", ATT, stars, SE)
    ),
    SMD_pscore_pre_post = ifelse(
      is.na(SMD_pscore_pre), NA_character_,
      sprintf("%.3f -> %.3f", SMD_pscore_pre, abs(SMD_pscore_post))
    )
  ) %>%
  select(Partido, Estrato, Outcome, Tratamiento, `ATT (EE)`,
         SMD_pscore_pre_post, N_matched_C, N_matched_T, ESS_C, ESS_T, Nota)

results_lastvote


# ******************************************************************************
#### (4) Heterogeneidad por exposición múltiple (multi-regalo) ####
# ******************************************************************************

add_multi_strata_for_j <- function(df, treat, other_gifts) {
  df %>%
    mutate(
      other_any = as.integer(
        rowSums(across(all_of(other_gifts),
                       ~ as.integer(.x == 1)), na.rm = TRUE) >= 1
      ),
      multi_strata = case_when(
        .data[[treat]] == 1 & other_any == 0 ~ "Solo-j",
        .data[[treat]] == 1 & other_any == 1 ~ "Multi-j",
        .data[[treat]] == 0 & other_any == 0 ~ "Control limpio",
        .data[[treat]] == 0 & other_any == 1 ~ "Control multi",
        TRUE ~ NA_character_
      ),
      multi_strata = factor(
        multi_strata,
        levels = c("Control limpio","Solo-j","Control multi","Multi-j")
      )
    )
}

run_psm_by_multi_strata <- function(data,
                                    treat, outcome, other_gifts,
                                    controls = c("edu","age","gen","type","ln_pop",
                                                 "last_party_vote","eth","margin","p_id"),
                                    year_var = "year",
                                    wvar     = "ponderador_norm",      # FIX 2
                                    cluster  = "clave_mun",
                                    caliper  = 0.2,
                                    min_n    = 30) {
  
  df <- data %>%
    mutate(
      year = factor(.data[[year_var]]),
      gen  = factor(gen)
    ) %>%
    add_multi_strata_for_j(treat = treat, other_gifts = other_gifts)
  
  need <- c(treat, outcome, other_gifts, controls,
            "year", wvar, cluster, "multi_strata")
  df   <- df %>% filter(complete.cases(across(all_of(need))))
  
  comps <- list(
    list(name = "Solo-j",  keep = c("Solo-j",  "Control limpio")),
    list(name = "Multi-j", keep = c("Multi-j", "Control multi"))
  )
  
  out_list <- lapply(comps, function(cc) {
    
    d_s <- df %>% filter(multi_strata %in% cc$keep)
    n_t <- sum(d_s[[treat]] == 1, na.rm = TRUE)
    n_c <- sum(d_s[[treat]] == 0, na.rm = TRUE)
    
    if (n_t < min_n || n_c < min_n) {
      return(tibble::tibble(
        Estrato             = cc$name,
        ATT = NA_real_, SE = NA_real_, p = NA_real_,
        SMD_pscore_pre_post = NA_character_,
        N_matched_C = NA_integer_, N_matched_T = NA_integer_,
        ESS_C = NA_real_, ESS_T = NA_real_,
        Nota = paste0("Sin potencia (T=", n_t, ", C=", n_c, ")")
      ))
    }
    
    fml_match <- as.formula(
      paste(treat, "~", paste(c(other_gifts, controls), collapse = " + "))
    )
    
    m <- MatchIt::matchit(
      fml_match,
      data        = d_s,
      method      = "nearest",
      distance    = "glm",
      link        = "logit",
      exact       = ~ year,
      s.weights   = d_s[[wvar]],
      ratio       = 1,
      replace     = TRUE,
      caliper     = caliper,
      std.caliper = TRUE
    )
    
    md <- MatchIt::match.data(m)
    
    att <- fixest::feols(
      as.formula(paste(outcome, "~", treat)),
      data    = md,
      weights = ~ weights,
      cluster = as.formula(paste0("~", cluster))
    )
    tt <- broom::tidy(att) %>% dplyr::filter(term == treat)
    
    # SMD robusto
    bt  <- cobalt::bal.tab(m, un = TRUE)
    b   <- bt$Balance
    rn  <- tolower(rownames(b))
    i   <- which(rn == "distance")
    smd_str <- if (length(i) == 0) NA_character_ else
      sprintf("%.3f -> %.3f",
              as.numeric(b[i, "Diff.Un"]),
              abs(as.numeric(b[i, "Diff.Adj"])))
    
    tabN <- table(md[[treat]])
    wC   <- md$weights[md[[treat]] == 0]
    wT   <- md$weights[md[[treat]] == 1]
    
    tibble::tibble(
      Estrato             = cc$name,
      ATT                 = tt$estimate,
      SE                  = tt$std.error,
      p                   = tt$p.value,
      SMD_pscore_pre_post = smd_str,
      N_matched_C         = as.integer(tabN["0"] %||% 0),
      N_matched_T         = as.integer(tabN["1"] %||% 0),
      ESS_C               = ess(wC),
      ESS_T               = ess(wT),
      Nota                = ""
    )
  })
  
  dplyr::bind_rows(out_list)
}

spec_multi <- tibble::tribble(
  ~party, ~treat,     ~outcome,       ~other_gifts,
  "PAN",  "gift_PAN", "vote_PAN_tot", c("gift_PRI","gift_PRD","gift_MRN","gift_other"),
  "PRI",  "gift_PRI", "vote_PRI_tot", c("gift_PAN","gift_PRD","gift_MRN","gift_other"),
  "PRD",  "gift_PRD", "vote_PRD_tot", c("gift_PAN","gift_PRI","gift_MRN","gift_other"),
  "MRN",  "gift_MRN", "vote_MRN_tot", c("gift_PAN","gift_PRI","gift_PRD","gift_other")
)

results_multi <- purrr::pmap_dfr(
  list(spec_multi$treat, spec_multi$outcome,
       spec_multi$other_gifts, spec_multi$party),
  function(treat, outcome, other_gifts, party) {
    run_psm_by_multi_strata(
      data        = gift2,                       # FIX 1
      treat       = treat,
      outcome     = outcome,
      other_gifts = other_gifts
    ) %>%
      dplyr::mutate(Partido = party, Tratamiento = treat, Outcome = outcome)
  }
) %>%
  dplyr::mutate(
    stars = dplyr::case_when(
      p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ ""
    ),
    `ATT (EE)` = ifelse(
      is.na(ATT), NA_character_,
      sprintf("%.3f%s (%.3f)", ATT, stars, SE)
    )
  ) %>%
  dplyr::select(Partido, Estrato, Outcome, Tratamiento, `ATT (EE)`,
                SMD_pscore_pre_post, N_matched_C, N_matched_T,
                ESS_C, ESS_T, Nota)

results_multi


# ******************************************************************************
#### (5) Heterogeneidad por presencia partidista (pres_*_12) ####
# ******************************************************************************

add_presence_strata <- function(df, pres_var) {
  df %>%
    mutate(
      pres_strata = case_when(
        .data[[pres_var]] <  0.33 ~ "Baja presencia",
        .data[[pres_var]] <  0.66 ~ "Presencia media",
        .data[[pres_var]] >= 0.66 ~ "Alta presencia",
        TRUE ~ NA_character_
      ),
      pres_strata = factor(
        pres_strata,
        levels = c("Baja presencia","Presencia media","Alta presencia")
      )
    )
}

run_psm_by_presence_strata <- function(data,
                                       treat, outcome, other_gifts, pres_var,
                                       controls = c("edu","age","gen","type","ln_pop",
                                                    "last_party_vote","eth","margin","p_id"),
                                       year_var = "year",
                                       wvar     = "ponderador_norm",      # FIX 2
                                       cluster  = "clave_mun",
                                       caliper  = 0.2,
                                       min_n    = 30) {
  
  df <- data %>%
    mutate(
      year = factor(.data[[year_var]]),
      gen  = factor(gen)
    ) %>%
    add_presence_strata(pres_var = pres_var)
  
  need <- c(treat, outcome, other_gifts, controls,
            pres_var, "year", wvar, cluster, "pres_strata")
  df   <- df %>% filter(complete.cases(across(all_of(need))))
  
  out_list <- lapply(levels(df$pres_strata), function(s) {
    
    d_s <- df %>% filter(pres_strata == s)
    n_t <- sum(d_s[[treat]] == 1, na.rm = TRUE)
    n_c <- sum(d_s[[treat]] == 0, na.rm = TRUE)
    
    if (n_t < min_n || n_c < min_n) {
      return(tibble(
        Tratamiento = treat, Outcome = outcome, Estrato = s,
        ATT = NA_real_, SE = NA_real_, p = NA_real_,
        SMD_pscore_pre = NA_real_, SMD_pscore_post = NA_real_,
        N_matched_C = NA_integer_, N_matched_T = NA_integer_,
        ESS_C = NA_real_, ESS_T = NA_real_,
        Nota = paste0("Sin potencia (T=", n_t, ", C=", n_c, ")")
      ))
    }
    
    # pres_var NO en la fórmula PS — es la variable de estratificación
    fml_match <- as.formula(
      paste(treat, "~", paste(c(other_gifts, controls), collapse = " + "))
    )
    
    m <- MatchIt::matchit(
      fml_match,
      data        = d_s,
      method      = "nearest",
      distance    = "glm",
      link        = "logit",
      exact       = ~ year,
      s.weights   = d_s[[wvar]],
      ratio       = 1,
      replace     = TRUE,
      caliper     = caliper,
      std.caliper = TRUE
    )
    
    md <- MatchIt::match.data(m)
    
    att <- fixest::feols(
      as.formula(paste(outcome, "~", treat)),
      data    = md,
      weights = ~ weights,
      cluster = as.formula(paste0("~", cluster))
    )
    tt <- broom::tidy(att) %>% dplyr::filter(term == treat)
    
    # SMD robusto (FIX +)
    bt  <- cobalt::bal.tab(m, un = TRUE)
    b   <- bt$Balance
    rn  <- tolower(rownames(b))
    i   <- which(rn == "distance")
    smd_pre  <- if (length(i) == 0) NA_real_ else as.numeric(b[i, "Diff.Un"])
    smd_post <- if (length(i) == 0) NA_real_ else as.numeric(b[i, "Diff.Adj"])
    
    tabN <- table(md[[treat]])
    wC   <- md$weights[md[[treat]] == 0]
    wT   <- md$weights[md[[treat]] == 1]
    
    tibble(
      Tratamiento     = treat,
      Outcome         = outcome,
      Estrato         = s,
      ATT             = tt$estimate,
      SE              = tt$std.error,
      p               = tt$p.value,
      SMD_pscore_pre  = smd_pre,
      SMD_pscore_post = smd_post,
      N_matched_C     = as.integer(tabN["0"] %||% 0),
      N_matched_T     = as.integer(tabN["1"] %||% 0),
      ESS_C           = ess(wC),
      ESS_T           = ess(wT),
      Nota            = ""
    )
  })
  
  dplyr::bind_rows(out_list)
}

spec_pres <- tibble::tribble(
  ~party, ~treat,     ~outcome,       ~pres_var,     ~other_gifts,
  "PAN",  "gift_PAN", "vote_PAN_tot", "pres_pan_12", c("gift_PRI","gift_PRD","gift_MRN","gift_other"),
  "PRI",  "gift_PRI", "vote_PRI_tot", "pres_pri_12", c("gift_PAN","gift_PRD","gift_MRN","gift_other"),
  "PRD",  "gift_PRD", "vote_PRD_tot", "pres_prd_12", c("gift_PAN","gift_PRI","gift_MRN","gift_other"),
  "MRN",  "gift_MRN", "vote_MRN_tot", "pres_mrn_12", c("gift_PAN","gift_PRI","gift_PRD","gift_other")
)

results_pres <- purrr::pmap_dfr(
  list(spec_pres$party, spec_pres$treat, spec_pres$outcome,
       spec_pres$pres_var, spec_pres$other_gifts),
  function(party, treat, outcome, pres_var, other_gifts) {
    run_psm_by_presence_strata(
      data        = gift4,                       # FIX 3
      treat       = treat,
      outcome     = outcome,
      other_gifts = other_gifts,
      pres_var    = pres_var
    ) %>%
      mutate(Partido = party)
  }
)

results_pres_tab <- results_pres %>%
  mutate(
    stars = case_when(
      p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ ""
    ),
    `ATT (EE)` = ifelse(
      is.na(ATT), NA_character_,
      sprintf("%.3f%s (%.3f)", ATT, stars, SE)
    ),
    SMD_pscore_pre_post = ifelse(
      is.na(SMD_pscore_pre), NA_character_,
      sprintf("%.3f -> %.3f", SMD_pscore_pre, abs(SMD_pscore_post))
    )
  ) %>%
  select(Partido, Estrato, Outcome, Tratamiento, `ATT (EE)`,
         SMD_pscore_pre_post, N_matched_C, N_matched_T, ESS_C, ESS_T, Nota)

results_pres_tab

# ******************************************************************************
#  ---- XXVIII. Pruebas de robustez ----
# ******************************************************************************

# Modelos sometidos a prueba (resultados PSM-ATT con significancia estadística):
#   (1) H1 PRD:   gift_PRD  → vote_PRD_tot   (ATT = 0.115**)
#   (2) H4 PRD:   gift_PRD  → pid_PRD        (ATT = 0.067*)
#   (3) H5 Excl:  excluded  → vote           (ATT = -0.035*)
#
# Pruebas aplicadas:
#   (i)  E-values (EValue::evalues.OLS desde ATT/SE)
#   (ii) Rosenbaum bounds (rbounds) con matching 1:1 sin reemplazo

# ---- Helpers ----------------------------------------------------------------

wtd_sd <- function(x, w){
  ok <- is.finite(x) & is.finite(w) & w > 0
  x  <- x[ok]; w <- w[ok]
  m  <- sum(w * x) / sum(w)
  sqrt(sum(w * (x - m)^2) / sum(w))
}

get_att_from_feols <- function(model, treat){
  ct <- summary(model)$coeftable
  tibble(
    att = unname(ct[treat, "Estimate"]),
    se  = unname(ct[treat, "Std. Error"]),
    p   = unname(ct[treat, "Pr(>|t|)"])
  )
}

get_obj <- function(x) get(x, envir = .GlobalEnv)

# ---- E-value desde ATT (LPM) ------------------------------------------------

evalue_from_att <- function(md, treat, outcome, att, se, true = 0, delta = 1){
  
  sd_y_w <- wtd_sd(md[[outcome]], md$weights)
  ev     <- EValue::evalues.OLS(est = att, se = se, sd = sd_y_w,
                                delta = delta, true = true)
  
  RR_point <- unname(ev["RR",       "point"])
  RR_lo    <- unname(ev["RR",       "lower"])
  RR_hi    <- unname(ev["RR",       "upper"])
  Ev_point <- unname(ev["E-values", "point"])
  
  crosses_null <- isTRUE(RR_lo <= 1 & RR_hi >= 1)
  Ev_ci <- if (crosses_null) 1 else {
    cand <- c(unname(ev["E-values", "lower"]),
              unname(ev["E-values", "upper"]))
    cand <- cand[is.finite(cand)]
    if (length(cand) == 0) NA_real_ else min(cand)
  }
  
  tibble(RR_used    = RR_point,
         RR_lo      = RR_lo,
         RR_hi      = RR_hi,
         E_value    = Ev_point,
         E_value_CI = Ev_ci,
         sd_y_w     = sd_y_w)
}

# ---- Gamma crítico ----------------------------------------------------------

gamma_crit_from_pupper <- function(df, alpha = 0.05){
  ok <- df %>% filter(!is.na(p_upper))
  if (nrow(ok) == 0) return(NA_real_)
  g <- ok$Gamma[which(ok$p_upper >= alpha)]
  if (length(g) == 0) return(NA_real_)
  min(g)
}

# ---- Rosenbaum: matching 1:1 sin reemplazo ----------------------------------

run_rb_one <- function(df, fml_match, treat, outcome,
                       outcome_type = c("continuous", "binary"),
                       year_var     = "year",
                       wvar         = "ponderador_norm",
                       caliper      = 0.2,
                       gamma_max    = 2,
                       gamma_step   = 0.1){
  
  outcome_type <- match.arg(outcome_type)
  
  df2 <- df %>%
    mutate(year = factor(.data[[year_var]])) %>%
    select(-any_of("distance")) %>%        # <-- fix: elimina columna conflictiva
    filter(!is.na(.data[[treat]]),
           !is.na(.data[[outcome]]),
           !is.na(.data[[wvar]]),
           .data[[wvar]] > 0)
  
  m_nr <- MatchIt::matchit(
    fml_match,
    data        = df2,
    method      = "nearest",
    distance    = "glm",
    link        = "logit",
    exact       = ~ year,
    s.weights   = df2[[wvar]],
    ratio       = 1,
    replace     = FALSE,
    caliper     = caliper,
    std.caliper = TRUE
  )
  
  md <- MatchIt::match.data(m_nr)
  
  if (!("subclass" %in% names(md))){
    return(tibble(Gamma = NA_real_, p_lower = NA_real_, p_upper = NA_real_,
                  hl_lower = NA_real_, hl_upper = NA_real_,
                  n_pairs = NA_integer_,
                  Gamma_crit_05 = NA_real_, Gamma_crit_10 = NA_real_,
                  Nota = "match.data() no trajo 'subclass'"))
  }
  
  md    <- md %>% filter(!is.na(subclass))
  pairs <- md %>%
    group_by(subclass) %>%
    summarise(y_t = .data[[outcome]][.data[[treat]] == 1][1],
              y_c = .data[[outcome]][.data[[treat]] == 0][1],
              .groups = "drop") %>%
    filter(!is.na(y_t), !is.na(y_c))
  
  n_pairs <- nrow(pairs)
  if (n_pairs < 30){
    return(tibble(Gamma = NA_real_, p_lower = NA_real_, p_upper = NA_real_,
                  hl_lower = NA_real_, hl_upper = NA_real_,
                  n_pairs = n_pairs,
                  Gamma_crit_05 = NA_real_, Gamma_crit_10 = NA_real_,
                  Nota = paste0("Muy pocos pares 1:1 (n_pairs=", n_pairs, ")")))
  }
  
  gammas <- seq(1, gamma_max, by = gamma_step)
  
  # Outcome continuo: psens + hlsens
  ps <- rbounds::psens(x = pairs$y_t, y = pairs$y_c,
                       Gamma = gamma_max, GammaInc = gamma_step)
  hl <- rbounds::hlsens(x = pairs$y_t, y = pairs$y_c,
                        Gamma = gamma_max, GammaInc = gamma_step)
  
  tibble(Gamma    = seq(1, gamma_max, by = gamma_step),
         p_lower  = ps$bounds[, "Lower bound"],
         p_upper  = ps$bounds[, "Upper bound"],
         hl_lower = hl$bounds[, "Lower bound"],
         hl_upper = hl$bounds[, "Upper bound"],
         n_pairs  = n_pairs,
         Nota     = "") %>%
    mutate(Gamma_crit_05 = gamma_crit_from_pupper(pick(everything()), 0.05),
           Gamma_crit_10 = gamma_crit_from_pupper(pick(everything()), 0.10))
}

# ==============================================================================
#  1) E-values
# ==============================================================================

robust_list <- tibble::tribble(
  ~label,       ~md_obj,            ~model_obj,       ~treat,      ~outcome,
  "Vote_PRD",   "prd_matched",      "psm_att_prd",    "gift_PRD",  "vote_PRD_tot",
  "PID_PRD",    "prd_pid_matched",  "att_pid_prd",    "gift_PRD",  "pid_PRD",
  "Exclu_vote", "exclu_matched_data",       "psm_att_exclu",  "excluded",  "vote"
)

evalues_results <- pmap_dfr(
  list(robust_list$label, robust_list$md_obj, robust_list$model_obj,
       robust_list$treat, robust_list$outcome),
  function(label, md_name, model_name, treat, outcome){
    md    <- get_obj(md_name)
    model <- get_obj(model_name)
    est   <- get_att_from_feols(model, treat)
    ev    <- evalue_from_att(md = md, treat = treat, outcome = outcome,
                             att = est$att, se = est$se)
    tibble(label = label, treat = treat, outcome = outcome,
           ATT = est$att, SE = est$se, p = est$p) %>%
      bind_cols(ev)
  }
)

evalues_results

# ==============================================================================
#  2) Rosenbaum bounds
# ==============================================================================

# NOTA: pan_psm, prd_psm y pan_inc_psm son los dataframes PRE-matching
# (el objeto que pasaste como data = ... a matchit() en H1 y H2).
# Confirma que esos nombres existen en tu entorno antes de correr.

rb_list <- tribble(
  ~label,        ~df_obj,        ~fml,
  ~treat,        ~outcome,
  
  "Vote_PRD",
  "prd_psm",
  gift_PRD ~ gift_PAN + gift_PRI + gift_MRN + gift_other +
    margin + edu + age + gen + p_id + type + last_party_vote + eth + ln_pop,
  "gift_PRD", "vote_PRD_tot",
  
  "PID_PRD",
  "pid_prd_psm",
  gift_PRD ~ gift_MRN + gift_PAN + gift_PRI + gift_other +
    margin + edu + age + gen + type + last_party_vote + eth + ln_pop +
    pid_PAN + pid_PRI + pid_MRN + pid_none + pid_other,
  "gift_PRD", "pid_PRD",
  
  "Exclu_vote",
  "exclu_psm",
  excluded ~ know_bin +
    margin + edu + age + gen + p_id + type + last_party_vote + eth + ln_pop,
  "excluded", "vote"
)

rosenbaum_grid <- pmap_dfr(
  list(rb_list$label, rb_list$df_obj, rb_list$fml,
       rb_list$treat, rb_list$outcome),
  function(label, df_name, fml, treat, outcome){
    df  <- get_obj(df_name)
    out <- run_rb_one(df = df, fml_match = fml, treat = treat,
                      outcome = outcome, outcome_type = "continuous",
                      gamma_max = 2, gamma_step = 0.1)
    out %>% mutate(label = label, treat = treat, outcome = outcome)
  }
)

rosenbaum_summary <- rosenbaum_grid %>%
  group_by(label, treat, outcome) %>%
  summarise(n_pairs       = max(n_pairs, na.rm = TRUE),
            Gamma_crit_05 = max(Gamma_crit_05, na.rm = TRUE),
            Gamma_crit_10 = max(Gamma_crit_10, na.rm = TRUE),
            Nota          = first(Nota),
            .groups = "drop")

rosenbaum_grid
rosenbaum_summary

# ==============================================================================
#  3) Gráficas Rosenbaum
# ==============================================================================

plot_rb <- function(grid, label_sel, alpha = 0.05){
  grid %>%
    filter(label == label_sel) %>%
    ggplot(aes(x = Gamma, y = p_upper)) +
    geom_line(linewidth = 0.8) +
    geom_hline(yintercept = alpha, linetype = "dashed", color = "grey40") +
    labs(x      = expression(Gamma),
         y      = "Upper bound p-value",
         title  = paste0("Rosenbaum bounds: ", label_sel),
         subtitle = paste0("Línea punteada: \u03b1 = ", alpha)) +
    theme_minimal(base_family = "Times New Roman")
}

plot_rb(rosenbaum_grid, "Vote_PRD",    alpha = 0.05)
plot_rb(rosenbaum_grid, "PID_PRD",     alpha = 0.05)
plot_rb(rosenbaum_grid, "Exclu_vote",  alpha = 0.05)

ggsave("rb_vote_prd.pdf",
       plot = plot_rb(rosenbaum_grid, "Vote_PRD",   alpha = 0.05),
       width = 6, height = 4, device = "pdf")

ggsave("rb_pid_prd.pdf",
       plot = plot_rb(rosenbaum_grid, "PID_PRD",    alpha = 0.05),
       width = 6, height = 4, device = "pdf")

ggsave("rb_exclu_vote.pdf",
       plot = plot_rb(rosenbaum_grid, "Exclu_vote", alpha = 0.05),
       width = 6, height = 4, device = "pdf")

