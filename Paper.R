  
  ########################################################
  ################                        ################
  ################       TI, H and Au     ################
  ################       23/05/2024       ################
  ################                        ################
  ########################################################
  # usethis::edit_file("~/AppData/Roaming/RStudio/templates/default.R")

  
  {
    extrafont::loadfonts(device = "win", quiet = T)
    options(timeout = max(1000, getOption("timeout")))
    
    if (!require("pacman")) install.packages("pacman") else library(pacman)
    pacman::p_load(tidyverse, magrittr,
                   readxl, openxlsx, sf, geobr, janitor, broom, biscale, ggtext, cowplot, patchwork,
                   EnvStats, broom, estimatr, lmtest, plm, fixest, fastDummies, car, rlang)
  }
  
  # ---- Funções para os Gráficos ----
  
  ## Gráfico de Tendências
  plot_trend <- function(data) {
    data %>%  
    group_by(ano, treatment_unit) %>% 
    summarise(hom_tot = sum(hom_tot, na.rm = T), pop = sum(pop, na.rm = T)) %>% 
    mutate(tx_hom_tot = hom_tot / pop * 100000) %>% 
    
    {ggplot(., aes(x = ano, y = tx_hom_tot, color = factor(treatment_unit))) +
      geom_vline(xintercept = '2018', linetype = "dashed", color = "black") +
      geom_line(linewidth= 0.8, aes(group = treatment_unit)) +
      geom_point(size = 2) +
      labs(
        title = "Taxa de Homicídios ao longo do tempo por grupo",
        x = "Ano",
        y = "Taxa de Homicídios (por 100 mil habitantes)",
        color = "Grupo"
      ) +
      theme_minimal() +
      scale_color_manual(labels = c("Controle", "Tratado"), values = c("steelblue", "tomato")) +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10)
      )}
  }

  ## Razão de Homicídios (Treatment/Control) ao longo do Tempo
  plot_ratio <- function(data, x_column, y_column) {
    data %>% 
    group_by(ano, treatment_unit) %>% 
    summarise(hom_tot = sum(hom_tot, na.rm = T), pop = sum(pop, na.rm = T)) %>% 
    mutate(tx_hom_tot = hom_tot / pop * 100000) %>% 
    group_by(ano) %>%
    select(-hom_tot, -pop) %>% 
    pivot_wider(
      names_from = treatment_unit,
      values_from = tx_hom_tot,
      names_prefix = "treated_"
    ) %>% 
    mutate(razao_tx_hom_tot = treated_1 / treated_0) %>%
        
    {ggplot(., aes(x = {{x_column}}, y = {{y_column}})) +
    geom_vline(xintercept = '2018', linetype = "dashed", color = "black") +
    geom_line(size = 0.8, group = 1) +
    geom_point(size = 2) +
    labs(
      title = "Razão da Taxa de Homicídios ao longo do tempo",
      y = "Razão Taxa de Homicídios",
      color = "Grupo"
    ) +
    theme_minimal() +
    scale_color_manual(labels = c("Controle", "Tratado"), values = c("steelblue", "tomato")) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10)
    )}
  }

  ## Event Study
  plot_event_study <- function(data, x_column, y_column, remove_year) {
    ggplot(data, aes(x = {{x_column}}, y = {{y_column}})) +
    geom_point() +
    geom_errorbar(aes(ymin = {{y_column}} - 1.96 * std.error,
                      ymax = {{y_column}} + 1.96 * std.error)) +
    geom_vline(xintercept = remove_year, linetype = "dashed", color = "blue") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    scale_x_continuous(breaks = seq(2010, 2022, by = 1)) +
    labs(x = "Years", #"Years relative to treatment (2018 = 0)",
         y = "DDD coefficient (Gold x TI x year)",
         title = "Dynamic DDD") +
    theme_minimal()
  }

  ## Plot Grid
  make_plot_grid <- function(plot_type, ncol = 4) {
  dict %>%
    dplyr::pull(model_name) %>%
    as.character() %>%
    purrr::keep(~ .x %in% names(workbook[[plot_type]])) %>%
    purrr::map(~ workbook[[plot_type]][[.x]] +
                 ggtitle(.x) +
                 theme(
                   plot.title = element_text(hjust = 0.5, face = "bold"),
                   plot.margin = margin(6, 6, 6, 6)
                 )) %>%
    patchwork::wrap_plots(ncol = ncol) &
    theme(legend.position = "bottom")
  }


  # ---- Função para os Modelos ----

  deploy_models <- function(idx, model_name, sample_filter_expr, treatment_condition_expr, years, model_expr) {

    # Dataset adjust to modelling
    temp_dataset <- dataset %>%
      { if (is.na(sample_filter_expr)) . else eval_tidy(parse_expr(sample_filter_expr), data = .) } %>% # Additional filter
      # Step 1: Create a treatment indicator that is 1 for treated units in all periods
      mutate(treatment_unit = ifelse(any(!!parse_expr(treatment_condition_expr)), 1, 0), .by = code_muni) %>%
      # Step 2: Create time period dummies interacted with the Rebate_dummy
      mutate(!!!setNames(
        map(years, ~expr(ifelse(ano == !!.x, treatment_unit, 0))),
        paste0("treatment_unit_", years)
      ))
      # Adicionar resultado à lista usando índice
      {
        workbook$dataset[[idx]] <<- temp_dataset
        names(workbook$dataset)[idx] <<- model_name
      }
      {
        workbook$plot_trend[[idx]] <<- plot_trend(temp_dataset)
        names(workbook$plot_trend)[idx] <<- model_name
      }
      {
        workbook$plot_ratio[[idx]] <<- plot_ratio(temp_dataset, ano, razao_tx_hom_tot)
        names(workbook$plot_ratio)[idx] <<- model_name      
      }
    
    # Step 3: Creating Event Study & Obtain cluster-robust standard errors
    event_study_robust_se <- plm(
      as.formula(paste("tx_hom_tot ~", paste(paste0("treatment_unit_", years), collapse = " + "))),
      data = pdata.frame(temp_dataset, index = c("name_muni", "ano")), 
      model = "within", effect = "twoway"
      ) %>% 
      coeftest(., vcov = vcovHC(., type = "HC1", cluster = "group")) %>% 
      # Step 5: Extract coefficients for plotting
      tidy() %>%
      filter(grepl("treatment_unit_", term))
      {
        workbook$event_study_rse[[idx]] <<- event_study_robust_se
        names(workbook$event_study_rse)[idx] <<- model_name
      }
      {
        workbook$plot_event_study[[idx]] <<- plot_event_study(event_study_robust_se, 
                                                      as.numeric(gsub("treatment_unit_", "", term)) - 2018, estimate)
        names(workbook$plot_event_study)[idx] <<- model_name
      }
    
    # Step 6: Model (plm package)
    model_plm <- plm(
      as.formula(model_expr),
      data = pdata.frame(temp_dataset, index = c("name_muni", "ano")), 
      model = "within", effect = "twoways"
    ) %>%
    coeftest(., vcov = vcovHC(., type = "HC1", cluster = "group"))
      {
        workbook$model[[idx]] <<- model_plm
        names(workbook$model)[idx] <<- model_name
      }

  }

  
  # ---- Importando Dados ----
  # O ideal é montar um base com:
  # 5570 (municípios) * 13 (anos entre 2010-2022) = 72410 observações

  ## Limites dos Municípios 
  # Fonte: {geobr} apud IBGE 
  
  municipalities <- read_municipality(year = 2022) %>%
                    select(code_muni, name_muni, abbrev_state, geometry = geom) %>% 
                    mutate(code_muni = str_sub(as.character(code_muni), 1, 6), # excluindo 7º (dígito verificador)
                          name_muni = gsub("Pontes E Lacerda", "Pontes e Lacerda", name_muni),
                          name_muni = gsub("D'oeste", "D´Oeste", name_muni),
                          name_muni = gsub("D'arco", "D´Arco", name_muni),
                          name_muni = str_c(name_muni, "/", abbrev_state)) %>% 
                    filter(!(code_muni %in% "430000")) # https://github.com/ipeaGIT/geobr/issues/176
  
  
  ## Municípios Limítrofes
  # Fonte: IBGE
  
  neighboring_municipalities <- read_xlsx("./1. Dados Municipais/Municipios Limitrofes - IBGE (2024).xlsx", sheet = 1) %>% 
                                clean_names() %>% 
                                rename(code_muni = cd_mun) %>% 
                                mutate(code_muni = as.character(substr(code_muni, 1, 6)), lim = 1)
  
  
  ## Municípios - Bioma Predominante
  # Fonte: IBGE
  
  biomes <- read_xlsx("./1. Dados Municipais/Bioma Predominante por Município - IBGE (2024).xlsx", sheet = 1, skip = 1) %>% 
            clean_names() %>% 
            rename(bioma = bioma_predominante)
  
  
  ## Territórios Indígenas 
  # Fonte: FUNAI 
  
  indigenous_lands <- read_sf("./2. Terras Indígenas - Shapefile/tis_poligonais_portariasPolygon.shp",
                              options = "ENCODING=WINDOWS-1252") %>% 
                      mutate(across(where(is.character), str_trim),
                             id_ti = row_number(), .before = gid) %>% 
                      mutate(municipio_ = gsub("Poxoréo", "Poxoréu", municipio_),
                             municipio_ = case_when(municipio_ == "Muquém de São Francisco"   ~ "Muquém do São Francisco",
                                                    municipio_ == "Santo Antônio do Leverger" ~ "Santo Antônio de Leverger",
                                                    T ~ as.character(municipio_))) 
  
  
  ## Províncias e Distritos Auríferos 
  # Fonte: SGB
  
  gold_reserves <- list(province = read_sf("./3. Províncias e Distritos Auríferos do Brasil - Shapefile/Distritos_Provincias/prov_dist_au_provincias_br.shp"),
                        district  = read_sf("./3. Províncias e Distritos Auríferos do Brasil - Shapefile/Distritos_Provincias/prov_dist_au_distritos_br.shp")) %>%
                   attach()
  

  ## População Residente por Município
  # Fonte: DataSUS apud IBGE; Obs.: 2010 e 2022 representam números de Censo

  population <- read_xlsx("./1. Dados Municipais/Estimativa de População IBGE TCU (2010-2022).xlsx", skip = 3) %>%
                  filter(!grepl("IGNORADO", Município)) %>% 
                  head(5570) %>% 
                  mutate(across(where(is.character) & -Município, ~ as.numeric(na_if(.x, "-")))) %>% 
                  pivot_longer(2:length(.), names_to = "ano", values_to = "pop") %>% 
                  clean_names() %>%
                  separate_wider_regex(municipio, c(code_muni = ".*?", " ", municipio = ".*"))


  ## Homicídios (totais e indígenas) por Município 
  # Fonte: SIM/MS apud DataSUS/Tabnet/MS e Atlas da Violência 2023 
  
  homicides = list(
    
    homicides_ds = pmap(list(caminho = c("./1. Dados Municipais/Óbitos totais por município - Tabnet DataSUS (2010-2022).xlsx", "./1. Dados Municipais/Óbitos de indígenas por município - Tabnet DataSUS (2010-2022).xlsx"),
                              skip = c(4, 5),
                              variavel = c("hom_tot", "hom_ind")),
                         
                         \(caminho, skip, variavel) {
                           
                           read_xlsx(caminho, skip = skip) %>% 
                             filter(!grepl("IGNORADO", Município)) %>%
                             slice_head(n = nrow(.) - 10) %>% 
                             separate_wider_regex(Município, c(codigo = ".*?", " ", municipio = ".*")) %>% 
                             pivot_longer(cols = 3:15, names_to = "ano", values_to = variavel) %>%
                             mutate("{variavel}" := as.numeric(ifelse(!!sym(variavel) == "-", 0, !!sym(variavel)))) %>% 
                             select(-Total, code_muni = codigo) 
                           
                         }) %>% 
      reduce(left_join, by = c("code_muni", "municipio", "ano")) %>% 
      left_join(population, by = c("code_muni", "municipio", "ano")) %>% 
      mutate(tx_hom_tot = (hom_tot / pop) * 100000),
    
    homicides_av = left_join(read_csv2("./1. Dados Municipais/Homicídios - Atlas da Violência (1989-2022).csv") %>%
                                distinct(),
                              
                              read_csv2("./1. Dados Municipais/Taxa de Homicídios 100mil hab - Atlas da Violência (1989-2022).csv") %>%
                                distinct() %>%
                                mutate(valor = as.numeric(valor)),
                              
                              by = c("cod", "nome", "período"), suffix = c("_bruto", "_taxa"))
    
    ) %>%
    attach()
  
  
  # ---- Base Final: Identificando Municípios com TI, Ouro e Garimpo ----
  
  st_crs(municipalities$geom) == st_crs(indigenous_lands$geometry) # 'make sure your coordinate systems projections are equal'
  sf_use_s2(FALSE) # para corrigir Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : Loop 0 is not valid: Edge 556 is degenerate (duplicate vertex)
  
  # Com os .shp em mãos, parte-se para decidir como identificar quais 
  # municípios possuem TI, reservas de ouro e/ou garimpo 

  # >> Em ambos o casos, a Abordagem 2 foi utilizada <<

  # (i) Municípios com Terra Indígena (FUNAI)
  # Abordagem 1: Cálculo do % de cada município ocupada por TIs (utilizando st_intersection e st_area)
  # Problema: st_intersection não tem precisão suficiente para excluir os que apenas fazem limite
  # Abordagem 2: Identificação a partir do tratamento da coluna de municípcios dada pela FUNAI 
  
  munincipalities_w_il <- indigenous_lands %>% 
                           st_drop_geometry() %>% 
                           select(gid, municipio_, uf_sigla) %>%  
                           separate_longer_delim(municipio_, delim = ",") %>% 
                           separate_longer_delim(uf_sigla, delim = ",") %>%
                           mutate(name_muni = str_c(municipio_, "/", uf_sigla), .keep = "unused", .after = gid) %>%
                           mutate(across(everything(), str_trim),
                           gid = as.integer(gid)) %>% 
                           left_join(municipalities %>% st_drop_geometry() %>% select(name_muni, code_muni), by = "name_muni") %>% 
                           select(-name_muni) %>% 
                           filter(!is.na(code_muni)) %>% 
                           pull(code_muni) %>% 
                           unique()
    
  
  # (ii) Municípios com 'Reservas de Ouro' (SGB)
  # Abordagem 1: Cálculo do % de cada município ocupada por TIs (utilizando st_intersection e st_area)
  # Abordagem 2: Verificação dos municípios com RO (utilizando st_intersects)
    
  municipalities_w_gr <- st_intersects(municipalities, 
                                       st_sfc((st_combine(province$geometry))[[1]], (st_combine(district$geometry))[[1]]) %>% 
                          st_combine %>% 
                          st_union(by_feature = T) %>% 
                          st_set_crs(st_crs(municipalities)), sparse = F)


  # (iii) Municípios da Amazônia Legal
  
  municipalities_la <- st_intersects(municipalities, geobr::read_amazon(), sparse = F)

  # Criando dataset final

  dataset = municipalities %>% 
    st_drop_geometry() %>%
    # ti = 1 se município possui terra indígena
    mutate(ti = ifelse(code_muni %in% munincipalities_w_il, 1, 0)) %>%
    # res_ou = 1 se o município possui provincía ou distrito aurífero
    bind_cols(municipalities_w_gr %>%
              as_tibble(.name_repair = "minimal") %>% 
              rename(intersects = 1) %>%
              mutate(res_ou = ifelse(intersects == TRUE, 1, 0), .keep = "unused")) %>%
    # la = 1 se o município possuir território dentro da Amazônia Legal
    bind_cols(municipalities_la %>%
              as_tibble(.name_repair = "minimal") %>% 
              rename(intersects = 1) %>%
              mutate(la = ifelse(intersects == TRUE, 1, 0), .keep = "unused")) %>%
    # Juntando via left_join para adicionar biomas
    left_join(biomes %>% mutate(code_muni = substr(as.character(geocodigo), 1, 6)) %>% select(code_muni, bioma), by = "code_muni") %>% 
    # Juntando via right_join para manter apenas municípios que possuem dados de homicídios (DataSUS) 
    right_join(homicides_ds %>% select(-municipio), by = "code_muni") %>% 
    # Criando dummy para o tratamento e transformando `ano` em factor
    mutate(pos2016 = ifelse(ano >= 2016, 1, 0),
           pos2017 = ifelse(ano >= 2017, 1, 0),
           ano = factor(ano)) %>% 
    relocate(ano, .before = code_muni)
  
  # Identificando limítrofes
  dataset <- dataset %>% 
    left_join(
        dataset %>% 
          filter(ti == 1, res_ou == 1) %>% 
          select(code_muni) %>% unique() %>% 
          inner_join(neighboring_municipalities, by = "code_muni") %>% 
          select(cd_lim, lim) %>% 
          rename(code_muni = cd_lim) %>% 
          mutate(code_muni = as.character(substr(code_muni, 1, 6))) %>% 
          unique(),
        by = "code_muni"
    ) %>% 
    mutate(lim = ifelse((ti == 1 & res_ou == 1) | is.na(lim), 0, lim))
 
        
  aux <- list(municipalities, neighboring_municipalities, indigenous_lands, gold_reserves, homicides,
              population, munincipalities_w_il, municipalities_w_gr, municipalities_la, biomes)
  rm(municipalities, neighboring_municipalities, indigenous_lands, gold_reserves, homicides,
     population, munincipalities_w_il, municipalities_w_gr, municipalities_la, biomes)
  
  # Salvando base final
  save(dataset, file = "dataset.RData")


  # ---- Workbook of Models ----

  ## [def] Exercício default (Grupo de Controle: todos os municípios que não possuírem (ti == 1 & res_ou == 1)
  ## [def_1022] Exercício default exc. 2010 e 2022 (anos de Censo) 
  ## [cg2] Grupo de Controle: municípios limítrofes e que não possuem (ti == 1 & res_ou == 1) 
  ## [cg3] Grupo de Controle: apenas municípios com terra indígena
  ## [cg4] Grupo de Controle: apenas municípios com reserva de ouro
  ## [tr2] Grupo de Tratamento: Amazônia Legal (la == 1) -> tx_hom_tot ~ la + pos2018 + la:pos2018
  ## [tr3] Grupo de Tratamento: Amazônia Legal e Reserva de Ouro (la == 1 & res_ou == 1) -> tx_hom_tot ~ res_ou:pos2018 + la:pos2018 + res_ou:la:pos2018
  ## [la1] Exercício default restringido à Amazônia Legal

  # Creating list
  workbook <- list(dataset = list(), plot_trend = list(), plot_ratio = list(),
                  event_study_rse = list(), plot_event_study = list(),
                  model = list())

  # Running models
  tribble(
    ~model_name,     ~sample_filter_expr,                                       ~treatment_group_condition_epxr, ~years,                  ~model_expr,
    # 2016
    "2016_def",       NA,                                                 "res_ou == 1 & ti == 1",   c(2010:2014, 2016:2022), "tx_hom_tot ~ res_ou:pos2016 + ti:pos2016 + res_ou:ti:pos2016", 
    "2016_def_1022",  "filter(., ano %in% c(2011:2021))",                 "res_ou == 1 & ti == 1",   c(2011:2014, 2016:2021), "tx_hom_tot ~ res_ou:pos2016 + ti:pos2016 + res_ou:ti:pos2016",
    "2016_cg2",       "filter(., (res_ou == 1 & ti == 1) | lim == 1)",    "res_ou == 1 & ti == 1",   c(2010:2014, 2016:2022), "tx_hom_tot ~ res_ou:pos2016 + ti:pos2016 + res_ou:ti:pos2016",
    "2016_cg3",       "filter(., (res_ou == 1 & ti == 1) | ti == 1)",     "res_ou == 1 & ti == 1",   c(2010:2014, 2016:2022), "tx_hom_tot ~ res_ou + pos2016 + res_ou:pos2016",
    "2016_cg4",       "filter(., (res_ou == 1 & ti == 1) | res_ou == 1)", "res_ou == 1 & ti == 1",   c(2010:2014, 2016:2022), "tx_hom_tot ~ ti + pos2016 + ti:pos2016",
    "2016_tr2",       NA,                                                 "la == 1",                 c(2010:2014, 2016:2022), "tx_hom_tot ~ la + pos2016 + la:pos2016",
    "2016_tr3",       NA,                                                 "res_ou == 1 & la == 1",   c(2010:2014, 2016:2022), "tx_hom_tot ~ res_ou:pos2016 + la:pos2016 + res_ou:la:pos2016",
    "2016_la1",       "filter(., la == 1)",                               "res_ou == 1 & ti == 1",   c(2010:2014, 2016:2022), "tx_hom_tot ~ res_ou:pos2016 + ti:pos2016 + res_ou:ti:pos2016",
    "2016_bio",       "filter(., bioma %in% c('Amazônia', 'Cerrado'))",   "res_ou == 1 & ti == 1",   c(2010:2014, 2016:2022), "tx_hom_tot ~ res_ou:pos2016 + ti:pos2016 + res_ou:ti:pos2016", 
    
    # 2018
    "2017_def",       NA,                                                 "res_ou == 1 & ti == 1",   c(2010:2015, 2017:2022), "tx_hom_tot ~ res_ou:pos2017 + ti:pos2017 + res_ou:ti:pos2017", 
    "2017_def_1022",  "filter(., ano %in% c(2011:2021))",                 "res_ou == 1 & ti == 1",   c(2011:2015, 2017:2021), "tx_hom_tot ~ res_ou:pos2017 + ti:pos2017 + res_ou:ti:pos2017",
    "2017_cg2",       "filter(., (res_ou == 1 & ti == 1) | lim == 1)",    "res_ou == 1 & ti == 1",   c(2010:2015, 2017:2022), "tx_hom_tot ~ res_ou:pos2017 + ti:pos2017 + res_ou:ti:pos2017",
    "2017_cg3",       "filter(., (res_ou == 1 & ti == 1) | ti == 1)",     "res_ou == 1 & ti == 1",   c(2010:2015, 2017:2022), "tx_hom_tot ~ res_ou + pos2017 + res_ou:pos2017",
    "2017_cg4",       "filter(., (res_ou == 1 & ti == 1) | res_ou == 1)", "res_ou == 1 & ti == 1",   c(2010:2015, 2017:2022), "tx_hom_tot ~ ti + pos2017 + ti:pos2017",
    "2017_tr2",       NA,                                                 "la == 1",                 c(2010:2015, 2017:2022), "tx_hom_tot ~ la + pos2017 + la:pos2017",
    "2017_tr3",       NA,                                                 "res_ou == 1 & la == 1",   c(2010:2015, 2017:2022), "tx_hom_tot ~ res_ou:pos2017 + la:pos2017 + res_ou:la:pos2017",
    "2017_la1",       "filter(., la == 1)",                               "res_ou == 1 & ti == 1",   c(2010:2015, 2017:2022), "tx_hom_tot ~ res_ou:pos2017 + ti:pos2017 + res_ou:ti:pos2017",
    "2017_bio",       "filter(., bioma %in% c('Amazônia', 'Cerrado'))",   "res_ou == 1 & ti == 1",   c(2010:2015, 2017:2022), "tx_hom_tot ~ res_ou:pos2017 + ti:pos2017 + res_ou:ti:pos2017",
  ) %T>% 
  { dict <<- . } %>% 
  mutate(idx = row_number(), .before = model_name) %>%
  pwalk(.f = function(idx, model_name, sample_filter_expr, treatment_group_condition_epxr, years, model_expr) {
    deploy_models(idx, model_name, sample_filter_expr, treatment_group_condition_epxr, years, model_expr)
  }) 

  # Charts
  make_plot_grid("plot_trend")
  make_plot_grid("plot_ratio")
  make_plot_grid("plot_event_study")
  
  workbook$model$`2016_bio`
  plot_event_study(workbook$event_study_rse$`2016_bio`, as.numeric(gsub("treatment_unit_", "", term))[as.numeric(gsub("treatment_unit_", "", term)) != 2015], estimate, 2015)

  workbook$model$`2017_bio`
  plot_event_study(workbook$event_study_rse$`2017_bio`, as.numeric(gsub("treatment_unit_", "", term))[as.numeric(gsub("treatment_unit_", "", term)) != 2016], estimate, 2016)
  
  