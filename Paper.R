  
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
                   readxl, openxlsx, sf, geobr, janitor, broom, biscale, ggtext, cowplot,
                   EnvStats, broom, estimatr, lmtest, plm, fixest, fastDummies, car)
  }
  
  # -------------------------------------------------------
  
  
  # O ideal é montar um base com:
  # . 5570 (municípios) * 13 (anos entre 2010-2022) observações
  
  
  # ---- Importando Dados ----
  
  ## Limites dos Municípios 
  # Fonte: {geobr} apud IBGE 
  
  municipios <- read_municipality(year = 2022) %>%
    select(code_muni, name_muni, abbrev_state, geometry = geom) %>% 
    mutate(code_muni = str_sub(as.character(code_muni), 1, 6),
           name_muni = gsub("Pontes E Lacerda", "Pontes e Lacerda", name_muni),
           name_muni = gsub("D'oeste", "D´Oeste", name_muni),
           name_muni = gsub("D'arco", "D´Arco", name_muni),
           name_muni = str_c(name_muni, "/", abbrev_state)) %>% # excluindo 7º (dígito verificador)
    filter(!(code_muni %in% "430000")) # https://github.com/ipeaGIT/geobr/issues/176
  
  
  ## Municípios Limítrofes
   # Fonte: IBGE
  
  municipios_lim <- read_xlsx("E:/4. Homícidios, Terras Indígenas e Reservas de Ouro/1. Dados Municipais/Municipios Limitrofes - IBGE (2024).xlsx", sheet = 1) %>% 
                      clean_names() %>% 
                      rename(code_muni = cd_mun) %>% 
                      mutate(code_muni = as.character(substr(code_muni, 1, 6)), lim = 1)
  
  
  ## Terras Indígenas 
  # Fonte: FUNAI 
  
  terras_indigenas <- read_sf("./2. Terras Indígenas - Shapefile/tis_poligonais_portariasPolygon.shp",
                            options = "ENCODING=WINDOWS-1252") %>% 
    mutate(across(where(is.character), str_trim),
           id_ti = row_number(), .before = gid) %>% 
    mutate(municipio_ = gsub("Poxoréo", "Poxoréu", municipio_),
           municipio_ = case_when(municipio_ == "Muquém de São Francisco"   ~ "Muquém do São Francisco",
                                  municipio_ == "Santo Antônio do Leverger" ~ "Santo Antônio de Leverger",
                                  T ~ as.character(municipio_))) 
  
  
  ## Províncias e Distritos Auríferos 
  # Fonte: SGB
  
  reservas_ouro <- list(provincia = read_sf("./3. Províncias e Distritos Auríferos do Brasil - Shapefile/Distritos_Provincias/prov_dist_au_provincias_br.shp"),
                        distrito  = read_sf("./3. Províncias e Distritos Auríferos do Brasil - Shapefile/Distritos_Provincias/prov_dist_au_distritos_br.shp"))
  attach(reservas_ouro)
  
  ## Homicídios (totais e indígenas) por Município 
  # Fonte: SIM/MS apud DataSUS/Tabnet/MS e Atlas da Violência 2023 
  
  homicidios = list(
    
    homicidios_ds = pmap(list(caminho = c("./1. Dados Municipais/Óbitos totais por município - Tabnet DataSUS (2010-2022).xlsx", "./1. Dados Municipais/Óbitos de indígenas por município - Tabnet DataSUS (2010-2022).xlsx"),
                              skip = c(4, 5),
                              variavel = c("hom_tot", "hom_ind")),
                         
                         \(caminho, skip, variavel) {
                           
                           read_xlsx(caminho, skip = skip) %>% 
                             filter(!grepl("IGNORADO", Município)) %>%
                             .[1:(nrow(.)-10),] %>% 
                             separate_wider_regex(Município, c(codigo = ".*?", " ", municipio = ".*")) %>% 
                             pivot_longer(cols = 3:15, names_to = "ano", values_to = variavel) %>%
                             mutate("{variavel}" := as.numeric(ifelse(!!sym(variavel) == "-", NA, !!sym(variavel)))) %>% 
                             select(-Total, code_muni = codigo) 
                           
                         }) %>% 
      reduce(left_join, by = c("code_muni", "municipio", "ano")) %>% 
      left_join(read_xlsx("./1. Dados Municipais/Estimativa de População TCU (2010-2021).xlsx", skip = 3) %>%
                  filter(!grepl("IGNORADO", Município)) %>% 
                  .[1:(nrow(.)-12),] %>% 
                  filter(`2010` != "-" & `2011` != "-", .by = Município) %>%   
                  mutate(`2010` = as.numeric(`2010`), `2011` = as.numeric(`2011`)) %>% 
                  pivot_longer(2:length(.), names_to = "ano", values_to = "pop") %>% 
                  clean_names() %>% 
                  mutate(pop_pct_change = pop/dplyr::lag(pop), 
                         pop_pct_change_geomean = geoMean(pop_pct_change, na.rm = T), .by = municipio) %>% 
                  complete(municipio, ano = "2022") %>%  
                  arrange(municipio, ano) %>%
                  group_by(municipio) %>% 
                  fill(everything(), .direction = "downup") %>% 
                  mutate(pop = ifelse(ano == "2022", round(pop * pop_pct_change_geomean, 0), pop)) %>% 
                  select(-pop_pct_change, -pop_pct_change_geomean) %>% 
                  separate_wider_regex(municipio, c(code_muni = ".*?", " ", municipio = ".*")),
                by = c("code_muni", "municipio", "ano")) %>% 
      mutate(tx_hom_tot = (hom_tot / pop) * 100000),
    
    homicidios_av = left_join(read_csv2("./1. Dados Municipais/Homicídios - Atlas da Violência (1989-2022).csv") %>%
                                distinct(),
                              
                              read_csv2("./1. Dados Municipais/Taxa de Homicídios 100mil hab - Atlas da Violência (1989-2022).csv") %>%
                                distinct() %>%
                                mutate(valor = as.numeric(valor)),
                              
                              by = c("cod", "nome", "período"), suffix = c("_bruto", "_taxa"))
    
  )
  attach(homicidios)
  
  
  
  
  
  
  # ---- Base Final: Identificando Municípios com TI, Ouro e Garimpo ----
  
  st_crs(municipios$geom) == st_crs(terras_indigenas$geometry) # 'make sure your coordinate systems projections are equal'
  sf_use_s2(FALSE) # para corrigir Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : Loop 0 is not valid: Edge 556 is degenerate (duplicate vertex)
  
  
  # (i) Municípios com Terra Indígena (FUNAI)
  # Abordagem 1: Cálculo do % de cada município ocupada por TIs (utilizando st_intersection e st_area)
  # Problema: st_intersection não tem precisão suficiente para excluir os que apenas fazem limite
  # Abordagem 2: Identificação a partir do tratamento da coluna de municípcios dada pela FUNAI 
  
  
  # (ii) Municípios com 'Reservas de Ouro' (SGB)
  # Abordagem 1: Cálculo do % de cada município ocupada por TIs (utilizando st_intersection e st_area)
  # Abordagem 2: Verificação dos municípios com RO (utilizando st_intersects)
  
  
  # >> Em ambos o casos, a Abordagem 2 foi utilizada <<
  
  
  # (iii) Municípios com Atividade de Garimpo (MapBiomas)
  # Planilha disponibilizada
  
  
  base_final = municipios %>%
    
    mutate(ti = ifelse(code_muni %in% (terras_indigenas %>% 
                                         st_drop_geometry() %>% 
                                         select(gid, municipio_, uf_sigla) %>%  
                                         separate_longer_delim(municipio_, delim = ",") %>% 
                                         separate_longer_delim(uf_sigla, delim = ",") %>%
                                         mutate(name_muni = str_c(municipio_, "/", uf_sigla), .keep = "unused", .after = gid) %>%
                                         mutate(across(everything(), str_trim),
                                                gid = as.integer(gid)) %>% 
                                         left_join(municipios %>% st_drop_geometry() %>% select(name_muni, code_muni), by = "name_muni") %>% 
                                         select(-name_muni) %>% 
                                         filter(!is.na(code_muni)) %>% 
                                         pull(code_muni) %>% 
                                         unique), 1, 0)) %>%
    
    bind_cols(st_intersects(municipios, st_sfc((st_combine(provincia$geometry))[[1]], (st_combine(distrito$geometry))[[1]]) %>% 
                              st_combine %>% 
                              st_union(by_feature = T) %>% 
                              st_set_crs(st_crs(municipios)), sparse = F) %>%
                as_tibble %>% 
                mutate(res_ou = ifelse(V1 == TRUE, 1, 0), .keep = "unused")) %>% 
    
    st_drop_geometry() %>% 
    left_join(read.xlsx('./1. Dados Municipais/Tabela Mineração - MapBiomas COL8.0.xlsx', sheet = 'CITY_STATE_BIOME') %>% 
                filter(level_3 == '1.1.01 Ouro' & !is.na(`2018`)) %>%               
                select(biome, city, state, code_muni = GEOCODE, `2018`) %>%
                mutate(code_muni = str_trim(as.character(str_sub(as.character(code_muni), 1, 6)))) %>% 
                summarise(ga2018 = sum(`2018`)/sum(`2018`), .by = code_muni), 
              by = "code_muni") %>% 
    mutate(ga2018 = replace_na(ga2018, 0)) %>%
    
    right_join(homicidios_ds %>% select(-municipio), by = "code_muni") %>% 
    
    mutate(pos2018 = ifelse(ano > 2018, 1, 0),
           ano = factor(ano),
           .by = code_muni, .after = ga2018) %>% 
    relocate(ano, .before = code_muni)
  
  base_final = base_final %>% 
    left_join(
        base_final %>% 
          filter(ti == 1, res_ou == 1) %>% 
          select(code_muni) %>% unique() %>% 
          inner_join(municipios_lim, by = "code_muni") %>% 
          select(cd_lim, lim) %>% 
          rename(code_muni = cd_lim) %>% 
          mutate(code_muni = as.character(substr(code_muni, 1, 6))) %>% 
          unique(),
        by = "code_muni"
    ) %>% 
    mutate(lim = ifelse((ti == 1 & res_ou == 1) | is.na(lim), 0, lim))
  
        
  rm(homicidios, municipios, municipios_lim, terras_indigenas, reservas_ouro)
  

  # ---- Regressões ----
  
  ## Teste para Tendências Paralelas
  
  # Step 1: Create a treatment indicator that is 1 for treated units in all periods
  base_final <- base_final %>%
    group_by(code_muni) %>%
    mutate(res_ou_dummy = ifelse(any(res_ou == 1), 1, 0)) %>%
    ungroup()
  
  # Step 2: Create time period dummies interacted with the Rebate_dummy
  base_final =
    base_final %>%
    # filter(ti == 1) %>% 
    mutate(
      res_ou_time2010 = ifelse(ano == 2010, res_ou_dummy, 0),
      res_ou_time2011 = ifelse(ano == 2011, res_ou_dummy, 0),
      res_ou_time2012 = ifelse(ano == 2012, res_ou_dummy, 0),
      res_ou_time2013 = ifelse(ano == 2013, res_ou_dummy, 0),
      res_ou_time2014 = ifelse(ano == 2014, res_ou_dummy, 0),
      res_ou_time2015 = ifelse(ano == 2015, res_ou_dummy, 0),
      res_ou_time2016 = ifelse(ano == 2016, res_ou_dummy, 0),
      res_ou_time2017 = ifelse(ano == 2017, res_ou_dummy, 0),
      # Exclude res_ou_time2018 to use it as the reference ano
      res_ou_time2019 = ifelse(ano == 2019, res_ou_dummy, 0),  # Treatment ano
      res_ou_time2020 = ifelse(ano == 2020, res_ou_dummy, 0),
      res_ou_time2021 = ifelse(ano == 2021, res_ou_dummy, 0),
      res_ou_time2022 = ifelse(ano == 2022, res_ou_dummy, 0)   # Post-treatment anos
    )
  
  event_study_model <- plm(
    tx_hom_tot ~ res_ou_time2010 + res_ou_time2011 + res_ou_time2012 +
      res_ou_time2013 + res_ou_time2014 + res_ou_time2015 + res_ou_time2016 +
      res_ou_time2017 + res_ou_time2019 + res_ou_time2020 + res_ou_time2021 +
      res_ou_time2022,
    data = pdata.frame(base_final, index = c("name_muni", "ano")), 
    model = "within", effect = "individual"
  )
  
  # Step 4: Obtain cluster-robust standard errors
  event_study_robust_se <- coeftest(event_study_model, vcov = vcovHC(event_study_model, type = "HC1", cluster = "group"))
  event_study_robust_se
  
  # Step 5: Extract coefficients for plotting
  event_study_robust_se_coef <- tidy(event_study_robust_se) %>%
                                filter(grepl("res_ou_time", term))
  View(event_study_robust_se_coef)
  
  # Gráfico
  ggplot(event_study_robust_se_coef, aes(x = as.numeric(gsub("res_ou_time", "", term)), y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error)) +
    labs(x = "Time Period Relative to Treatment", y = "Coefficient on res_ou",
         title = "Event Study for Parallel Trends Assumption") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    theme_minimal()
  
  
  ## Model
  
  # plm package
  model_plm <- plm(
    tx_hom_tot ~ res_ou:pos2018 + ti:pos2018 + res_ou:ti:pos2018,
    data = pdata.frame(base_final, index = c("name_muni", "ano")), 
    model = "within", effect = "twoways"
  )
  model_plm
  coeftest(model_plm, vcov = vcovHC(model_plm, type = "HC1", cluster = "group"))
  
  # feols package (same result)
  model_feols <- feols(tx_hom_tot ~ res_ou:pos2018 + ti:pos2018 + res_ou:ti:pos2018 | ano + code_muni, 
                 data = base_final)
  summary(model_feols, vcov = ~code_muni)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #STEP 1:
  base_final_new <- base_final %>%
    filter(res_ou == 1 | ti == 1 | lim == 1) %>% # REMOVER DEPOIS PARA VOLTAR AO ORIGINAL
    mutate(
      treated = as.integer(res_ou == 1 & ti == 1)
    )
  
  #STEP 2:
  base_final_new <- base_final_new %>%
    mutate(
      rel_year = as.numeric(ano) - 9  # 0 is the year before treatment
    )
  
  # Step 3:
  # Create clean dummy names like event_relm2, event_rel0, event_rel3
    for (k in -8:4) {
      varname <- paste0("event_rel", ifelse(k < 0, "m", ""), abs(k))
      base_final_new[[varname]] <- ifelse(base_final_new$rel_year == k & base_final_new$treated == 1, 1, 0)
    }

  
  #STEP 4:
  event_study_model_new <- plm(
    tx_hom_tot ~ 
      event_relm8 + event_relm7 + event_relm6 + event_relm5 + event_relm4 + event_relm3 + event_relm2 + event_relm1 +
      event_rel1 + event_rel2 + event_rel3 + event_rel4,
    data = pdata.frame(base_final_new, index = c("name_muni", "ano")),
    model = "within", effect = "twoways"
  )
  
  #STEP 5:
  event_study_robust_se_new <- coeftest(event_study_model_new, vcov = function(x) vcovHC(x, type = "HC1", cluster = "group"))
  event_study_robust_se_new
  
  #STEP 6:
  event_study_df <- broom::tidy(event_study_robust_se_new) %>%
    filter(grepl("event_", term)) %>%
    mutate(
      rel_year = case_when(
        str_detect(term, "relm") ~ paste0("-", str_extract(term, "\\d+")),
        str_detect(term, "rel")  ~ str_extract(term, "\\d+"),
        TRUE ~ NA_character_
      )
    )
    
  
  ggplot(event_study_df, aes(x = as.numeric(rel_year), y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                      ymax = estimate + 1.96 * std.error)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Years relative to treatment (2018 = 0)",
         y = "Effect on homicide rate",
         title = "Event Study: Homicides in Treated Municipalities") +
    theme_minimal()

  

  ## Gráfico de Tendências
  
  base_final_new %>% 
  group_by(ano, treated) %>% 
  summarise(hom_tot = sum(hom_tot, na.rm = T), pop = sum(pop, na.rm = T)) %>% 
  mutate(tx_hom_tot = hom_tot / pop * 100000) %>% 
  
  {ggplot(., aes(x = ano, y = tx_hom_tot, color = factor(treated))) +
    geom_vline(xintercept = '2018', linetype = "dashed", color = "black") +
    geom_line(size = 0.8, aes(group = treated)) +
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
  
  
  base_final_new %>% 
    group_by(ano, treated) %>% 
    summarise(hom_tot = sum(hom_tot, na.rm = T), pop = sum(pop, na.rm = T)) %>% 
    mutate(tx_hom_tot = hom_tot / pop * 100000) %>% 
    group_by(ano) %>%
    select(-hom_tot, -pop) %>% 
    pivot_wider(
      names_from = treated,
      values_from = tx_hom_tot,
      names_prefix = "treated_"
    ) %>% 
    mutate(
      razao_tx_hom_tot = treated_1 / treated_0
    ) %>% 
    
    {ggplot(., aes(x = ano, y = razao_tx_hom_tot)) +
        geom_vline(xintercept = '2018', linetype = "dashed", color = "black") +
        geom_line(size = 0.8, group = 1) +
        geom_point(size = 2) +
        labs(
          title = "Razão da Taxa de Homicídios ao longo do tempo",
          x = "Ano",
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
  