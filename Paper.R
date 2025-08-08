  
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
  
  # ---- Código para os Gráficos ----
  
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

  plot_event_study <- function(data, x_column, y_column) {
    ggplot(data, aes(x = as.numeric({{x_column}}), y = {{y_column}})) +
    geom_point() +
    geom_errorbar(aes(ymin = {{y_column}} - 1.96 * std.error,
                      ymax = {{y_column}} + 1.96 * std.error)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Years relative to treatment (2018 = 0)",
         y = "Effect on homicide rate",
         title = "Event Study: Homicides in Treated Municipalities") +
    theme_minimal()
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
    # Juntando via right_join para manter apenas municípios que possuem dados de homicídios (DataSUS) 
    right_join(homicides_ds %>% select(-municipio), by = "code_muni") %>% 
    # Criando dummy para o tratamento e transformando `ano` em factor
    mutate(pos2018 = ifelse(ano > 2018, 1, 0),
           ano = factor(ano)) %>% 
    relocate(ano, .before = code_muni)
  
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
  
        
  rm(municipalities, neighboring_municipalities, indigenous_lands, gold_reserves, homicides,
     population, munincipalities_w_il, municipalities_w_gr)
  


  # ---- Regressões ----
  
  # Criando dummies de Tratamento x Ano para o Teste de Tendências Paralelas
  dataset <- dataset %>%
    # Step 1: Create a treatment indicator that is 1 for treated units in all periods  
    mutate(treatment_unit = ifelse(any(ti == 1 & res_ou == 1), 1, 0), .by = code_muni) %>% 
    # Step 2: Create time period dummies interacted with the Rebate_dummy
    mutate(
      treatment_unit_2010 = ifelse(ano == 2010, treatment_unit, 0),
      treatment_unit_2011 = ifelse(ano == 2011, treatment_unit, 0),
      treatment_unit_2012 = ifelse(ano == 2012, treatment_unit, 0),
      treatment_unit_2013 = ifelse(ano == 2013, treatment_unit, 0),
      treatment_unit_2014 = ifelse(ano == 2014, treatment_unit, 0),
      treatment_unit_2015 = ifelse(ano == 2015, treatment_unit, 0),
      treatment_unit_2016 = ifelse(ano == 2016, treatment_unit, 0),
      treatment_unit_2017 = ifelse(ano == 2017, treatment_unit, 0),
      # Exclude treatment_unit_2018 to use it as the year reference
      treatment_unit_2019 = ifelse(ano == 2019, treatment_unit, 0),  # 1st year of treatment
      treatment_unit_2020 = ifelse(ano == 2020, treatment_unit, 0),
      treatment_unit_2021 = ifelse(ano == 2021, treatment_unit, 0),
      treatment_unit_2022 = ifelse(ano == 2022, treatment_unit, 0)   # Post-treatment years
    )


  ### i) Grupo de Controle: todos os municípios que não possuírem (ti == 1 & res_ou == 1) 

  plot_trend(dataset)
  plot_ratio(dataset, ano, razao_tx_hom_tot)

  ## Teste para Tendências Paralelas  
  event_study_model <- plm(
    tx_hom_tot ~ treatment_unit_2010 + treatment_unit_2011 + treatment_unit_2012 +
      treatment_unit_2013 + treatment_unit_2014 + treatment_unit_2015 + treatment_unit_2016 +
      treatment_unit_2017 + treatment_unit_2019 + treatment_unit_2020 + treatment_unit_2021 +
      treatment_unit_2022,
    data = pdata.frame(dataset, index = c("name_muni", "ano")), 
    model = "within", effect = "twoway"
  )
  
  # Step 4: Obtain cluster-robust standard errors
  event_study_robust_se <- coeftest(event_study_model, 
                                    vcov = vcovHC(event_study_model, type = "HC1", cluster = "group")) %>% 
                            # Step 5: Extract coefficients for plotting
                            tidy() %>%
                            filter(grepl("treatment_unit_", term))
  event_study_robust_se
  
  # Gráfico
  plot_event_study(event_study_robust_se, as.numeric(gsub("treatment_unit_", "", term)) - 2018, estimate)
  
  ## Model
  
  # plm package
  model_plm <- plm(
    tx_hom_tot ~ res_ou:pos2018 + ti:pos2018 + res_ou:ti:pos2018,
    data = pdata.frame(dataset, index = c("name_muni", "ano")), 
    model = "within", effect = "twoways"
  )
  model_plm
  coeftest(model_plm, vcov = vcovHC(model_plm, type = "HC1", cluster = "group"))
  
  # feols package (same result)
  model_feols <- feols(tx_hom_tot ~ res_ou:pos2018 + ti:pos2018 + res_ou:ti:pos2018 | ano + code_muni, 
                 data = base_final)
  summary(model_feols, vcov = ~code_muni)
  
  
  ### ii) Grupo de Controle: municípios limítrofes e que não possuem (ti == 1 & res_ou == 1) 
  
  dataset_cg2 <- dataset %>% 
    filter((res_ou == 1 & ti == 1) | lim == 1)
  
  plot_trend(dataset_cg2)
  plot_ratio(dataset_cg2, ano, razao_tx_hom_tot)

  ## Teste para Tendências Paralelas
  event_study_model_cg2 <- plm(
    tx_hom_tot ~ treatment_unit_2010 + treatment_unit_2011 + treatment_unit_2012 +
      treatment_unit_2013 + treatment_unit_2014 + treatment_unit_2015 + treatment_unit_2016 +
      treatment_unit_2017 + treatment_unit_2019 + treatment_unit_2020 + treatment_unit_2021 +
      treatment_unit_2022,
    data = pdata.frame(dataset_cg2, index = c("name_muni", "ano")), 
    model = "within", effect = "twoway"
  )
  
  # Step 4: Obtain cluster-robust standard errors
  event_study_robust_se_cg2 <- coeftest(event_study_model_cg2, 
                                    vcov = vcovHC(event_study_model_cg2, type = "HC1", cluster = "group")) %>% 
                            # Step 5: Extract coefficients for plotting
                            tidy() %>%
                            filter(grepl("treatment_unit_", term))
  event_study_robust_se_cg2
  
  # Gráfico
  plot_event_study(event_study_robust_se_cg2, as.numeric(gsub("treatment_unit_", "", term)) - 2018, estimate)
  
  ## Model
  model_plm_cg2 <- plm(
    tx_hom_tot ~ res_ou:pos2018 + ti:pos2018 + res_ou:ti:pos2018,
    data = pdata.frame(dataset_cg2, index = c("name_muni", "ano")), 
    model = "within", effect = "twoways"
  )
  model_plm_cg2
  coeftest(model_plm_cg2, vcov = vcovHC(model_plm_cg2, type = "HC1", cluster = "group"))
