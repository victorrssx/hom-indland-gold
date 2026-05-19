  
  ########################################################
  ################                        ################
  ################       IT, H and Au     ################
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
  
  # ---- Auxiliary Chart Functions ----
  
  ## Trend Chart
  plot_trend <- function(data, hom_var, tx_var, pt_subtitle = NULL, ref_year_num) {
    data %>%
      group_by(year, treatment_unit) %>%
      summarise(!!hom_var := sum(!!sym(hom_var), na.rm = T), pop = sum(pop, na.rm = T)) %>%
      mutate(!!tx_var := .data[[hom_var]] / pop * 100000) %>%
      
      {ggplot(., aes(x = year, y = .data[[tx_var]], color = factor(treatment_unit))) +
          geom_vline(xintercept = ref_year_num %>% as.character(), linetype = "dashed", color = "black") +
          geom_line(linewidth = 0.8, aes(group = treatment_unit)) +
          geom_point(size = 2) +
          labs(
            title = "Homicide Rate Ratio Throughout Time by Group", subtitle = pt_subtitle,
            x = "Year", y = "Homicide Rate (per 100k inhabitants)",
            color = "Group"
          ) +
          scale_color_manual(labels = c("Control", "Treatment"), values = c("steelblue", "tomato"))}
  }

  ## Homicide Ratio (Treatment/Control) Throughout Time
  plot_ratio <- function(data, x_column, y_column, hom_var, tx_var, pr_subtitle = NULL, ref_year_num) {
    data %>%
      group_by(year, treatment_unit) %>%
      summarise(!!hom_var := sum(!!sym(hom_var), na.rm = T), pop = sum(pop, na.rm = T)) %>%
      mutate(!!tx_var := .data[[hom_var]] / pop * 100000) %>%
      group_by(year) %>%
      select(-all_of(hom_var), -pop) %>%
      pivot_wider(
        names_from = treatment_unit,
        values_from = all_of(tx_var),
        names_prefix = "treated_"
      ) %>%
      mutate(tx_hom_ratio = treated_1 / treated_0) %>%
        
    {ggplot(., aes(x = {{x_column}}, y = {{y_column}})) +
    geom_vline(xintercept = ref_year_num %>% as.character(), linetype = "dashed", color = "black") +
    geom_line(size = 0.8, group = 1) +
    geom_point(size = 2) +
    labs(
      title = "Homicide Rate Ratio Throughout Time", subtitle = pr_subtitle,
      x = "Year", y = "Homicide Rate Ratio",
      color = "Group"
    ) +
    scale_color_manual(labels = c("Control", "Treatment"), values = c("steelblue", "tomato"))}
  }

  ## Event Study
  plot_event_study <- function(data, x_column, y_column, pes_title = NULL, pes_subtitle = NULL, ref_year_num) {
    ggplot(data, aes(x = {{x_column}}, y = {{y_column}})) +
    geom_hline(yintercept = 0, linetype = "solid", color = "red") +
    geom_point(size = 3.5) +
    geom_errorbar(aes(ymin = {{y_column}} - 1.96 * std.error,
                      ymax = {{y_column}} + 1.96 * std.error),
                  width = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
    scale_x_continuous(breaks = seq(-5, 7, by = 1)) +
    labs(x = str_c("Years to Treatment (", ref_year_num, " = 0)"), # Years relative to treatment
         y = "DDD coefficient (Gold x IT x Post)",
         title = pes_title,
         subtitle = pes_subtitle) +
    theme(axis.line.x = element_blank())
  }

  ## Plot Grid
  make_plot_grid <- function(plot_type, ncol = 4) {
  dict %>%
    pull(model_name) %>%
    as.character() %>%
    keep(~ .x %in% names(workbook[[plot_type]])) %>%
    map(~ workbook[[plot_type]][[.x]] +
                 ggtitle(.x) +
                 theme(
                   plot.title = element_text(hjust = 0.5, face = "bold"),
                   plot.margin = margin(6, 6, 6, 6)
                 )) %>%
    patchwork::wrap_plots(ncol = ncol) &
    theme(legend.position = "bottom")
  }


  # ---- Main Function to Deploy Models ----

  deploy_models <- function(idx, model_name, sample_filter_expr, treatment_condition_expr, years, model_expr) {
    
    # Creating auxiliary variables
    ref_year = setdiff(2010:2022, years)
    post_var = str_c("post", ref_year + 1)
    hom_var = str_extract(model_expr, "hom_\\w+")
    tx_var  = str_c("tx_", hom_var)

    # Creating auxiliary functions
    lin_combo <- function(model, terms) {
      b <- coef(model)
      V <- vcov(model, vcov = ~code_muni)
      common <- intersect(names(b), colnames(V))
      b <- b[common]
      V <- V[common, common, drop = FALSE]
      a <- rep(0, length(common))
      names(a) <- common
      present_terms <- intersect(terms, common)
      if (length(present_terms) == 0) {
          message("No linear-combination terms found: ", paste(terms, collapse = ", "))
          return(invisible(NULL))
      }
      a[present_terms] <- 1
      est <- sum(a * b)
      se <- sqrt(as.numeric(t(a) %*% V %*% a))
      z <- est / se
      p <- 2 * pnorm(abs(z), lower.tail = FALSE)
      c(estimate = est, se = se, p = p)
    }
    
    # Dataset adjust to modelling
    temp_dataset <- dataset %>%
      { if (is.na(sample_filter_expr)) . else eval_tidy(parse_expr(sample_filter_expr), data = .) } %>% # Additional filter
      # Step 1: Create a treatment indicator that is 1 for treated units in all periods
      mutate(treatment_unit = ifelse(any(!!parse_expr(treatment_condition_expr)), 1, 0), .by = code_muni) %>%
      # Step 2: Create time period dummies interacted with the Rebate_dummy
      mutate(!!!setNames(
        map(years, ~expr(ifelse(year == !!.x, treatment_unit, 0))),
        paste0("treatment_unit_", years)
      ))
      {
        workbook$dataset[[idx]] <<- temp_dataset
        names(workbook$dataset)[idx] <<- model_name
      }
      {
        workbook$plot_trend[[idx]] <<- plot_trend(temp_dataset, hom_var, tx_var, model_name, ref_year)
        names(workbook$plot_trend)[idx] <<- model_name
      }
      {
        workbook$plot_ratio[[idx]] <<- plot_ratio(temp_dataset, year, tx_hom_ratio, hom_var, tx_var, model_name, ref_year)
        names(workbook$plot_ratio)[idx] <<- model_name      
      }
    
    # Step 3: Creating Event Study & Obtain cluster-robust standard errors
    event_study_robust_se <- feols(
        as.formula(str_c(tx_var,  " ~ i(year, gold, ref = ref_year) + i(year, il, ref = ref_year) + i(year, gold * il, ref = ref_year) | code_muni + year")),
        data = temp_dataset,
        cluster = ~code_muni
      ) %>%
      # Step 5: Extract coefficients for plotting
      tidy() %>%
      filter(grepl("gold * il", term, fixed = TRUE))
      {
        workbook$event_study_rse[[idx]] <<- event_study_robust_se
        names(workbook$event_study_rse)[idx] <<- model_name
      }
      {
        workbook$plot_event_study[[idx]] <<- plot_event_study(event_study_robust_se, 
                                                              as.numeric(gsub(".*year::(\\d{4}).*", "\\1", term)) - ref_year, 
                                                              ref_year_num = ref_year,
                                                              estimate,
                                                              pes_title = str_c("Dynamic DDD in", "", sep = " "),
                                                              pes_subtitle = model_name)
        names(workbook$plot_event_study)[idx] <<- model_name
      }
    
    # Step 6: Model (feols package)
    model_feols <- feols(
      as.formula(str_c(model_expr, " | code_muni + year")),
      data = temp_dataset,
      cluster = ~code_muni
    )
      {
        workbook$model[[idx]] <<- model_feols
        names(workbook$model)[idx] <<- model_name
      }

    # Step 7: Rubstness Check: State-year FE
    model_feols_sy <- feols(
      as.formula(str_c(model_expr, " | code_muni + abbrev_state^year")),
      data = temp_dataset,
      cluster = ~code_muni
    )
      {
        workbook$model_sy[[idx]] <<- model_feols_sy
        names(workbook$model_sy)[idx] <<- model_name
      }    
    
    # Step 8: Rubstness Check: WLS
    model_wls <- feols(
        as.formula(str_c(model_expr, " | code_muni + year")),
        data = temp_dataset,
      weights = ~pop
    )
    model_wls_sy <- feols(
        as.formula(str_c(model_expr, " | code_muni + abbrev_state^year")),
        data = temp_dataset,
      weights = ~pop
    )
      {
        workbook$model_wls[[idx]] <<- model_wls
        names(workbook$model_wls)[idx] <<- model_name
        workbook$model_wls_sy[[idx]] <<- model_wls_sy
        names(workbook$model_wls_sy)[idx] <<- model_name
      } 
    
    # Step 8: Rubstness Check: DD slices (post treatment)
    if (n_distinct(temp_dataset$il) > 1 && n_distinct(temp_dataset$gold) > 1) {
    model_il1 <- feols(as.formula(str_c(tx_var, " ~ ", "gold:", post_var, " | code_muni + year")), data = temp_dataset %>% filter(il == 1))
    model_il0 <- feols(as.formula(str_c(tx_var, " ~ ", "gold:", post_var, " | code_muni + year")), data = temp_dataset %>% filter(il == 0))
    model_gold1 <- feols(as.formula(str_c(tx_var, " ~ ", "il:", post_var, " | code_muni + year")), data = temp_dataset %>% filter(gold == 1))
    model_gold0 <- feols(as.formula(str_c(tx_var, " ~ ", "il:", post_var, " | code_muni + year")), data = temp_dataset %>% filter(gold == 0))
    {
      workbook$model_il1[[idx]] <<- model_il1; names(workbook$model_il1)[idx] <<- model_name
      workbook$model_il0[[idx]] <<- model_il0; names(workbook$model_il0)[idx] <<- model_name
      workbook$model_gold1[[idx]] <<- model_gold1; names(workbook$model_gold1)[idx] <<- model_name
      workbook$model_gold0[[idx]] <<- model_gold0; names(workbook$model_gold0)[idx] <<- model_name
    }
   }

    # Step 9: Rubstness Check: Placebo treatment years in pre-period (<=2015) 
    model_fake_post2013 <- feols(
      as.formula(str_c(tx_var, "~ gold:fake_post2013 + il:fake_post2013 + gold:il:fake_post2013 | code_muni + abbrev_state^year")),
      data = temp_dataset %>% rename(fake_post2013 = post2013) %>% filter(as.numeric(as.character(year)) <= 2015)
    )
    model_fake_post2014 <- feols(
      as.formula(str_c(tx_var, "~ gold:fake_post2014 + il:fake_post2014 + gold:il:fake_post2014 | code_muni + abbrev_state^year")),
      data = temp_dataset %>% rename(fake_post2014 = post2014) %>% filter(as.numeric(as.character(year)) <= 2015)
    )
    {
      workbook$model_fake13[[idx]] <<- model_fake_post2013; names(workbook$model_fake13)[idx] <<- model_name
      workbook$model_fake14[[idx]] <<- model_fake_post2014; names(workbook$model_fake14)[idx] <<- model_name
    }
    
    # Step 10: Rubstness Check: Dual-break models to separate the 2013 deregulation channel from the post treatment shift.
    model_dual_break <- feols(
      as.formula(str_c(
        tx_var, " ~ gold:post2013 + il:post2013 + gold:il:post2013 + ", 
                "gold:", post_var, " + il:", post_var," + gold:il:", post_var, " | code_muni + year^abbrev_state")),
      data = temp_dataset
    )
    ddd_est = tribble(~early, ~inc, ~total, 
                      lin_combo(model_dual_break, c("gold:post2013:il", "gold:il:post2013")),
                      lin_combo(model_dual_break, c(str_c("gold:", post_var, ":il"), str_c("gold:il", post_var))),
                      lin_combo(model_dual_break, c("gold:post2013:il", str_c("gold:", post_var, ":il"), "gold:il:post2013", str_c("gold:il:", post_var)))
                    )
    
    model_dual_break_sy <- feols(
      as.formula(str_c(
        tx_var, " ~ gold:post2013 + il:post2013 + gold:il:post2013 + ", 
                "gold:", post_var, " + il:", post_var," + gold:il:", post_var, " | code_muni + year^abbrev_state")),
      data = temp_dataset
    )
    ddd_est_sy = tribble(~early, ~inc, ~total, 
                      lin_combo(model_dual_break_sy, c("gold:post2013:il", "gold:il:post2013")),
                      lin_combo(model_dual_break_sy, c(str_c("gold:", post_var, ":il"), str_c("gold:il:", post_var))),
                      lin_combo(model_dual_break_sy, c("gold:post2013:il", str_c("gold:", post_var, ":il"), "gold:il:post2013", str_c("gold:il:", post_var)))
                    )    
    {
      workbook$model_dual_break[[idx]] <<- model_dual_break; names(workbook$model_dual_break)[idx] <<- model_name
      workbook$model_dual_break_dddest[[idx]] <<- ddd_est; names(workbook$model_dual_break_dddest)[idx] <<- model_name
      workbook$model_dual_break_sy[[idx]] <<- model_dual_break_sy; names(workbook$model_dual_break_sy)[idx] <<- model_name
      workbook$model_dual_break_sy_dddest[[idx]] <<- ddd_est_sy; names(workbook$model_dual_break_sy_dddest)[idx] <<- model_name
    }

    # Step 11: Rubstness Check: Wald
    wald <- temp_dataset %>%  
            feols(as.formula(paste(tx_var, " ~ ", paste(paste0("treatment_unit_", years), collapse = " + "), "| code_muni + year")), 
                  data = .) %>% 
            wald("treatment_unit_201[0-4]")
    {
      workbook$wald[[idx]] <<- model_dual_break; names(workbook$wald)[idx] <<- model_name
    }

  }

  
  # ---- Importing Data ----
  # Ideal dataset:
  # 5570 (municipalities) * 13 (years between 2010-2022) = 72410 observations

  ## Municipal Boundaries 
  # Source: {geobr} apud IBGE 
  
  municipalities <- read_municipality(year = 2022) %>%
                    select(code_muni, name_muni, abbrev_state, geometry = geom) %>% 
                    mutate(code_muni = str_sub(as.character(code_muni), 1, 6), # excluding 7th (check digit)
                           name_muni = gsub("Pontes E Lacerda", "Pontes e Lacerda", name_muni),
                           name_muni = gsub("D'oeste", "D´Oeste", name_muni),
                           name_muni = gsub("D'arco", "D´Arco", name_muni),
                           name_muni = str_c(name_muni, "/", abbrev_state)) %>% 
                    filter(!(code_muni %in% "430000")) # https://github.com/ipeaGIT/geobr/issues/176
  
  
  ## Bordering Municipalities
  # Source: IBGE
  
  neighboring_municipalities <- read_xlsx("./1. Dados Municipais/Municipios Limitrofes - IBGE (2024).xlsx", sheet = 1) %>% 
                                clean_names() %>% 
                                rename(code_muni = cd_mun) %>% 
                                mutate(code_muni = as.character(substr(code_muni, 1, 6)), lim = 1)
  
  
  ## Municipalities - Predominant Biome
  ## Municipalities - Biome
  # Source: IBGE
  
  predominant_biome <- read_xlsx("./1. Dados Municipais/Bioma Predominante por Município - IBGE (2024).xlsx", sheet = 1, skip = 1) %>% 
                       clean_names() %>% 
                       rename(bioma = bioma_predominante)
  
  all_biomes <- read_xlsx("./1. Dados Municipais/Biomas por Município - IBGE (2024).xlsx", sheet = 1) %>% 
                clean_names() %>% 
                rename(bioma_all = bioma)
  
  
  ## Indigenous Territories/Lands 
  # Source: FUNAI 
  
  indigenous_lands <- read_sf("./2. Terras Indígenas - Shapefile/tis_poligonais_portariasPolygon.shp",
                              options = "ENCODING=WINDOWS-1252") %>% 
                      mutate(across(where(is.character), str_trim),
                             id_ti = row_number(), .before = gid) %>% 
                      mutate(municipio_ = gsub("Poxoréo", "Poxoréu", municipio_),
                             municipio_ = case_when(municipio_ == "Muquém de São Francisco"   ~ "Muquém do São Francisco",
                                                    municipio_ == "Santo Antônio do Leverger" ~ "Santo Antônio de Leverger",
                                                    T ~ as.character(municipio_))) 
  
  
  ## Gold Districts and Provinces
  # Source: SGB
  
  gold_reserves <- list(province = read_sf("./3. Províncias e Distritos Auríferos do Brasil - Shapefile/Distritos_Provincias/prov_dist_au_provincias_br.shp"),
                        district  = read_sf("./3. Províncias e Distritos Auríferos do Brasil - Shapefile/Distritos_Provincias/prov_dist_au_distritos_br.shp")) %>%
                   attach()
  

  ## Resident Population by Municipality
  # Source: DataSUS apud IBGE; Note: 2010 and 2022 representing Census numbers

  population <- read_xlsx("./1. Dados Municipais/Estimativa de População IBGE TCU (2010-2022).xlsx", skip = 3) %>% 
                  filter(!grepl("IGNORADO", Município)) %>% 
                  head(5570) %>% 
                  mutate(across(where(is.character) & -Município, ~ as.numeric(na_if(.x, "-")))) %>% 
                  pivot_longer(2:length(.), names_to = "year", values_to = "pop") %>% 
                  clean_names() %>%
                  separate_wider_regex(municipio, c(code_muni = ".*?", " ", municipio = ".*")) %>% 
                  select(-municipio)


  ## Homicides (total and indigenous) by Municipality 
  # Source: SIM/MS apud DataSUS/Tabnet/MS e Atlas da Violência 2023 
  
  homicides = list(
    
    homicides_ds =     pmap(list(sheet = seq(1, 13)),
                            
                            \(sheet) {
                              
                              variable = excel_sheets("./1. Dados Municipais/Óbitos totais por município - Tabnet DataSUS (2010-2024).xlsx")[sheet]
                              
                              read_xlsx("./1. Dados Municipais/Óbitos totais por município - Tabnet DataSUS (2010-2024).xlsx", sheet = sheet) %>% 
                                slice(which(`Mortalidade - Brasil` == "Município"):(which(`Mortalidade - Brasil` == "Total") - 1)) %>%
                                row_to_names(1) %>% select(-Total) %>%
                                separate_wider_regex(Município, c(code_muni = ".*?", " ", municipality = ".*")) %>% 
                                pivot_longer(cols = 3:length(.), names_to = "year", values_to = variable) %>%
                                mutate("{variable}" := as.numeric(ifelse(!!sym(variable) == "-", 0, !!sym(variable)))) %>% 
                                filter(!grepl("IGNORADO", municipality) & year %in% as.character(2010:2022)) %>%
                                select(-municipality)
                              
                            }) %>% 
      reduce(full_join, by = c("code_muni", "year")) %>% 
      right_join(population, by = c("code_muni", "year")) %>% 
      mutate(across(where(is.numeric), ~ replace_na(., 0)),
             across(starts_with("hom"), ~ (. / pop) * 100000, .names = "tx_{.col}")),
    
    homicides_av =   full_join(read_delim("./1. Dados Municipais/Homicídios - Atlas da Violência (1989-2022).csv", delim = ";") %>% 
                               filter(valor == max(valor), .by = c(cod, período)),
                               
                               read_delim("./1. Dados Municipais/Homicídios Estimados - Atlas da Violência (1996-2022).csv", delim = ";"),
                               
                               by = c("cod", "nome", "período"), suffix = c("_registrado", "_estimado")) %>%
                     filter(período %in% c(2010:2022)) %>%
                     mutate(cod = substr(as.character(cod), 1, 6), período = as.character(período)) %>% 
                     rename(code_muni = cod, municipio = nome, year = período, hom_tot_avre = valor_registrado, hom_tot_aves = valor_estimado) %>% 
                     select(-municipio) %>% 
                     right_join(population, by = c("code_muni", "year")) %>%
                     mutate(across(where(is.numeric), ~ replace_na(., 0)),
                            tx_hom_tot_avre = hom_tot_avre / pop * 100000,
                            tx_hom_tot_aves = hom_tot_aves / pop * 100000)
    
  ) %>%
  attach()
  
  
  # ---- Building Final Dataset: Identifying Municipalities with IL and Gold ----
  
  st_crs(municipalities$geom) == st_crs(indigenous_lands$geometry) # 'make sure your coordinate systems projections are equal'
  sf_use_s2(FALSE) # to correct 'Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : Loop 0 is not valid: Edge 556 is degenerate (duplicate vertex)'
  
  # With the .shp files in hand, the next step is to decide how to identify which municipalities have Indigenous Territories/Lands and gold reserves.

  # >> In both cases, Approach 2 was chosen <<

  # (i) Municipalities with IL (FUNAI)
  # Approach 1: Calculation of the percentage of each municipality occupied by Indigenous Territories (using st_intersection and st_area)
  # Problem: st_intersection does not have sufficient precision to exclude those that only form a boundary
  # Approach 2: Identification based on the treatment of the municipalities column provided by FUNAI 
  
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
    
  # (ii) Municipalities with 'Gold Reserves' (SGB)
  # Approach 1: Calculation of the % of each municipality occupied by TIs (using st_intersection and st_area)
  # Approach 2: Verification of municipalities with GR (using st_intersects)
    
  municipalities_w_gr <- st_intersects(municipalities, 
                                       st_sfc((st_combine(province$geometry))[[1]], (st_combine(district$geometry))[[1]]) %>% 
                          st_combine %>% 
                          st_union(by_feature = T) %>% 
                          st_set_crs(st_crs(municipalities)), sparse = F)


  # (iii) Legal Amazon Municipalities
  
  municipalities_la <- st_intersects(municipalities, geobr::read_amazon(), sparse = F)

  # Final Dataset

  dataset = municipalities %>% 
    st_drop_geometry() %>%
    # il = 1 if the municipality has indigenous territories/lands 
    mutate(il = ifelse(code_muni %in% munincipalities_w_il, 1, 0)) %>%
    # gold = 1 if the municipality has indigenous districts or provinces of gold
    bind_cols(municipalities_w_gr %>%
              as_tibble(.name_repair = "minimal") %>% 
              rename(intersects = 1) %>%
              mutate(gold = ifelse(intersects == TRUE, 1, 0), .keep = "unused")) %>%
    # la = 1 if the municipality overlaps part or all of its territory with Legal Amazon
    bind_cols(municipalities_la %>%
              as_tibble(.name_repair = "minimal") %>% 
              rename(intersects = 1) %>%
              mutate(la = ifelse(intersects == TRUE, 1, 0), .keep = "unused")) %>%
    # Joining via left_join to add biomes
    left_join(predominant_biome %>% 
              mutate(code_muni = substr(as.character(geocodigo), 1, 6)) %>% 
              select(code_muni, bioma), 
              by = "code_muni") %>%
    left_join(all_biomes %>% 
              filter(bioma_all %in% c('Amazônia', 'Cerrado')) %>% 
              distinct(cd_geocmu) %>% 
              mutate(code_muni = substr(as.character(cd_geocmu), 1, 6), 
                     amz_cer = T) %>% 
              select(code_muni, amz_cer),
              by = "code_muni") %>%
    # Joining via left_join to add homicide data (AV & DataSUS) 
    left_join(homicides_av, by = "code_muni", relationship = "many-to-many") %>%
    left_join(homicides_ds, by = c("code_muni", "year", "pop"), relationship = "many-to-many") %>%
    # Creating treatment dummy and transforming `year` in factor
    mutate(post2013 = ifelse(year >= 2013, 1, 0),
           post2014 = ifelse(year >= 2014, 1, 0),
           post2015 = ifelse(year >= 2015, 1, 0),
           post2016 = ifelse(year >= 2016, 1, 0),
           post2017 = ifelse(year >= 2017, 1, 0),
           post2018 = ifelse(year >= 2018, 1, 0),
           post2019 = ifelse(year >= 2019, 1, 0),
           year = factor(year),
           amz_cer = replace_na(amz_cer, F)) %>%
    relocate(year, .before = code_muni) %>% 
    relocate(c(pop, tx_hom_tot_avre, tx_hom_tot_aves), .before = tx_hom_tot_lo_x85_y35)

 
  #list2env(aux, envir = .GlobalEnv)
  aux <- list(
    municipalities = municipalities,
    neighboring_municipalities = neighboring_municipalities,
    indigenous_lands = indigenous_lands,
    gold_reserves = gold_reserves,
    homicides = homicides,
    population = population,
    munincipalities_w_il = munincipalities_w_il,
    municipalities_w_gr = municipalities_w_gr,
    municipalities_la = municipalities_la,
    predominant_biome = predominant_biome,
    all_biomes = all_biomes
  )
  rm(municipalities, neighboring_municipalities, indigenous_lands, gold_reserves, homicides,
     population, munincipalities_w_il, municipalities_w_gr, municipalities_la, predominant_biome, all_biomes)
  
  # Saving final dataset
  save(dataset, file = "dataset.RData")


  # ---- Workbook of Models ----

  # Creating list
  workbook <- list()

  # Running models
  tribble(
    ~model_name,                 ~sample_filter_expr,                               ~treatment_group_condition_epxr,    ~years,                  ~model_expr,
    ## [2016]
    # HR w/ Total Homicides
    "2016_def_lo_x85_y35",       NA,                                                 "gold == 1 & il == 1",            c(2010:2014, 2016:2022), "tx_hom_tot_lo_x85_y35 ~ gold:post2016 + il:post2016 + gold:il:post2016", 
    "2016_bio_lo_x85_y09_y35",   "filter(., amz_cer == T)",                          "gold == 1 & il == 1",            c(2010:2014, 2016:2022), "tx_hom_tot_lo_x85_y09_y35 ~ gold:post2016 + il:post2016 + gold:il:post2016",
    "2016_bio_lo_picking",       "filter(., amz_cer == T)",                          "gold == 1 & il == 1",            c(2010:2014, 2016:2022), "tx_hom_tot_lo_picking ~ gold:post2016 + il:post2016 + gold:il:post2016",
    "2016_bio_lo_x60_y35",       "filter(., amz_cer == T)",                          "gold == 1 & il == 1",            c(2010:2014, 2016:2022), "tx_hom_tot_lo_x60_y35 ~ gold:post2016 + il:post2016 + gold:il:post2016",
    "2016_bio_lo_x85_y35",       "filter(., amz_cer == T)",                          "gold == 1 & il == 1",            c(2010:2014, 2016:2022), "tx_hom_tot_lo_x85_y35 ~ gold:post2016 + il:post2016 + gold:il:post2016",
    "2016_bio_lo_x85_y35_sh",    "filter(., amz_cer == T)",                          "gold == 1 & il == 1",            c(2010:2014, 2016:2022), "tx_hom_tot_lo_x85_y35_sh ~ gold:post2016 + il:post2016 + gold:il:post2016",
    "2016_def_lr_x85_y35",       NA,                                                 "gold == 1 & il == 1",            c(2010:2014, 2016:2022), "tx_hom_tot_lr_x85_y35 ~ gold:post2016 + il:post2016 + gold:il:post2016",
    "2016_bio_lr_x85_y35",       "filter(., amz_cer == T)",                          "gold == 1 & il == 1",            c(2010:2014, 2016:2022), "tx_hom_tot_lr_x85_y35 ~ gold:post2016 + il:post2016 + gold:il:post2016",
    "2016_bio_lr_x85_y09_y35",   "filter(., amz_cer == T)",                          "gold == 1 & il == 1",            c(2010:2014, 2016:2022), "tx_hom_tot_lr_x85_y09_y35 ~ gold:post2016 + il:post2016 + gold:il:post2016",
    # HR w/ Indigenous Homicides
    "2016_def_ind_lr_x60_x84",   NA,                                                 "gold == 1 & il == 1",            c(2010:2014, 2016:2022), "tx_hom_ind_lr_x60_x84 ~ gold:post2016 + il:post2016 + gold:il:post2016",
    "2016_def_ind_lr_x60_y09",   NA,                                                 "gold == 1 & il == 1",            c(2010:2014, 2016:2022), "tx_hom_ind_lr_x60_y09 ~ gold:post2016 + il:post2016 + gold:il:post2016",
    "2016_def_ind_lr_x60_y35",   NA,                                                 "gold == 1 & il == 1",            c(2010:2014, 2016:2022), "tx_hom_ind_lr_x60_y35 ~ gold:post2016 + il:post2016 + gold:il:post2016",
    "2016_bio_ind_lr_x60_x84",   "filter(., amz_cer == T)",                          "gold == 1 & il == 1",            c(2010:2014, 2016:2022), "tx_hom_ind_lr_x60_x84 ~ gold:post2016 + il:post2016 + gold:il:post2016",
    "2016_bio_ind_lr_x60_y09",   "filter(., amz_cer == T)",                          "gold == 1 & il == 1",            c(2010:2014, 2016:2022), "tx_hom_ind_lr_x60_y09 ~ gold:post2016 + il:post2016 + gold:il:post2016",
    "2016_bio_ind_lr_x60_y35",   "filter(., amz_cer == T)",                          "gold == 1 & il == 1",            c(2010:2014, 2016:2022), "tx_hom_ind_lr_x60_y35 ~ gold:post2016 + il:post2016 + gold:il:post2016",
     # Other Specifications
    "2016_cg3_lo_x85_y35",       "filter(., (gold == 1 & il == 1) | il == 1)",       "gold == 1 & il == 1",            c(2010:2014, 2016:2022), "tx_hom_tot_lo_x85_y35 ~ gold + post2016 + gold:post2016",
    "2016_cg4_lo_x85_y35",       "filter(., (gold == 1 & il == 1) | gold == 1)",     "gold == 1 & il == 1",            c(2010:2014, 2016:2022), "tx_hom_tot_lo_x85_y35 ~ il + post2016 + il:post2016",
    "2016_tr2_lo_x85_y35",       NA,                                                 "la == 1",                        c(2010:2014, 2016:2022), "tx_hom_tot_lo_x85_y35 ~ la + post2016 + la:post2016",
    "2016_tr3_lo_x85_y35",       NA,                                                 "gold == 1 & la == 1",            c(2010:2014, 2016:2022), "tx_hom_tot_lo_x85_y35 ~ gold:post2016 + la:post2016 + gold:la:post2016",
    "2016_la1_lo_x85_y35",       "filter(., la == 1)",                               "gold == 1 & il == 1",            c(2010:2014, 2016:2022), "tx_hom_tot_lo_x85_y35 ~ gold:post2016 + il:post2016 + gold:il:post2016",
    "2016_bio_avre",             "filter(., amz_cer == T)",                          "gold == 1 & il == 1",            c(2010:2014, 2016:2022), "tx_hom_tot_avre ~ gold:post2016 + il:post2016 + gold:il:post2016",
    "2016_bio_aves",             "filter(., amz_cer == T)",                          "gold == 1 & il == 1",            c(2010:2014, 2016:2022), "tx_hom_tot_aves ~ gold:post2016 + il:post2016 + gold:il:post2016",
    
    # 2017
    "2017_def",       NA,                                                     "gold == 1 & il == 1",         c(2010:2015, 2017:2022), "tx_hom_tot_lo_x85_y35 ~ gold:post2017 + il:post2017 + gold:il:post2017", 
    "2017_cg3",       "filter(., (gold == 1 & il == 1) | il == 1)",           "gold == 1 & il == 1",         c(2010:2015, 2017:2022), "tx_hom_tot_lo_x85_y35 ~ gold + post2017 + gold:post2017",
    "2017_cg4",       "filter(., (gold == 1 & il == 1) | gold == 1)",         "gold == 1 & il == 1",         c(2010:2015, 2017:2022), "tx_hom_tot_lo_x85_y35 ~ il + post2017 + il:post2017",
    "2017_tr2",       NA,                                                     "la == 1",                     c(2010:2015, 2017:2022), "tx_hom_tot_lo_x85_y35 ~ la + post2017 + la:post2017",
    "2017_tr3",       NA,                                                     "gold == 1 & la == 1",         c(2010:2015, 2017:2022), "tx_hom_tot_lo_x85_y35 ~ gold:post2017 + la:post2017 + gold:la:post2017",
    "2017_la1",       "filter(., la == 1)",                                   "gold == 1 & il == 1",         c(2010:2015, 2017:2022), "tx_hom_tot_lo_x85_y35 ~ gold:post2017 + il:post2017 + gold:il:post2017",
    "2017_bio",       "filter(., amz_cer == T)",                              "gold == 1 & il == 1",         c(2010:2015, 2017:2022), "tx_hom_tot_lo_x85_y35 ~ gold:post2017 + il:post2017 + gold:il:post2017",
    "2017_bio_lo_x85_y35",       "filter(., amz_cer == T)",                          "gold == 1 & il == 1",            c(2010:2015, 2017:2022), "tx_hom_tot_lo_x85_y35 ~ gold:post2017 + il:post2017 + gold:il:post2017",
    
    # 2018
    "2013_bio_lr_x85_y35",       "filter(., amz_cer == T)",                          "gold == 1 & il == 1",            c(2010:2011, 2013:2022), "tx_hom_tot_lr_x85_y35 ~ gold:post2013 + il:post2013 + gold:il:post2013",
    "2015_bio_lr_x85_y35",       "filter(., amz_cer == T)",                          "gold == 1 & il == 1",            c(2010:2013, 2015:2022), "tx_hom_tot_lr_x85_y35 ~ gold:post2015 + il:post2015 + gold:il:post2015",
    "2018_bio_lr_x85_y35",       "filter(., amz_cer == T)",                          "gold == 1 & il == 1",            c(2010:2016, 2018:2022), "tx_hom_tot_lr_x85_y35 ~ gold:post2018 + il:post2018 + gold:il:post2018",
    "2019_bio_lr_x85_y35",       "filter(., amz_cer == T)",                          "gold == 1 & il == 1",            c(2010:2017, 2019:2022), "tx_hom_tot_lr_x85_y35 ~ gold:post2019 + il:post2019 + gold:il:post2019",
    "2013_bio_lo_x85_y35",       "filter(., amz_cer == T)",                          "gold == 1 & il == 1",            c(2010:2011, 2013:2022), "tx_hom_tot_lo_x85_y35 ~ gold:post2013 + il:post2013 + gold:il:post2013",
    "2015_bio_lo_x85_y35",       "filter(., amz_cer == T)",                          "gold == 1 & il == 1",            c(2010:2013, 2015:2022), "tx_hom_tot_lo_x85_y35 ~ gold:post2015 + il:post2015 + gold:il:post2015",
    "2018_bio_lo_x85_y35",       "filter(., amz_cer == T)",                          "gold == 1 & il == 1",            c(2010:2016, 2018:2022), "tx_hom_tot_lo_x85_y35 ~ gold:post2018 + il:post2018 + gold:il:post2018",
    "2019_bio_lo_x85_y35",       "filter(., amz_cer == T)",                          "gold == 1 & il == 1",            c(2010:2017, 2019:2022), "tx_hom_tot_lo_x85_y35 ~ gold:post2019 + il:post2019 + gold:il:post2019",
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
  
  # Main Reesults
  workbook$plot_event_study$`2016_bio_lo_x85_y35`
  workbook$model$`2016_bio_lo_x85_y35`
  workbook$model_sy$`2016_bio_lo_x85_y35`
  workbook$model_wls$`2016_bio_lo_x85_y35`
  workbook$model_wls_sy$`2016_bio_lo_x85_y35`
  workbook$model_il1$`2016_bio_lo_x85_y35`
  workbook$model_il0$`2016_bio_lo_x85_y35`
  workbook$model_gold1$`2016_bio_lo_x85_y35`
  workbook$model_gold0$`2016_bio_lo_x85_y35`
  workbook$model_fake13$`2016_bio_lo_x85_y35`
  workbook$model_fake14$`2016_bio_lo_x85_y35`
  workbook$model_dual_break$`2016_bio_lo_x85_y35`
  workbook$model_dual_break_dddest$`2016_bio_lo_x85_y35`$total
  workbook$model_dual_break_sy$`2016_bio_lo_x85_y35`
  workbook$model_dual_break_sy_dddest$`2016_bio_lo_x85_y35`$total
  workbook$wald$`2016_bio_lo_x85_y35`

  workbook$plot_event_study$`2017_bio_lo_x85_y35`
  workbook$model$`2017_bio_lo_x85_y35`
  workbook$model_sy$`2017_bio_lo_x85_y35`
  workbook$model_wls$`2017_bio_lo_x85_y35`
  workbook$model_wls_sy$`2017_bio_lo_x85_y35`
  workbook$model_il1$`2017_bio_lo_x85_y35`
  workbook$model_il0$`2017_bio_lo_x85_y35`
  workbook$model_gold1$`2017_bio_lo_x85_y35`
  workbook$model_gold0$`2017_bio_lo_x85_y35`
  workbook$model_fake13$`2017_bio_lo_x85_y35`
  workbook$model_fake14$`2017_bio_lo_x85_y35`
  workbook$model_dual_break$`2017_bio_lo_x85_y35`
  workbook$lin_combo$`2017_bio_lo_x85_y35`
  workbook$model_dual_break$`2017_bio_lo_x85_y35`
  workbook$model_dual_break_dddest$`2017_bio_lo_x85_y35`$total
  workbook$model_dual_break_sy$`2017_bio_lo_x85_y35`
  workbook$model_dual_break_sy_dddest$`2017_bio_lo_x85_y35`$total
  workbook$wald$`2017_bio_lo_x85_y35`
  
  # ---- Charts ----

  ## Multiple Dependent Variables
  tibble(
    name = c(rep("X85-Y35", 12), rep("X85-Y09-Y35 (AV equivalent)", 12), rep("X85-Y35 (except Y10-Y19, Y21, Y25-Y27)", 12), rep("X60-Y35", 12)),
    ano  = rep(setdiff(seq(-5, 7, by = 1), 0), 4),
    term = c(workbook$event_study_rse$`2016_bio_lo_x85_y35`$term, workbook$event_study_rse$`2016_bio_lo_x85_y09_y35`$term, workbook$event_study_rse$`2016_bio_lo_picking`$term, workbook$event_study_rse$`2016_bio_lo_x60_y35`$term),
    estimate = c(workbook$event_study_rse$`2016_bio_lo_x85_y35`$estimate, workbook$event_study_rse$`2016_bio_lo_x85_y09_y35`$estimate, workbook$event_study_rse$`2016_bio_lo_picking`$estimate, workbook$event_study_rse$`2016_bio_lo_x60_y35`$estimate),
    std.error = c(workbook$event_study_rse$`2016_bio_lo_x85_y35`$std.error, workbook$event_study_rse$`2016_bio_lo_x85_y09_y35`$std.error, workbook$event_study_rse$`2016_bio_lo_picking`$std.error, workbook$event_study_rse$`2016_bio_lo_x60_y35`$std.error)
  ) %>% 
    ggplot(., aes(x = ano, 
                  y = estimate)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "red") +
    geom_point(aes(color = name, shape = name), size = 3.5, position = position_dodge(width = .6)) +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                      ymax = estimate + 1.96 * std.error,
                      color = name),
                  width = 0.2, position = position_dodge(width = .6)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    scale_x_continuous(breaks = seq(-5, 7, by = 1)) +
    scale_color_manual(values = c("orange", "#ffdb99", "#9999ff", "darkblue", "#4c4cff", "#ccccff"), 
                       limits = c("X85-Y35", "X85-Y09-Y35 (AV equivalent)", "X85-Y35 (except Y10-Y19, Y21, Y25-Y27)", "X60-Y35")) +
    scale_shape_manual(values = c(16, 20, 16, 17, 19, 20),
                       limits = c("X85-Y35", "X85-Y09-Y35 (AV equivalent)", "X85-Y35 (except Y10-Y19, Y21, Y25-Y27)", "X60-Y35")) +
    labs(x = str_c("Years to Treatment (", setdiff(2010:2022, c(2010:2014, 2016:2022)), " = 0)"),
         y = "DDD coefficient (Gold x TI x Post)",
         title = str_c("Dynamic DDD in", 'Cerrado and Amazonia Municipalities', sep = " "),
         subtitle = "Any-overlap biome definition; 2010-2022 sample",
         color = "Homicide Rate Variable", shape = "Homicide Rate Variable") +
    theme(axis.line.x = element_blank(),
          legend.position = "inside",
          legend.position.inside = c(0.2, 0.8))
  

  ## Map: Municipalities with IT and Gold Reserves 
  
  base_mapa <- (dataset %>%
                  filter(year == 2022) %>%
                  right_join(aux$municipalities, by = c("code_muni", "name_muni")) %>%
                  st_as_sf() %>%  
                  select(name_muni, il, gold, geometry) %>% 
                  mutate(il = ifelse(is.na(il), 0, il),
                         gold = ifelse(is.na(gold), 0, gold),
                         bi_class = case_when(il == 1 & gold == 0 ~ "2-1",
                                              il == 1 & gold == 1 ~ "2-2",
                                              il == 0 & gold == 0 ~ "1-1",
                                              il == 0 & gold == 1 ~ "1-2"))) 
  
  
  (ggdraw() +
      draw_plot(ggplot() +
                  geom_sf(data = geobr::read_country(year = 2020), colour = "black") +
                  geom_sf(data = base_mapa, mapping = aes(fill = bi_class), colour = NA, linewidth = 1, show.legend = FALSE) +
                  bi_scale_fill(pal = "GrPink", dim = 2) +
                  geom_sf(data = geobr::read_amazon(), alpha = 0.01, colour = "green", linewidth = 1.5) +
                  #labs(title = "Indigenous Territories and Provinces/Districts of Gold \n in brazilian municipalities",
                  #     subtitle = "") +
                  
                  #geom_curve(aes(x = -62, y = 2, xend = -70, yend = 7),
                  #           arrow = arrow(length = unit(0.02, "npc"), type = "closed"),
                  #           curvature = 0.5) +
                  #annotate("text", label = "In wine color, municipalities that have \n indigenous territories and \n provinces/districts of gold \n in their territory",
                  #         x = -80, y = 7, size = 3.5, family = "Montserrat") +
                  #
                  #geom_curve(aes(x = -49, y = 0, xend = -42, yend = 8),
                  #           arrow = arrow(length = unit(0.02, "npc"), type = "closed"),
                  #           curvature = -0.5) +
                  #annotate("text", label = "Brazil's Legal Amazon as defined \n in the federal law n. 12.651/2012", 
                  #         x = -30, y = 8, size = 3.5, family = "Montserrat") +   
                  
                  theme(plot.background = element_blank(),
                        plot.title = element_text(size = 23, hjust = 0.5),
                        panel.grid = element_blank(),
                        panel.background = element_blank(),
                        axis.title.y = element_blank(),
                        axis.title.x = element_blank(),
                        axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank()) +
                  coord_sf(clip = 'off', datum = NA), 
                0, 0, 1, 1) +
      draw_plot(bi_legend(pal = "GrPink", dim = 2,
                          xlab = "Indigenous Territory", ylab = "Gold",
                          size = 20, arrows = F,
                          breaks = list(bi_x = c("No", "Yes"), 
                                        bi_y = c("No", "Yes"))) +
                  theme(plot.background = element_blank(),
                        panel.background = element_blank(),
                        axis.line = element_blank(),
                        axis.ticks = element_blank(),
                        axis.text = element_text(size = 12, color = "black"),
                        axis.title = element_markdown(size = 15, face = "bold"),
                        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), angle = 0, vjust = 0.5),
                        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))), 
                0.05, 0.15, 0.3, 0.3) + 
      #draw_plot(ggplot(data = aux$indigenous_lands) +
      #            geom_sf() +
      #            geom_sf(data = geobr::read_country(), alpha = 0.01) +
      #            labs(title = "Indigenous \n Territories") +  
      #            theme(plot.background = element_blank(),
      #                  plot.title = element_text(size = 8.5, hjust = 0.5),
      #                  panel.background = element_blank(),
      #                  axis.line = element_blank(),
      #                  axis.ticks = element_blank(),
      #                  panel.grid = element_blank(),
      #                  axis.text = element_blank()) +
      #            coord_sf(datum = NA),
      #          0.65, 0.05, 0.25, 0.30) +
      #draw_plot(ggplot(data = st_sfc((st_combine(aux$gold_reserves$province$geometry))[[1]], (st_combine(aux$gold_reserves$district$geometry))[[1]]) %>% 
      #                   st_combine %>% 
      #                   st_union(by_feature = T) %>% 
      #                   st_set_crs(st_crs(aux$municipalities))) +
      #            geom_sf() +
      #            geom_sf(data = geobr::read_country(), alpha = 0.01) +
      #            labs(title = "Provinces/Districts \n of Gold") +  
      #            theme(plot.background = element_blank(),
      #                  plot.title = element_text(size = 8.5, hjust = 0.5),
      #                  panel.background = element_blank(),
      #                  axis.line = element_blank(),
      #                  axis.ticks = element_blank(),
      #                  panel.grid = element_blank(),
      #                  axis.text = element_blank()) +
      #            coord_sf(datum = NA),
      #          0.80, 0.05, 0.25, 0.30) +
      draw_plot(ggplot(data = aux$municipalities %>% 
                         right_join(aux$predominant_biome %>% 
                                      filter(bioma %in% c('Amazônia', 'Cerrado')) %>% 
                                      mutate(code_muni = substr(as.character(geocodigo), 1, 6), .keep = "unused", .before = bioma), 
                                    by = "code_muni")) +
                  geom_sf(data = geobr::read_country(), alpha = 0.01) +
                  geom_sf(aes(fill = bioma), colour = NA, linewidth = 0) +
                  scale_fill_manual(values = c("Amazônia" = "darkgreen", "Cerrado" = "tan4")) +
                  labs(title = "Predominant Biome", fill = "Biome") +  
                  theme(plot.background = element_blank(),
                        plot.title = element_text(size = 8.5, hjust = 0.5),
                        legend.position = "bottom",
                        legend.text = element_text(size = 7),
                        legend.title = element_text(size = 8),
                        legend.key.spacing.x = unit(0.25, "cm"),
                        panel.background = element_blank(),
                        axis.line = element_blank(),
                        axis.ticks = element_blank(),
                        panel.grid = element_blank(),
                        axis.text = element_blank()) +
                  coord_sf(datum = NA),
                0.70, 0.35, 0.30, 0.40) 
  ) %>%  
  ggsave("./MapaTIOU.png", ., width = 12, height = 6, units = "in", dpi = 300)
  rm(df_map)