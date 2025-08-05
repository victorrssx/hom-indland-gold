
  ########################################################
  ################                        ################
  ################       TI, H e Au       ################
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
                   EnvStats, broom, estimatr, lmtest, plm, fixest, fastDummies, car,
                   read.dbc)
  }
  
  # -------------------------------------------------------
  
  
  # O ideal é montar um base com:
  # . 5570 (municípios) * 13 (anos entre 2010-2022) observações
  # . 11 colunas (cod_mun, nome_mun, ano, hom_tot, hom_ind, ti, qtd_ti, perc_ti, res_ouro, perc_res_ouro, pos2018) 
  
  # res_ouro é variável binária constante ao longo do tempo, dado que uma reserva não se desenvolve em 13 anos
  # perc_res_ouro é contínua e mostra o percentual do território do município coberto por alguma província/distrito de ouro
  # ti é variável binária que se inicia a partir da portaria de regularização da primeira TI no município 
  # qtd_ti é discreta e varia conforme os anos, mostrando o acúmulo de TI em determinado município 
  # perc_ti é contínua e mostra o percentual do território do município coberto por alguma terra indígena
  
  
  
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
  
  
  ## Terras Indígenas. 
   # Fonte: FUNAI 
  
  terra_indigena <- read_sf("./2. Terras Indígenas - Shapefile/tis_poligonais_portariasPolygon.shp",
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
  
  st_crs(municipios$geom) == st_crs(terra_indigena$geometry) # 'make sure your coordinate systems projections are equal'
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
    
               mutate(ti = ifelse(code_muni %in% (terra_indigena %>% 
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
                     
  
  # Quantos municípios com observações >>completas<< para homicídios? Isto é, homicídio != NA
  # em todos os anos (2010-2022). No total, 1923, representando 1923 * 13 = 24999 observações.    
  # E para homícidios de indígenas? Apenas 6 (78 observações). Note que esse procedimento não 
  # não é a mesma coisa que contar municípios com uma ou mais observações != NA. 
  
  # Para verificar os cálculos acima (para indígenas, trocar hom_tot por hom_ind):
  # n_distinct((base_final %>% filter(!any(is.na(hom_tot)), .by = code_muni))$code_muni)
  
  
  # (Obs) 'Problema': nem sempre um município identificado com garimpo ilegal de ouro pelo
  # MapBiomas possui reserva de ouro identificada através do cruzamento de dados da base 
  # do SGB com a do {geobr}. Ao todo, dos 79 identificados pelo MapBiomas, 68 tem reserva
  # de ouro pelo SGB (logicamente, deveriam ser todos).
  
  base_final %>% 
    filter(ga2018 == 1) %>% 
    mutate(teste = ifelse(ga2018 == 1 & res_ou == 1, 1, 0)) %>%
    summarise(n = sum(teste), .by = code_muni) %>% 
    summarise(num = n(), .by = n)
  
  
  

  
  
  # ---- Regressões ----
  
  model <- feols(tx_hom_tot ~ res_ou:pos2018 + ti:pos2018 + res_ou:ti:pos2018 | ano + code_muni, 
                 data = base_final)
  summary(model, vcov = "hc1")  
  

  # "Após 2018, qual a diferença na tx de hom entre os municípios com/sem TI e conflito?"
  # Amostra com municípios apenas com todos os dados de hom. disponíveis de 2010 a 2022
    # mun c/ ti após 2018: na média +0,7
    # mun geral com garimpo: na média +0,01
    # mun c/ ti e garimpo após 2018: +12,5 na tx. de hom. ***

  model <- feols(tx_hom_tot ~ ti:pos2018 + ga2018:pos2018 + ti:ga2018:pos2018 | ano + code_muni, 
                 data = base_final) 
  summary(model, vcov = "hc1")
  


  
  
  
  # ---- Plotando ----
  
  
  ## Separadamente
  
  # Municípios
  ggplot(municipios) + 
    geom_sf(aes(geometry = geometry))
  
  # Terras Indígenas
  ggplot(terra_indigena) +
    geom_sf(aes(geometry = geometry))
  
  # Províncias de Ouro
  ggplot(provincia) +
    geom_sf(aes(geometry = geometry))
  
  # Distritos de Ouro
  ggplot(distrito) +
    geom_sf(aes(geometry = geometry))
  
  # União de Províncias e Distritos de Ouro
  ggplot(st_sfc((st_combine(provincia$geometry))[[1]], (st_combine(distrito$geometry))[[1]]) %>% 
           st_combine %>% 
           st_union(by_feature = T)) +
    geom_sf(aes(geometry = geometry))
  
  
  ## Mapa: Municípios pela presença de TI e ResOu 
  
  base_mapa <- (base_final %>%
                filter(ano == 2022) %>%
                right_join(municipios, by = c("code_muni", "name_muni")) %>%
                st_as_sf() %>%  
                select(name_muni, ti, res_ou, geometry) %>% 
                mutate(ti = ifelse(is.na(ti), 0, ti),
                       res_ou = ifelse(is.na(res_ou), 0, res_ou),
                       bi_class = case_when(ti == 1 & res_ou == 0 ~ "2-1",
                                            ti == 1 & res_ou == 1 ~ "2-2",
                                            ti == 0 & res_ou == 0 ~ "1-1",
                                            ti == 0 & res_ou == 1 ~ "1-2"))) 
  
  
  (ggdraw() +
      draw_plot(ggplot() +
                  geom_sf(data = geobr::read_country(year = 2020), colour = "black") +
                  geom_sf(data = base_mapa, mapping = aes(fill = bi_class), colour = NA, linewidth = 1, show.legend = FALSE) +
                  bi_scale_fill(pal = "GrPink", dim = 2) +
                  geom_sf(data = geobr::read_amazon(), alpha = 0.01, colour = "green", linewidth = 1.5) +
                  labs(title = "**Indigenous Territories** and **Provinces/Districts of Gold** <br> in brazilian municipalities",
                       subtitle = "") +
                  
                  coord_sf(clip = 'off') +  
                  geom_curve(aes(x = -62, y = 2, xend = -70, yend = 7),
                             arrow = arrow(length = unit(0.02, "npc"), type = "closed"),
                             curvature = 0.5) +
                  annotate("text", label = "In wine color, municipalities that have \n indigenous territories and \n provinces/districts of gold \n in their territory",
                           x = -80, y = 7, size = 3.5) +
                  
                  geom_curve(aes(x = -49, y = 0, xend = -42, yend = 4),
                             arrow = arrow(length = unit(0.02, "npc"), type = "closed"),
                             curvature = -0.5) +
                  annotate("text", label = "Brazil's Legal Amazon as defined \n in the federal law n. 12.651/2012", 
                           x = -30, y = 4, size = 3.5) +   
                  
                  theme(plot.background = element_blank(),
                        plot.title = element_markdown(size = 23, hjust = 0.5),
                        panel.grid = element_blank(),
                        panel.background = element_blank(),
                        axis.title = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank()), 
                0, 0, 1, 1) +
      draw_plot(bi_legend(pal = "GrPink", dim = 2,
                          xlab = "Indigenous Territory", ylab = "Gold",
                          size = 20, arrows = F,
                          breaks = list(bi_x = c("No", "Yes"), 
                                        bi_y = c("No", "Yes"))) +
                  theme(plot.background = element_blank(),
                        panel.background = element_blank(),
                        axis.ticks = element_blank(),
                        axis.text = element_text(size = 12, color = "black"),
                        axis.title = element_markdown(size = 15, face = "bold"),
                        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), angle = 0, vjust = 0.5),
                        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))), 
                0.05, 0.15, 0.3, 0.3) + 
      draw_plot(ggplot(data = terra_indigena) +
                  geom_sf() +
                  geom_sf(data = geobr::read_country(), alpha = 0.01) +
                  labs(title = "Indigenous \n Territories") +  
                  theme(plot.background = element_blank(),
                        plot.title = element_text(size = 8.5, hjust = 0.5),
                        panel.background = element_blank(),
                        axis.ticks = element_blank(),
                        panel.grid = element_blank(),
                        axis.text = element_blank()),
                0.65, 0.05, 0.25, 0.30) +
      draw_plot(ggplot(data = st_sfc((st_combine(provincia$geometry))[[1]], (st_combine(distrito$geometry))[[1]]) %>% 
                              st_combine %>% 
                              st_union(by_feature = T) %>% 
                              st_set_crs(st_crs(municipios))) +
                  geom_sf() +
                  geom_sf(data = geobr::read_country(), alpha = 0.01) +
                  labs(title = "Provinces/Districts \n of Gold") +  
                  theme(plot.background = element_blank(),
                        plot.title = element_text(size = 8.5, hjust = 0.5),
                        panel.background = element_blank(),
                        axis.ticks = element_blank(),
                        panel.grid = element_blank(),
                        axis.text = element_blank()),
                0.80, 0.05, 0.25, 0.30)) # %>% 
    
    ggsave("./MapaTIOU.png", ., width = 9, height = 6, units = "in", dpi = 300)
    rm(df_map)
  
    
    ## Tendências Paralelas
    
    base_final %>% 
      filter(ti == 1) %>%
      summarise(hom_tot_sum = sum(hom_tot, na.rm = T),
                pop_sum = sum(pop), 
                .by = c(ano, res_ou)) %>%
      mutate(res_ou = as.character(res_ou),
             tx_hom_tot_sum = (hom_tot_sum / pop_sum) * 100000) %>% 
      
      {ggplot(., aes(x = ano, y = tx_hom_tot_sum, group = res_ou)) +
          geom_vline(xintercept = '2018', linetype = "dashed") +
          geom_point(aes(color = res_ou), size = 4) +
          geom_line(aes(color = res_ou), linewidth = 0.8) +
          scale_color_manual(values = c('darkblue', 'darkred')) +
          labs(x = "Year", y = "Violent mortality ratio \n (municipalities with ind. land)")} %>% 
      
      ggsave("./plot0.png", ., width = 12, height = 7, units = "in", dpi = 300)  
    
    
    base_final %>% 
      filter(ti == 1) %>%
      summarise(hom_tot_sum = sum(hom_tot, na.rm = T),
                pop_sum = sum(pop), 
                .by = c(ano, res_ou)) %>%
      mutate(tx_hom_tot_sum = (hom_tot_sum / pop_sum) * 100000) %>%
      select(-c(hom_tot_sum, pop_sum)) %>% 
      pivot_wider(names_from = res_ou, values_from = tx_hom_tot_sum) %>% 
      mutate(razao = `1`/`0`) %>% 
      
      {ggplot(., aes(x = ano, y = razao, group = 1)) +
          geom_vline(xintercept = '2018', linetype = "dashed") +
          geom_line(color = 'darkblue', linewidth = 0.8) +
          geom_point(color = 'darkblue', fill = 'blue', size = 4) +
          labs(x = "Year", y = "Violent mortality ratio \n (municipalities - ind. land and gold res. / \n municipalities - ind. land without gold res.)")} %>% 
      
      ggsave("./plot1.png", ., width = 12, height = 7, units = "in", dpi = 300)
    
    
    base_final %>% 
      filter(ti == 1) %>%
      summarise(hom_tot_sum = sum(hom_tot, na.rm = T),
                pop_sum = sum(pop), 
                .by = c(ano, ga2018)) %>%
      mutate(tx_hom_tot_sum = (hom_tot_sum / pop_sum) * 100000) %>%
      select(-c(hom_tot_sum, pop_sum)) %>% 
      pivot_wider(names_from = ga2018, values_from = tx_hom_tot_sum) %>% 
      mutate(razao = `1`/`0`) %>% 
      
      {ggplot(., aes(x = ano, y = razao, group = 1)) +
          geom_vline(xintercept = '2018', linetype = "dashed") +
          geom_line(color = 'darkblue', linewidth = 0.8) +
          geom_point(color = 'darkblue', fill = 'blue', size = 4) +
          labs(x = "Year", y = "Violent mortality ratio \n (municipalities - ind. land and minning / \n municipalities - ind. land without minning)")} %>% 
      
      ggsave("./plot2.png", ., width = 12, height = 7, units = "in", dpi = 300)
    