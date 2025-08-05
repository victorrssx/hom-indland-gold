  
  ########################################################
  ################                        ################
  ################     Nome do Projeto    ################
  ################       xx/xx/20xx       ################
  ################                        ################
  ########################################################
  # usethis::edit_file("~/AppData/Roaming/RStudio/templates/default.R")
  
  {
    extrafont::loadfonts(device = "win")
    options(timeout = max(1000, getOption("timeout")))
  
    if (!require("pacman")) install.packages("pacman") else library(pacman)
    pacman::p_load(tidyverse, magrittr)
  }
    
  # -------------------------------------------------------
  

  ## Comparando estimativas de diferentes pacotes sem dummy pos2018 (que causa colinearidade)

  df %>% 
    select(hom_tot, code_muni, ano, res_ou, ti, pos2018) %>% 
    mutate(`res_ou:pos2018` = res_ou * pos2018,
           `ti:pos2018` =  ti * pos2018,
           `res_ou:ti:pos2018` =  res_ou * ti * pos2018) %>%
    dummy_cols("ano") %>%
    filter(!is.na(hom_tot)) %>% 
    # count(`res_ou:pos2018`, `ti:pos2018`, `res_ou:ti:pos2018`)
    select(hom_tot, pos2018, `res_ou:pos2018`, `ti:pos2018`, `res_ou:ti:pos2018`, contains("ano_")) -> df1

 
  model1 <- lm(tx_hom_tot ~ res_ou:pos2018 + ti:pos2018 + res_ou:ti:pos2018 + ano, data = df)    
  # model1 <- coeftest(model1, vcov. = vcovHC(model1, type = "HC1"))
  
  model2 <- feols(tx_hom_tot ~ pos2018 + res_ou:pos2018 + ti:pos2018 + res_ou:ti:pos2018 | ano, data = df, vcov = "hc1")
  
  model3 <- lm_robust(tx_hom_tot ~ pos2018 + res_ou:pos2018 + ti:pos2018 + res_ou:ti:pos2018 + ano, data = df, 
                      se_type = "HC1")    
  
  cbind(
  `stats::lm` = coef(model1),
  `fixest::feols` =   c(unlist(fixef(model2)), coef(model2)) %>% as.data.frame() %>% rownames_to_column() %>% `colnames<-`(c("coef", "fixest::feols")) %>%   
                      mutate(`fixest::feols` = ifelse(!(coef %in% c("ano.2010", "res_ou:pos2018", "pos2018:ti", "res_ou:pos2018:ti")), `fixest::feols` - 26.8272932, `fixest::feols`)) %>% 
                      select(`fixest::feols`),
  `estimatr::lm_robust` = coef(model3)
  ) %>% 
    as.data.frame %>% 
    mutate(across(where(is.numeric), function(x) round(x, 7)))
  
  
  ## Homicidios totais por ano
  
  df %>% 
    summarise(soma_hom = sum(hom_tot, na.rm = T),
              soma_pop = sum(pop, na.rm = T), .by = ano) %>%
    mutate(tx_hom_pop_br = soma_hom / soma_pop) %>% 
    {ggplot(., aes(x = ano, y = tx_hom_pop_br * 100000)) +
        geom_col()}
  
  
  ## st_within
  sf_use_s2(FALSE)
  
  # Selecionando TIs que estão completamente dentro de Províncias/Distritos de Ouro 
  terra_indigena_reou <- terra_indigena[st_within(terra_indigena, st_sfc((st_combine(provincia_ouro$geometry))[[1]], (st_combine(distrito_ouro$geometry))[[1]]) %>% 
                                                                  st_combine %>% 
                                                                  st_union(by_feature = T) %>% 
                                                                  st_set_crs(st_crs(terra_indigena)), sparse = F),]
  
  # Identificando municípios que intersectam essas TIs (incluindo apenas tocando suas bordas)
  mun_ti_reou_intersects <- st_intersects(municipios, terra_indigena_reou) %>%   
                            set_names(municipios$code_muni) %>% 
                            map(\(x) if(length(x) >= 1) {str_flatten_comma(x)}) %>%
                            bind_rows %>%
                            colnames
  
  # Identificando municípios que apenas tocam essas TIs
  mun_ti_reou_touches <- st_touches(st_set_precision(municipios, 0.0095), 
                                    st_set_precision(terra_indigena_reou, 0.01)) %>%   
                         set_names(municipios$code_muni) %>% 
                         map(\(x) if(length(x) >= 1) {str_flatten_comma(x)}) %>%
                         bind_rows %>%
                         colnames
  
  # Removendo os que apenas tocam (precisão 0.01)
  mun_ti_reou_intersects
  mun_ti_reou_touches
  municipios_terra_indigena_reou <- data.frame(code_muni = setdiff(mun_ti_reou_intersects, mun_ti_reou_touches),
                                               ti_within_reou = 1)
  
  
  # Notar que estamos falando de TIs que estão completamente contidas em 'reservas de ouro'. 
  # Isso não significa que essa TI vai estar inteiramente dentro do município! Em outras
  # palavras, mais de um município pode estar associado com determinadaa TI completamente 
  # contida em 'reserva de ouro'.
  
  df1 = left_join(df, municipios_terra_indigena_reou, by = "code_muni") %>% 
        mutate(ti_within_reou = replace_na(ti_within_reou, 0))
  
  # Mapa 
  ggplot() +
    geom_sf(data = municipios, aes(geometry = geometry)) +
    # geom_sf(data = terra_indigena, aes(geometry = geometry)) + # TI
    # União de Províncias/Distritos de Ouro
    # geom_sf(data = st_sfc((st_combine(provincia_ouro$geometry))[[1]], (st_combine(distrito_ouro$geometry))[[1]]) %>% 
                           #st_combine %>% 
                           #st_union(by_feature = T) %>% 
                           #st_set_crs(st_crs(terra_indigena)), aes(geometry = geometry), fill = "blue", alpha = 0.2) +
    geom_sf(data = municipios %>% filter(code_muni %in% (df1 %>% filter(ti_within_reou == 1) %>% pull(code_muni) %>% unique())), aes(geometry = geometry), fill = "green", alpha = 0.2) +
    # TI que estão inteiramente dentro de Províncias/Distritos de Ouro
    geom_sf(data = terra_indigena_reou, aes(geometry = geometry), fill = "red", alpha = 0.5) +
    coord_sf(xlim = c(-50, -52), ylim = c(-5, -8), expand = FALSE)
    
  
  ggplot() +
    geom_sf(data = municipios, aes(geometry = geometry)) +
    geom_sf(data = municipios %>% filter(code_muni %in% (df1 %>% filter(ti_within_reou == 1) %>% pull(code_muni) %>% unique())), aes(geometry = geometry), fill = "green", alpha = 0.2) +
    geom_sf(data = municipios %>% filter(code_muni == "150553"), aes(geometry = geometry), fill = "pink")

  
  
  
  
  
  
  
  
  
  provincias_distritos <- st_sfc((st_combine(provincia_ouro$geometry))[[1]], (st_combine(distrito_ouro$geometry))[[1]]) %>% 
                          st_combine %>% 
                          st_union(by_feature = T) %>% 
                          st_set_crs(st_crs(terra_indigena)) %>% 
                          st_cast('POLYGON')
  
  
  # Selecionando Províncias/Distritos de Ouro que estão completamente dentro de TIs 
  reou_terra_indigena <- terra_indigena[st_within(provincias_distritos, terra_indigena, sparse = F),]
  
  
  which(st_within(provincias_distritos, terra_indigena, sparse = F) == T)

  st_within(provincias_distritos, terra_indigena, sparse = F) 
  
  (as_tibble(provincias_distritos) %>% 
  st_set_geometry("geometry") %>% 
  mutate(area = st_area(.)) %>% 
  arrange(area))[1:40,] %>% 
    
  {ggplot() +
    # geom_sf(data = municipios, aes(geometry = geometry)) +
    geom_sf(data = terra_indigena, aes(geometry = geometry)) +
    geom_sf(data = ., aes(geometry = geometry), fill = "pink", color = "pink") +
    coord_sf(xlim = c(-75, -25), ylim = c(10, -35), expand = FALSE)}
  
  
  ## Talvez um critério alternativo seja por sobreposição de áreas, estabelecendo 
  ## um treshold percentual mínimo para se considerar que reserva de ouro está dentro da TI.  
  
  
  
  
  ## Identificando, de forma única, os municípios de TIs que estão em mais de 1 município
  
  {
  codigos_mun <- bind_rows(
    
    { # códigos dos municípios de TIs com > 1 município
    terra_indigena %>% 
    filter(grepl(",", uf_sigla)) %>% 
    {x <<- pull(., municipio_) %>% strsplit(split = ",", fixed = T); y <<- pull(., uf_sigla) %>% strsplit(split = ",", fixed = T)} 
    
    pmap(list(x, y), \(x, y) outer(x, y, str_c, sep = "/")) %>%
    set_names(terra_indigena %>% filter(grepl(",", uf_sigla)) %>% pull(gid)) %>%
    map(\(x) as.data.frame(x) %>% 
             pivot_longer(cols = 1:2, values_to = "name_muni") %>% 
             select(-name) %>% 
             left_join(municipios %>% st_drop_geometry %>% select(name_muni, code_muni), 
                       by = "name_muni") %>% 
             filter(!is.na(code_muni)) %>% 
             summarise(code_muni = str_c(code_muni, collapse = ","))) %>% 
    bind_rows(.id = "gid") %>% 
    mutate(gid = as.integer(gid))
    },
    
    { # códigos dos municípios de TIs com 1 município
    terra_indigena %>% 
    filter(!grepl(",", uf_sigla)) %>% 
    {x <<- pull(., municipio_) %>% strsplit(split = ",", fixed = T); y <<- pull(., uf_sigla) %>% strsplit(split = ",", fixed = T)} 
    
    pmap(list(x, y), \(x, y) outer(x, y, str_c, sep = "/")) %>%
    set_names(terra_indigena %>% filter(!grepl(",", uf_sigla)) %>% pull(gid)) %>%
    map(\(x) as.data.frame(x) %>% 
             rename("name_muni" = 1) %>%  
             left_join(municipios %>% st_drop_geometry %>% select(name_muni, code_muni), 
                       by = "name_muni") %>% 
             filter(!is.na(code_muni)) %>% 
             summarise(code_muni = str_c(code_muni, collapse = ","))) %>% 
    bind_rows(.id = "gid") %>% 
    mutate(gid = as.integer(gid))  
    }
  ) %>% 
  separate_rows(code_muni, sep = ',')
  
  rm(x, y)
  }
  
  

  
  ## Chunks antigos
  
  x <- st_intersection(municipios, st_union(reservas_ouro[["provincia"]]$geometry, reservas_ouro[["distrito"]]$geometry)) %>% 
    mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
    select(code_muni, name_muni, intersect_area) %>%   # only select columns needed to merge
    st_drop_geometry()  # drop geometry as we don't need it
  

  # percent = list(
  #   
  # perc_ti_atual = left_join( 
  #                            st_intersection(municipios, terra_indigena) %>% 
  #                              mutate(area = st_area(.) %>% as.numeric()) %>% 
  #                              as_tibble() %>% 
  #                              summarise(area_ti_muni = sum(area), .by = code_muni),
  #                           
  #                            municipios %>% 
  #                              mutate(area_muni = st_area(.) %>% as.numeric()) %>% 
  #                              st_drop_geometry() %>% 
  #                              as_tibble(),
  #                           
  #                            by = "code_muni") %>% 
  #                  relocate(area_ti_muni, .after = area_muni) %>% 
  #                  mutate(perc_ti = area_ti_muni / area_muni * 100), 
  # 
  # 
  # perc_ou = left_join( 
  #                      st_intersection(municipios, st_sfc((st_combine(provincia$geometry))[[1]], (st_combine(distrito$geometry))[[1]]) %>% 
  #                                                  st_combine %>% 
  #                                                  st_union(by_feature = T) %>% 
  #                                                  st_set_crs(st_crs(municipios))) %>% 
  #                        mutate(area = st_area(.) %>% as.numeric()) %>% 
  #                        as_tibble() %>% 
  #                        summarise(area_ou_muni = sum(area), .by = code_muni),
  #                       
  #                      municipios %>% 
  #                        mutate(area_muni = st_area(.) %>% as.numeric()) %>% 
  #                        st_drop_geometry() %>% 
  #                        as_tibble(),
  #                       
  #                      by = "code_muni") %>% 
  #            relocate(area_ou_muni, .after = area_muni) %>% 
  #            mutate(perc_ou = area_ou_muni / area_muni * 100)
  # 
  # )
  # attach(percent)
  
  # https://stackoverflow.com/questions/75532340/creating-column-of-intersected-objects
  #
  # ti_muni_intersects <- st_intersects(municipios, terra_indigena, sparse = T) %>%
  #                       set_names(municipios$code_muni) %>% 
  #                       map(\(x) if(length(x) >= 2) {str_flatten_comma(x)}) %>% 
  #                       bind_rows %>% 
  #                       pivot_longer(cols = 1:length(.), names_to = "code_muni", values_to = "id_ti") %>% 
  #                       separate_longer_delim(id_ti, delim = ", ")
  # 
  # ou_muni_intersects <- st_intersects(municipios, area_ouro, sparse = T) %>%
  #                       set_names(municipios$code_muni) %>% 
  #                       map(\(x) if(length(x) >= 2) {str_flatten_comma(x)}) %>% 
  #                       bind_rows %>% 
  #                       pivot_longer(cols = 1:length(.), names_to = "code_muni", values_to = "id_ou")
  
  
  
  
  
  
  # ---- Base final ----
  # Por enquanto, utiliza homicidios_ds e Abordagem 2.
  
  base_final <- municipios %>% 
    st_drop_geometry() %>% 
    right_join(homicidios_ds %>% select(-municipio), by = "code_muni") %>% 
    mutate(pos2018 = ifelse(ano > 2018, 1, 0), 
           ano = factor(ano), .by = code_muni)
  
  # list(., perc_ti_atual %>% select(code_muni, perc_ti), perc_ou %>% select(code_muni, perc_ou)) %>% 
  # reduce(left_join, by = "code_muni") %>% 
  # mutate(ti = ifelse(!is.na(perc_ti), 1, 0),
  #        res_ou = ifelse(!is.na(perc_ou), 1, 0), 
  #        pos2018 = ifelse(ano > 2018, 1, 0),
  #        .by = code_muni) %>% 
  # mutate(ano = factor(ano))
  