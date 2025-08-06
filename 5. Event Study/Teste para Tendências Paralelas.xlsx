  
  ########################################################
  ################                        ################
  ################     Nome do Projeto    ################
  ################       xx/xx/20xx       ################
  ################                        ################
  ########################################################
  # usethis::edit_file("~/AppData/Roaming/RStudio/templates/default.R")
  
  {
    if (!require("pacman")) install.packages("pacman") else library(pacman)
    pacman::p_load(tidyverse, magrittr, usethis, extrafont,
                   readxl,
                   haven, estimatr, plm, lmtest, sandwich, stargazer, AER)
    
    extrafont::loadfonts(device = "win")
    options(timeout = max(1000, getOption("timeout")))
  }
  
  # -------------------------------------------------------
  
  # O dataset contém 200 unidades, que são observadas ao longo de 8 períodos.
  # O tratamento é dado pela variável 'Rebate', que ocorre a partir do 4º período. 
  # As unidades tratadas são aquelas que possuem valor '1' na coluna Rebate4 em diante.
  
  # Load the data
  data <- read_excel("E:/Projetos em R/15. Consultoria GPEQ UFRJ/2. Demandas/4. Homícidios, Terras Indígenas e Reservas de Ouro/EnergySavingTechAdoption2final.xlsx")
  
  # Reshape to long format
  long_data <- data %>%
    pivot_longer(
      cols = starts_with("Adoption") | starts_with("Rebate") | starts_with("Outage"),
      names_to = c(".value", "period"),
      names_pattern = "(Adoption|Rebate|Outage)(\\d)"
    ) %>% 
    mutate(period = as.integer(period))
  
  # Convert to a pdata.frame (panel data frame)
  pdata <- pdata.frame(long_data, index = c("Obs", "period"))
  
  # Run the two-way fixed effects model WITHOUT CONTROL
  fe_model <- plm(Adoption ~ Rebate, data = pdata, model = "within", effect = "twoways")
  
  # Obtain cluster-robust standard errors (clustered by individual)
  fe_model_robust_se <- coeftest(fe_model, vcov = vcovHC(fe_model, type = "HC1", cluster = "group"))
  
  # Display results
  print(fe_model_robust_se)
  
  
  # Run the two-way fixed effects model WITH CONTROL
  fe_model <- plm(Adoption ~ Rebate + Outage, data = pdata, model = "within", effect = "twoways")
  
  # Obtain cluster-robust standard errors (clustered by individual)
  fe_model_robust_se <- coeftest(fe_model, vcov = vcovHC(fe_model, type = "HC1", cluster = "group"))
  
  # Display results
  print(fe_model_robust_se)
  
  # Define a binary indicator for treated units (those that receive Rebate at any point)
  long_data <- long_data %>%
    group_by(Obs) %>%
    mutate(treated = ifelse(any(Rebate == 1), 1, 0)) %>%
    ungroup()
  
  # Calculate the mean Adoption for treated and control groups by period
  mean_adoption <- long_data %>%
    group_by(period, treated) %>%
    summarize(mean_adoption = mean(Adoption, na.rm = TRUE), .groups = 'drop')
  
  # Plot the trends
  library(ggplot2)
  
  ggplot(mean_adoption, aes(x = period, y = mean_adoption, color = factor(treated), group = treated)) +
    geom_line() +
    geom_point() +
    labs(x = "Period", y = "Mean Adoption", color = "Group",
         title = "Parallel Trends Check: Mean Adoption Over Time") +
    scale_color_manual(labels = c("Control", "Treated"), values = c("blue", "red")) +
    theme_minimal() +
    geom_vline(xintercept = 4, linetype = "dashed", color = "black") +
    annotate("text", x = 4.2, y = max(mean_adoption$mean_adoption), label = "Treatment Begins", hjust = 0)
  
  #THIS IS RELEVANT ONLY FOR DATASET 2FINAL: PARALLEL TRENDS CONDITIONAL ON CONTROL
  #WE RUN ADOPTION ON OUTAGE AND USE THE RESIDUALS: PART OF ADOPTION NOT EXPLAINED BY OUTAGE
  
  # Step 1: Regress Adoption on Outage to get residuals
  residuals_data <- long_data %>%
    group_by(period) %>%
    mutate(adoption_residuals = lm(Adoption ~ Outage)$residuals) %>%
    ungroup()
  
  # Step 2: Calculate mean residuals for treated and control groups
  mean_residuals <- residuals_data %>%
    group_by(period, treated) %>%
    summarize(mean_residual = mean(adoption_residuals, na.rm = TRUE), .groups = 'drop')
  
  # Step 3: Plot the mean residuals over time
  ggplot(mean_residuals, aes(x = period, y = mean_residual, color = factor(treated), group = treated)) +
    geom_line() +
    geom_point() +
    labs(x = "Period", y = "Mean Residuals (Conditional on Outage)", color = "Group",
         title = "Parallel Trends Check: Residualized Adoption Over Time") +
    scale_color_manual(labels = c("Control", "Treated"), values = c("blue", "red")) +
    theme_minimal() +
    geom_vline(xintercept = 4, linetype = "dashed", color = "black") +
    annotate("text", x = 4.2, y = max(mean_residuals$mean_residual, na.rm = TRUE),
             label = "Treatment Begins", hjust = 0)
  
  # Step 1: Create a treatment indicator that is 1 for treated units in all periods
  long_data <- long_data %>%
    group_by(Obs) %>%
    mutate(Rebate_dummy = ifelse(any(Rebate == 1), 1, 0)) %>%
    ungroup()
  
  # Step 2: Create time period dummies interacted with the Rebate_dummy
  long_data <- long_data %>%
    mutate(
      Rebate_time0 = ifelse(period == 0, Rebate_dummy, 0),
      Rebate_time1 = ifelse(period == 1, Rebate_dummy, 0),
      Rebate_time2 = ifelse(period == 2, Rebate_dummy, 0),
      # Exclude Rebate_time3 to use it as the reference period
      Rebate_time4 = ifelse(period == 4, Rebate_dummy, 0),  # Treatment period
      Rebate_time5 = ifelse(period == 5, Rebate_dummy, 0),
      Rebate_time6 = ifelse(period == 6, Rebate_dummy, 0),
      Rebate_time7 = ifelse(period == 7, Rebate_dummy, 0)   # Post-treatment periods
    )
  
  # Convert to a pdata.frame after creating the dummy variables
  pdata <- pdata.frame(long_data, index = c("Obs", "period"))
  
  # Step 3: Run the event study model, omitting Rebate_time3 as the reference period
  event_study_model <- plm(
    Adoption ~ Rebate_time0 + Rebate_time1 + Rebate_time2 +
      Rebate_time4 + Rebate_time5 + Rebate_time6 + Rebate_time7 + Outage,
    data = pdata, model = "within", effect = "twoways"
  )
  
  # Step 4: Obtain cluster-robust standard errors
  event_study_robust_se <- coeftest(event_study_model, vcov = vcovHC(event_study_model, type = "HC1", cluster = "group"))
  
  # Display results
  print(event_study_robust_se)
  
  # Step 5: Extract coefficients for plotting
  library(broom)
  coef_df <- tidy(event_study_robust_se) %>%
    filter(grepl("Rebate_time", term))
  
  # Plotting the coefficients to visualize parallel trends
  library(ggplot2)
  ggplot(coef_df, aes(x = as.numeric(gsub("Rebate_time", "", term)), y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error)) +
    labs(x = "Time Period Relative to Treatment", y = "Coefficient on Rebate",
         title = "Event Study for Parallel Trends Assumption") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    theme_minimal()