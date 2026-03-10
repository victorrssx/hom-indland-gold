suppressPackageStartupMessages({
  pkgs <- c("fixest", "readxl", "dplyr", "stringr", "ggplot2", "readr")
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      install.packages(p, repos = "https://cloud.r-project.org")
    }
  }

  library(fixest)
  library(readxl)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(readr)
})

out_dir <- "outputs/paper_amazcerr_2010_t2016"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

base_file <- "outputs/base_final.rds"
biome_overlap_file <- "1. Dados Municipais/Lista_Municipio_Bioma_250mil.xls"

if (!file.exists(base_file)) {
  stop("Arquivo ausente: ", base_file, ". Rode Rscript run_quick.R antes.")
}
if (!file.exists(biome_overlap_file)) {
  stop("Arquivo ausente: ", biome_overlap_file)
}

base <- readRDS(base_file) %>%
  mutate(
    code_muni = as.character(code_muni),
    ano_num = as.integer(as.character(ano))
  )

overlap <- read_xls(biome_overlap_file) %>%
  transmute(
    code_muni = str_sub(as.character(CD_GEOCMU), 1, 6),
    biome = as.character(BIOMA)
  )

codes_target <- overlap %>%
  filter(biome %in% c("Amazônia", "Cerrado")) %>%
  distinct(code_muni)

d <- base %>%
  semi_join(codes_target, by = "code_muni") %>%
  filter(ano_num >= 2010) %>%
  mutate(
    ano = factor(ano_num),
    gold = as.integer(res_ou),
    ti = as.integer(ti),
    res_ti = as.integer(res_ou) * as.integer(ti),
    post2016 = as.integer(ano_num >= 2016),
    post2017 = as.integer(ano_num >= 2017),
    w_pop = as.numeric(pop),
    w_pop = ifelse(is.na(w_pop) | w_pop <= 0, NA_real_, w_pop)
  )

# Main specifications
m2016 <- feols(
  tx_hom_tot ~ gold:post2016 + ti:post2016 + gold:ti:post2016 | code_muni + ano,
  data = d
)

m2017 <- feols(
  tx_hom_tot ~ gold:post2017 + ti:post2017 + gold:ti:post2017 | code_muni + ano,
  data = d
)

m2016_sy <- feols(
  tx_hom_tot ~ gold:post2016 + ti:post2016 + gold:ti:post2016 | code_muni + abbrev_state^ano,
  data = d
)

# Event-study models
m_dyn <- feols(
  tx_hom_tot ~ i(ano_num, gold, ref = 2015) + i(ano_num, ti, ref = 2015) + i(ano_num, res_ti, ref = 2015) | code_muni + ano,
  data = d,
  vcov = ~code_muni
)

m_dyn_sy <- feols(
  tx_hom_tot ~ i(ano_num, gold, ref = 2015) + i(ano_num, ti, ref = 2015) + i(ano_num, res_ti, ref = 2015) | code_muni + abbrev_state^ano,
  data = d,
  vcov = ~code_muni
)

extract_event <- function(model) {
  ct <- as.data.frame(coeftable(model))
  ct$term <- rownames(ct)
  rownames(ct) <- NULL
  es <- ct[grepl("res_ti", ct$term), c("term", "Estimate", "Std. Error", "Pr(>|t|)")]
  colnames(es) <- c("term", "estimate", "std_error", "p_value")
  es$year <- as.integer(sub("ano_num::([0-9]{4}):res_ti", "\\1", es$term))
  es <- es[order(es$year), c("year", "estimate", "std_error", "p_value")]
  es$ci_low <- es$estimate - 1.96 * es$std_error
  es$ci_high <- es$estimate + 1.96 * es$std_error
  es
}

es <- extract_event(m_dyn)
es_sy <- extract_event(m_dyn_sy)
write.csv(es, file.path(out_dir, "event_study_ddd.csv"), row.names = FALSE)
write.csv(es_sy, file.path(out_dir, "event_study_ddd_stateyear.csv"), row.names = FALSE)

p <- ggplot(es, aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4, color = "gray35") +
  geom_vline(xintercept = 2016, linetype = "dotted", linewidth = 0.4, color = "gray35") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.12, linewidth = 0.4) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = sort(unique(es$year))) +
  labs(
    x = "Year",
    y = "DDD coefficient (Gold x TI x year)",
    title = "Dynamic DDD in Amazonia+Cerrado municipalities",
    subtitle = "Any-overlap biome definition; 2010-2022 sample; reference year 2015"
  ) +
  theme_minimal(base_size = 10)

ggsave(file.path(out_dir, "fig_event_study_ddd.pdf"), p, width = 7.0, height = 4.2)
ggsave(file.path(out_dir, "fig_event_study_ddd.png"), p, width = 7.0, height = 4.2, dpi = 320)

get_coef <- function(model, term) {
  ct <- as.data.frame(coeftable(model, vcov = ~code_muni))
  ct$term <- rownames(ct)
  rownames(ct) <- NULL
  row <- ct[ct$term == term, c("Estimate", "Std. Error", "Pr(>|t|)")]
  c(
    estimate = as.numeric(row$Estimate),
    se = as.numeric(row$`Std. Error`),
    p = as.numeric(row$`Pr(>|t|)`)
  )
}

# DD slices (post2016)
m_ti1 <- feols(tx_hom_tot ~ gold:post2016 | code_muni + ano, data = d[d$ti == 1, ])
m_ti0 <- feols(tx_hom_tot ~ gold:post2016 | code_muni + ano, data = d[d$ti == 0, ])
m_g1 <- feols(tx_hom_tot ~ ti:post2016 | code_muni + ano, data = d[d$gold == 1, ])
m_g0 <- feols(tx_hom_tot ~ ti:post2016 | code_muni + ano, data = d[d$gold == 0, ])

r_ddd_2016 <- get_coef(m2016, "gold:post2016:ti")
r_ddd_2017 <- get_coef(m2017, "gold:post2017:ti")
r_ddd_sy <- get_coef(m2016_sy, "gold:post2016:ti")
r_ti1 <- get_coef(m_ti1, "gold:post2016")
r_ti0 <- get_coef(m_ti0, "gold:post2016")
r_g1 <- get_coef(m_g1, "ti:post2016")
r_g0 <- get_coef(m_g0, "ti:post2016")

# WLS models (post2016)
m_wls <- feols(
  tx_hom_tot ~ gold:post2016 + ti:post2016 + gold:ti:post2016 | code_muni + ano,
  data = d,
  weights = ~w_pop
)
m_wls_sy <- feols(
  tx_hom_tot ~ gold:post2016 + ti:post2016 + gold:ti:post2016 | code_muni + abbrev_state^ano,
  data = d,
  weights = ~w_pop
)
r_wls <- get_coef(m_wls, "gold:post2016:ti")
r_wls_sy <- get_coef(m_wls_sy, "gold:post2016:ti")

# Placebo treatment years in pre-period (<=2015)
d_pre <- d[d$ano_num <= 2015, ]
d_pre$fake_post2013 <- as.integer(d_pre$ano_num >= 2013)
d_pre$fake_post2014 <- as.integer(d_pre$ano_num >= 2014)

m_pl2013 <- feols(
  tx_hom_tot ~ gold:fake_post2013 + ti:fake_post2013 + gold:ti:fake_post2013 | code_muni + abbrev_state^ano,
  data = d_pre
)
m_pl2014 <- feols(
  tx_hom_tot ~ gold:fake_post2014 + ti:fake_post2014 + gold:ti:fake_post2014 | code_muni + abbrev_state^ano,
  data = d_pre
)
r_pl2013 <- get_coef(m_pl2013, "gold:fake_post2013:ti")
r_pl2014 <- get_coef(m_pl2014, "gold:fake_post2014:ti")

# Save summary CSVs
dd_slices <- data.frame(
  model = c(
    "DD within TI=1 (Gold x Post2016)",
    "DD within TI=0 (Gold x Post2016)",
    "DD within Gold=1 (TI x Post2016)",
    "DD within Gold=0 (TI x Post2016)",
    "DDD main (Gold x TI x Post2016)"
  ),
  estimate = c(r_ti1["estimate"], r_ti0["estimate"], r_g1["estimate"], r_g0["estimate"], r_ddd_2016["estimate"]),
  std_error = c(r_ti1["se"], r_ti0["se"], r_g1["se"], r_g0["se"], r_ddd_2016["se"]),
  p_value = c(r_ti1["p"], r_ti0["p"], r_g1["p"], r_g0["p"], r_ddd_2016["p"]),
  n_obs = c(nobs(m_ti1), nobs(m_ti0), nobs(m_g1), nobs(m_g0), nobs(m2016))
)
write.csv(dd_slices, file.path(out_dir, "dd_slices_summary.csv"), row.names = FALSE)

checks <- data.frame(
  check = c(
    "Main DDD (Post2016, muni+year FE)",
    "Robustness DDD (Post2017, muni+year FE)",
    "Main DDD (Post2016, muni+state-year FE)",
    "Population-weighted DDD (Post2016, muni+year FE)",
    "Population-weighted DDD (Post2016, muni+state-year FE)",
    "Placebo fake post>=2013 (pre-period only, muni+state-year FE)",
    "Placebo fake post>=2014 (pre-period only, muni+state-year FE)"
  ),
  estimate = c(
    r_ddd_2016["estimate"], r_ddd_2017["estimate"], r_ddd_sy["estimate"],
    r_wls["estimate"], r_wls_sy["estimate"], r_pl2013["estimate"], r_pl2014["estimate"]
  ),
  std_error = c(
    r_ddd_2016["se"], r_ddd_2017["se"], r_ddd_sy["se"],
    r_wls["se"], r_wls_sy["se"], r_pl2013["se"], r_pl2014["se"]
  ),
  p_value = c(
    r_ddd_2016["p"], r_ddd_2017["p"], r_ddd_sy["p"],
    r_wls["p"], r_wls_sy["p"], r_pl2013["p"], r_pl2014["p"]
  )
)
write.csv(checks, file.path(out_dir, "additional_checks_summary.csv"), row.names = FALSE)

# Main table via etable (full) and compact LaTeX table
etable(
  m2016, m2017, m2016_sy,
  vcov = ~code_muni,
  tex = TRUE,
  file = file.path(out_dir, "table_main_full.tex"),
  headers = c("Main: Post>=2016", "Robustness: Post>=2017", "State-year FE: Post>=2016"),
  digits = 3,
  fitstat = c("n", "r2", "wr2"),
  title = "DDD estimates in Amazonia+Cerrado municipalities (2010-2022)"
)

fmt_num <- function(x) sprintf("%.3f", x)
fmt_sig <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", ""))))
}

get_term_info <- function(model, terms) {
  ct <- as.data.frame(coeftable(model, vcov = ~code_muni))
  ct$term <- rownames(ct)
  rownames(ct) <- NULL
  idx <- match(terms, ct$term)
  idx <- idx[!is.na(idx)]
  if (length(idx) == 0) stop("Termo nao encontrado: ", paste(terms, collapse = " ou "))
  row <- ct[idx[1], ]
  list(
    estimate = as.numeric(row$Estimate),
    se = as.numeric(row$`Std. Error`),
    p = as.numeric(row$`Pr(>|t|)`)
  )
}

gold_2016 <- get_term_info(m2016, c("gold:post2016"))
gold_2017 <- get_term_info(m2017, c("gold:post2017"))
gold_sy <- get_term_info(m2016_sy, c("gold:post2016"))
ti_2016 <- get_term_info(m2016, c("ti:post2016", "post2016:ti"))
ti_2017 <- get_term_info(m2017, c("ti:post2017", "post2017:ti"))
ti_sy <- get_term_info(m2016_sy, c("ti:post2016", "post2016:ti"))

line_coef <- function(label, vals) {
  paste0(label, " & ", paste(vals, collapse = " & "), " \\\\")
}

main_rows <- c(
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "& \\multicolumn{3}{c}{Dependent variable: Homicide rate per 100,000} \\\\",
  "\\cmidrule(lr){2-4}",
  "& Post>=2016 & Post>=2017 & Post>=2016 + state-year FE \\\\",
  "& (1) & (2) & (3) \\\\",
  "\\midrule",
  line_coef("Gold $\\times$ Post", c(
    paste0(fmt_num(gold_2016$estimate), fmt_sig(gold_2016$p)),
    paste0(fmt_num(gold_2017$estimate), fmt_sig(gold_2017$p)),
    paste0(fmt_num(gold_sy$estimate), fmt_sig(gold_sy$p))
  )),
  line_coef("", c(
    paste0("(", fmt_num(gold_2016$se), ")"),
    paste0("(", fmt_num(gold_2017$se), ")"),
    paste0("(", fmt_num(gold_sy$se), ")")
  )),
  line_coef("TI $\\times$ Post", c(
    paste0(fmt_num(ti_2016$estimate), fmt_sig(ti_2016$p)),
    paste0(fmt_num(ti_2017$estimate), fmt_sig(ti_2017$p)),
    paste0(fmt_num(ti_sy$estimate), fmt_sig(ti_sy$p))
  )),
  line_coef("", c(
    paste0("(", fmt_num(ti_2016$se), ")"),
    paste0("(", fmt_num(ti_2017$se), ")"),
    paste0("(", fmt_num(ti_sy$se), ")")
  )),
  line_coef("Gold $\\times$ TI $\\times$ Post", c(
    paste0(fmt_num(r_ddd_2016["estimate"]), fmt_sig(r_ddd_2016["p"])),
    paste0(fmt_num(r_ddd_2017["estimate"]), fmt_sig(r_ddd_2017["p"])),
    paste0(fmt_num(r_ddd_sy["estimate"]), fmt_sig(r_ddd_sy["p"]))
  )),
  line_coef("", c(
    paste0("(", fmt_num(r_ddd_2016["se"]), ")"),
    paste0("(", fmt_num(r_ddd_2017["se"]), ")"),
    paste0("(", fmt_num(r_ddd_sy["se"]), ")")
  )),
  "\\midrule",
  line_coef("Municipality FE", c("Yes", "Yes", "Yes")),
  line_coef("Year FE", c("Yes", "Yes", "No")),
  line_coef("State-year FE", c("No", "No", "Yes")),
  line_coef("Observations", c(as.character(nobs(m2016)), as.character(nobs(m2017)), as.character(nobs(m2016_sy)))),
  line_coef("Within R\\textsuperscript{2}", c(fmt_num(r2(m2016, "wr2")), fmt_num(r2(m2017, "wr2")), fmt_num(r2(m2016_sy, "wr2")))),
  "\\midrule",
  "\\multicolumn{4}{l}{\\footnotesize Clustered standard errors by municipality in parentheses.} \\\\",
  "\\multicolumn{4}{l}{\\footnotesize *** p<0.01, ** p<0.05, * p<0.1.} \\\\",
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(main_rows, file.path(out_dir, "table_main_compact.tex"))

checks_rows <- c(
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "Check & Estimate & Std. Err. & p-value \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(checks))) {
  checks_rows <- c(
    checks_rows,
    paste0(gsub("_", "\\\\_", checks$check[i]), " & ", fmt_num(checks$estimate[i]), " & ", fmt_num(checks$std_error[i]), " & ", fmt_num(checks$p_value[i]), " \\\\")
  )
}
checks_rows <- c(
  checks_rows,
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(checks_rows, file.path(out_dir, "table_additional_checks.tex"))

dd_rows <- c(
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "Model & Estimate & Std. Err. & p-value \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(dd_slices))) {
  dd_rows <- c(
    dd_rows,
    paste0(gsub("_", "\\\\_", dd_slices$model[i]), " & ", fmt_num(dd_slices$estimate[i]), " & ", fmt_num(dd_slices$std_error[i]), " & ", fmt_num(dd_slices$p_value[i]), " \\\\")
  )
}
dd_rows <- c(dd_rows, "\\bottomrule", "\\end{tabular}")
writeLines(dd_rows, file.path(out_dir, "table_dd_slices.tex"))

# Pre-trend tests
test_pre <- wald(m_dyn, "ano_num::201[0-4]:res_ti")
test_pre_sy <- wald(m_dyn_sy, "ano_num::201[0-4]:res_ti")

# Save concise report
sink(file.path(out_dir, "summary.txt"))
cat("Amazonia+Cerrado (any overlap) sample, 2010-2022\n")
cat("Municipalities:", dplyr::n_distinct(d$code_muni), "\n")
cat("Observations:", nrow(d), "\n\n")
cat("Main DDD (Post>=2016):\n")
print(coeftable(m2016, vcov = ~code_muni))
cat("\nRobustness DDD (Post>=2017):\n")
print(coeftable(m2017, vcov = ~code_muni))
cat("\nState-year FE (Post>=2016):\n")
print(coeftable(m2016_sy, vcov = ~code_muni))
cat("\nDD slices:\n")
print(dd_slices)
cat("\nAdditional checks:\n")
print(checks)
cat("\nPre-trend test baseline (2010-2014 joint):\n")
print(test_pre)
cat("\nPre-trend test state-year FE (2010-2014 joint):\n")
print(test_pre_sy)
sink()

cat("Done. Outputs in ", out_dir, "\n", sep = "")
