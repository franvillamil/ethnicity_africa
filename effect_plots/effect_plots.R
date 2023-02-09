# setwd("~/Documents/Projects/osv_afrobarometer")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("ggplot2", "dplyr", "forcats", "stringr", "xtable")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())),
    repos = "http://cran.us.r-project.org")}
# Load
lapply(pkg, library, character.only = TRUE)

# ------------------------------

source("func/eff_plot.R")

# ------------------------------

minfo = bind_rows(
  read.csv("models/output/model_info.csv") %>% mutate(model = "glm"),
  read.csv("models_lm/output/model_info.csv") %>% mutate(model = "lm"),
  read.csv("models_1525/output/model_info_1525.csv") %>% mutate(model = "1525"))
meff = bind_rows(
  read.csv("models/output/effects_of_violence.csv") %>% mutate(model = "glm"),
  read.csv("models_lm/output/effects_of_violence.csv") %>% mutate(model = "lm"),
  read.csv("models_1525/output/effects_of_violence_1525.csv") %>% mutate(model = "1525"))

# ------------------------------
# Create summary tables for models

# Get actor, km, time
minfo$actor = "1State"
minfo$actor = ifelse(grepl("_all_", minfo$violence_var), "3Any actor", minfo$actor)
minfo$actor = ifelse(grepl("_reb_", minfo$violence_var), "2Rebel", minfo$actor)
minfo$time = "anytime"
minfo$time = ifelse(grepl("_2yr", minfo$violence_var), "1Previous 2 years", minfo$time)
minfo$time = ifelse(grepl("_5yr", minfo$violence_var), "1Previous 5 years", minfo$time)
minfo$time = ifelse(grepl("_bf2yr", minfo$violence_var), "2Before prev 2 years", minfo$time)
minfo$time = ifelse(grepl("_bf5yr", minfo$violence_var), "2Before prev 5 years", minfo$time)
minfo$dist = gsub(".*_((5|10|25)km)_.*", "\\1", minfo$violence_var)
minfo$EOSV_int = ifelse(grepl("EOSV", minfo$int), "Yes", "No")
minfo$dv = ifelse(grepl("ethnic_id", minfo$dep_var), "1Ethnic ID", "2Ethnic grievances")
minfo$AIC = format(round(minfo$AIC, 1), n.small = 1, big.mark = ",")
minfo$n = format(minfo$n, big.mark = ",")

# Remove "all" actor
minfo = subset(minfo, actor != "3Any actor")
meff = subset(meff, !grepl("_all", violence_var))
# Change DV if lm
# minfo$dv[minfo$model == "lm"] = paste(minfo$dv[minfo$model == "lm"], "(cont)")

# Years?
y_choice = "5 years"

## Model table for glm (main) models
model_table = minfo %>%
  filter(model == "glm", grepl(y_choice, time) & dist == "10km") %>%
  dplyr::select(dv, time, actor, EOSV_int, n, AIC, country_rounds) %>%
  arrange(dv, time, actor, EOSV_int) %>%
  mutate(
    dv = str_sub(dv, 2, -1L),
    actor = str_sub(actor, 2, -1L),
    time = str_sub(time, 2, -1L))

model_table_tex = paste0(
  c("& & \\multirow{2}{*}{State} & No",
    "& Previous & & Yes",
    "& 5 years & \\multirow{2}{*}{Rebel} & No",
    "Ethnic & & & Yes",
    "Identification & & \\multirow{2}{*}{State} & No",
    "& Before & & Yes",
    "& last 5 years & \\multirow{2}{*}{Rebel} & No",
    "& & & Yes",
    "& & \\multirow{2}{*}{State} & No",
    "& Previous & & Yes",
    "& 5 years & \\multirow{2}{*}{Rebel} & No",
    "Ethnic & & & Yes",
    "Grievances & & \\multirow{2}{*}{State} & No",
    "& Before & & Yes",
    "& last 5 years & \\multirow{2}{*}{Rebel} & No",
    "& & & Yes"),
  " & ", model_table$n,
  " & ", model_table$country_rounds,
  " & ", model_table$AIC,
  "\\\\")

model_table_tex = c(model_table_tex[1:2],
  "\\cline{3-7}", model_table_tex[3:4],
  "\\cline{2-7}", model_table_tex[5:6],
  "\\cline{3-7}", model_table_tex[7:8],
  "\\hline", model_table_tex[9:10],
  "\\cline{3-7}", model_table_tex[11:12],
  "\\cline{2-7}", model_table_tex[13:14],
  "\\cline{3-7}", model_table_tex[15:16])

filecon = file("effect_plots/output/tab_model_info.tex")
writeLines(model_table_tex, filecon)
close(filecon)

## Model table for lm (continuous DV) models
model_table_lm = minfo %>%
  filter(model == "lm", grepl(y_choice, time) & dist == "10km") %>%
  dplyr::select(dv, time, actor, EOSV_int, n, AIC, country_rounds) %>%
  arrange(dv, time, actor, EOSV_int) %>%
  mutate(
    dv = str_sub(dv, 2, -1L),
    actor = str_sub(actor, 2, -1L),
    time = str_sub(time, 2, -1L))

model_table_lm_tex = paste0(
  c("& & \\multirow{2}{*}{State} & No",
    "& Previous & & Yes",
    "& 5 years & \\multirow{2}{*}{Rebel} & No",
    "Ethnic & & & Yes",
    "Identification & & \\multirow{2}{*}{State} & No",
    "& Before & & Yes",
    "& last 5 years & \\multirow{2}{*}{Rebel} & No",
    "& & & Yes",
    "& & \\multirow{2}{*}{State} & No",
    "& Previous & & Yes",
    "& 5 years & \\multirow{2}{*}{Rebel} & No",
    "Ethnic & & & Yes",
    "Grievances & & \\multirow{2}{*}{State} & No",
    "& Before & & Yes",
    "& last 5 years & \\multirow{2}{*}{Rebel} & No",
    "& & & Yes"),
  " & ", model_table_lm$n,
  " & ", model_table_lm$country_rounds,
  " & ", model_table_lm$AIC,
  "\\\\")

model_table_lm_tex = c(model_table_lm_tex[1:2],
  "\\cline{3-7}", model_table_lm_tex[3:4],
  "\\cline{2-7}", model_table_lm_tex[5:6],
  "\\cline{3-7}", model_table_lm_tex[7:8],
  "\\hline", model_table_lm_tex[9:10],
  "\\cline{3-7}", model_table_lm_tex[11:12],
  "\\cline{2-7}", model_table_lm_tex[13:14],
  "\\cline{3-7}", model_table_lm_tex[15:16])

filecon = file("effect_plots/output/tab_model_info_lm.tex")
writeLines(model_table_lm_tex, filecon)
close(filecon)

model_table_1525 = minfo %>%
  filter(grepl("1525", violence_var) & dist == "10km") %>%
  select(dv, actor, EOSV_int, n, AIC, region_rounds) %>%
  arrange(dv, actor, EOSV_int) %>%
  mutate(
    dv = str_sub(dv, 2, -1L),
    actor = str_sub(actor, 2, -1L))

model_table_1525_tex = paste0(
  c("& \\multirow{2}{*}{State} & No",
    "Ethnic & & Yes",
    "Identification & \\multirow{2}{*}{Rebel} & No",
    "& & Yes",
    "& \\multirow{2}{*}{State} & No",
    "Ethnic & & Yes",
    "Grievances & \\multirow{2}{*}{Rebel} & No",
    "& & Yes"),
  " & ", model_table_1525$n,
  " & ", model_table_1525$region_rounds,
  " & ", model_table_1525$AIC,
  "\\\\")

model_table_1525_tex = c(model_table_1525_tex[1:2],
  "\\cline{2-6}", model_table_1525_tex[3:4],
  "\\hline", model_table_1525_tex[5:6],
  "\\cline{2-6}", model_table_1525_tex[7:8])

filecon = file("effect_plots/output/tab_model_info_1525.tex")
writeLines(model_table_1525_tex, filecon)
close(filecon)

# ------------------------------

# Recode names, create new label variables, etc
vio_eff = meff %>%
  mutate(vio_var_g = gsub("expo_(|reb_|all_)(.*)_bin", "\\2", violence_var)) %>%
  mutate(vio_var_g = ifelse(grepl("_ord", dep_var),
    paste0(vio_var_g, "_ord"), vio_var_g)) %>%
  mutate(dep_var_label = factor(recode(dep_var,
    "ethnic_id" = "Ethnic over national ID",
    "ethnic_fairtreat" = "Own ethnic group treated unfairly",
    "ethnic_id_ord" = "Ethnic over national ID",
    "ethnic_fairtreat_ord" = "Own ethnic group treated unfairly"))) %>%
  mutate(int_name = recode(int,
    "EOSV_state0" = "No previous ethnic targeting",
    "EOSV_state1" = "Previous state-led ethnic targeting",
    "EOSV_rebel0" = "No previous ethnic targeting",
    "EOSV_rebel1" = "Previous rebel-led ethnic targeting",
    "EOSV_any0" = "No previous ethnic targeting",
    "EOSV_any1" = "Previous ethnic targeting")) %>%
  mutate(int = recode(int,
    "EOSV_state0" = "EOSV",
    "EOSV_state1" = "EOSV",
    "EOSV_rebel0" = "EOSV",
    "EOSV_rebel1" = "EOSV",
    "EOSV_any0" = "EOSV",
    "EOSV_any1" = "EOSV"))

# Reorder dep var labels by groups
dv_names = c("Ethnic over national ID",
  "Own ethnic group treated unfairly")
vio_eff$dep_var_label = fct_relevel(vio_eff$dep_var_label, dv_names[length(dv_names):1])

# Remove extra years (otherwise doesn't work function)

eff_bandw = vio_eff %>%
  filter(grepl("expo_(1|2|3|4|5|6|7|8|9|10)yr_10km_bin", violence_var) &
    model == "glm") %>%
  mutate(time = gsub("expo_(1|2|3|4|5|6|7|8|9|10)yr_10km_bin", "\\1", violence_var)) %>%
  mutate(time_lab = paste0("Previous ", time, " years")) %>%
  mutate(time = as.integer(time))

vio_eff = vio_eff %>%
  filter(!grepl("(1|3|4|6|7|8|9|10)yr", violence_var))

# ------------------------------
# Plots

# Grid of options to plot
plots_f = data.frame(v = unique(vio_eff$vio_var_g))
plots_f$file = paste0("eff_", gsub("expo_(.*)_bin", "\\1", plots_f$v))

for(r in 1:nrow(plots_f)){

  fn = paste0("effect_plots/output/", plots_f$file[r], ".pdf")
  d = subset(vio_eff, vio_var_g == plots_f$v[r])
  p = effplot_multiple(df = d)#, title = plots_f$file[r])
  ggsave(filename = fn, height = 3.5, width = 8.5, plot = p)

}

# ------------------------------
# EXTRA / Plot with location (geo coords) FE

eff_ll = read.csv("models_1525/output/effects_of_violence_1525_location.csv") %>%
  mutate(vio_var_g = gsub("expo_(|reb_|all_)(.*)_bin", "\\2", violence_var)) %>%
  mutate(vio_var_g = ifelse(grepl("_ord", dep_var),
    paste0(vio_var_g, "_ord"), vio_var_g)) %>%
  mutate(dep_var_label = factor(recode(dep_var,
    "ethnic_id" = "Ethnic over national ID",
    "ethnic_fairtreat" = "Own ethnic group treated unfairly",
    "ethnic_id_ord" = "Ethnic over national ID",
    "ethnic_fairtreat_ord" = "Own ethnic group treated unfairly"))) %>%
  mutate(int_name = recode(int,
    "EOSV_state0" = "No previous ethnic targeting",
    "EOSV_state1" = "Previous state-led ethnic targeting",
    "EOSV_rebel0" = "No previous ethnic targeting",
    "EOSV_rebel1" = "Previous rebel-led ethnic targeting",
    "EOSV_any0" = "No previous ethnic targeting",
    "EOSV_any1" = "Previous ethnic targeting")) %>%
  mutate(int = recode(int,
    "EOSV_state0" = "EOSV",
    "EOSV_state1" = "EOSV",
    "EOSV_rebel0" = "EOSV",
    "EOSV_rebel1" = "EOSV",
    "EOSV_any0" = "EOSV",
    "EOSV_any1" = "EOSV"))

  # Labels for legend
  eff_ll$int_name[eff_ll$int_name == "no_int"] = "Base model"
  eff_ll$int[eff_ll$int == "no_int"] = "Base model"
  eff_ll$int[eff_ll$int == "EOSV"] = "EOSV model"
  eff_ll$int[eff_ll$int == "OSV"] = "OSV model"
  eff_ll$int_name = gsub(" (rebel|state)-led", "", eff_ll$int_name)
  eff_ll$int_name = gsub("Previous ethnic targeting", "Collective\ntargeting\n", eff_ll$int_name)
  eff_ll$int_name = gsub("No previous ethnic targeting", "No collective\ntargeting\n", eff_ll$int_name)
  int_values = c(unique(eff_ll$int_name)[grepl("^No", unique(eff_ll$int_name))],
    unique(eff_ll$int_name)[grepl("^Coll", unique(eff_ll$int_name))])

p = ggplot(eff_ll, aes(x = dep_var_label, y = mean, color = int_name)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_point(position = position_dodge(width = 1/3)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0,
    position = position_dodge(width = 1/3)) +
  geom_errorbar(aes(ymin = lwr90, ymax = upr90), width = 0, size = 1.1,
    position = position_dodge(width = 1/3)) +
  facet_grid(rows = vars(int)) +
  # scale_y_continuous(breaks = axis_breaks) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        legend.position = "right",
        legend.title = element_blank(),
        # panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        # strip.background = element_blank(),
        strip.background = element_rect(fill = "grey80", color = NA),
        strip.text = element_text(size = 10, face = "bold")) +
  coord_flip() +
  labs(x = "", y = "\nEffect of exposure to violence") +
  scale_color_manual(breaks = int_values, values = c("#294b66", "#f53831", "#000000"))

ggsave(filename = "effect_plots/output/eff_1525_location_10km.pdf",
  height = 3.5, width = 6, plot = p)


# # ------------------------------
# # EXTRA / Plot with time bandwidths
#
# p = ggplot(subset(eff_bandw, grepl("^Previous", int_name)),
#     aes(x = reorder(time_lab, desc(time)), y = mean)) +
#   geom_point(position = position_dodge(width = 1/3)) +
#   geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0,
#     position = position_dodge(width = 1/3)) +
#   geom_errorbar(aes(ymin = lwr90, ymax = upr90), width = 0, size = 1.1,
#     position = position_dodge(width = 1/3)) +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   facet_grid(~dep_var_label) +
#   # scale_y_continuous(breaks = axis_breaks) +
#   theme_minimal() +
#   theme(panel.background = element_blank(),
#         legend.position = "right",
#         legend.title = element_blank(),
#         # panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         # strip.background = element_blank(),
#         strip.background = element_rect(fill = "grey80", color = NA),
#         strip.text = element_text(size = 10, face = "bold")) +
#   coord_flip() +
#   labs(x = "Temporal window of exposure to violence\n",
#     y = "\nEffect of exposure to violence")
# ggsave("effect_plots/output/eff_bandwidth_eosv.pdf", height = 3.5, width = 7)
#
# p = ggplot(subset(eff_bandw, grepl("^No previous", int_name)),
#     aes(x = reorder(time_lab, desc(time)), y = mean)) +
#   geom_point(position = position_dodge(width = 1/3)) +
#   geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0,
#     position = position_dodge(width = 1/3)) +
#   geom_errorbar(aes(ymin = lwr90, ymax = upr90), width = 0, size = 1.1,
#     position = position_dodge(width = 1/3)) +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   facet_grid(~dep_var_label) +
#   # scale_y_continuous(breaks = axis_breaks) +
#   theme_minimal() +
#   theme(panel.background = element_blank(),
#         legend.position = "right",
#         legend.title = element_blank(),
#         # panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         # strip.background = element_blank(),
#         strip.background = element_rect(fill = "grey80", color = NA),
#         strip.text = element_text(size = 10, face = "bold")) +
#   coord_flip() +
#   labs(x = "Temporal window of exposure to violence\n",
#     y = "\nEffect of exposure to violence")
# ggsave("effect_plots/output/eff_bandwidth_no_eosv.pdf", height = 3.5, width = 7)
#
# p = ggplot(subset(eff_bandw, int == "EOSV"),
#     aes(x = reorder(time_lab, desc(time)), y = mean)) +
#   geom_point(position = position_dodge(width = 1/3)) +
#   geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0,
#     position = position_dodge(width = 1/3)) +
#   geom_errorbar(aes(ymin = lwr90, ymax = upr90), width = 0, size = 1.1,
#     position = position_dodge(width = 1/3)) +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   facet_grid(int_name~dep_var_label) +
#   # scale_y_continuous(breaks = axis_breaks) +
#   theme_minimal() +
#   theme(panel.background = element_blank(),
#         legend.position = "right",
#         legend.title = element_blank(),
#         # panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         # strip.background = element_blank(),
#         strip.background = element_rect(fill = "grey80", color = NA),
#         strip.text = element_text(size = 10, face = "bold")) +
#   coord_flip() +
#   labs(x = "Temporal window of exposure to violence\n",
#     y = "\nEffect of exposure to violence")
# ggsave("effect_plots/output/eff_bandwidth_eosv_full.pdf", height = 6, width = 7)
