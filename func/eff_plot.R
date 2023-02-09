effplot_multiple = function(df, use = "EOSV", focus = NULL, title = NULL){

  df = subset(df, int %in% c("no_int", use))
  df$int_name[df$int_name == "no_int"] = "Base model"
  df$int[df$int == "no_int"] = "Base model"
  df$int[df$int == "EOSV"] = "EOSV model"
  df$int[df$int == "OSV"] = "OSV model"

  df$actor = ifelse(grepl("_reb_", df$violence_var), "Rebel violence", "State violence")
  df$actor = ifelse(grepl("_all_", df$violence_var), "Any actor", df$actor)
  df$actor = factor(df$actor)
  df$actor = fct_relevel(df$actor, c("State violence", "Rebel violence"))#, "Any actor"))

  levels(df$dep_var_label) = c("Own ethnic group\ntreated unfairly", "Ethnic over\nnational ID")

  # Labels for legend
  df$int_name = gsub(" (rebel|state)-led", "", df$int_name)
  df$int_name = gsub("Previous ethnic targeting", "Collective\ntargeting\n", df$int_name)
  df$int_name = gsub("No previous ethnic targeting", "No collective\ntargeting\n", df$int_name)
  int_values = c(unique(df$int_name)[grepl("^No", unique(df$int_name))],
    unique(df$int_name)[grepl("^Coll", unique(df$int_name))])

  # Establish limits
  lwrlim = min(df$lwr)
  uprlim = max(df$upr)
  axis_breaks = seq(round(lwrlim, 2), uprlim, round((uprlim-lwrlim)/5, 2))

  p = ggplot(df, aes(x = dep_var_label, y = mean, color = int_name)) +
    geom_point(position = position_dodge(width = 1/3)) +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0,
      position = position_dodge(width = 1/3)) +
    geom_errorbar(aes(ymin = lwr90, ymax = upr90), width = 0, size = 1.1,
      position = position_dodge(width = 1/3)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    facet_grid(int~actor) +
    scale_y_continuous(breaks = axis_breaks) +
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
    scale_color_manual(breaks = int_values,
      values = c("#294b66", "#f53831", "#000000"))

  if(!is.null(title)){
    p = p + ggtitle(title)
  }

  return(p)

}

## ========================================================================
## Old versions

effplot = function(df){

  int = ifelse(unique(df$int) == "no_int", FALSE, TRUE)

  if(int){
    inttype = unique(df$int)
    p = ggplot(df, aes(x = dep_var_label, y = mean, group = int_name, color = int_name))
  } else if(!int){
    p = ggplot(df, aes(x = dep_var_label, y = mean))
  }

  p = p +
    geom_point(position = position_dodge(width = 1/3)) +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0,
      position = position_dodge(width = 1/3)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 6.5, size = 0.25, color = "grey20") +
    geom_vline(xintercept = 11.5, size = 0.25, color = "grey20") +
    geom_vline(xintercept = 16.5, size = 0.25, color = "grey20") +
    facet_wrap(~violence_var) +
    theme_bw() +
    theme(panel.background = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          strip.text = element_text(size = 10),
          strip.background = element_blank()) +
    coord_flip() +
    labs(x = "", y = "\nEffect of violence (nearby OSV events)")

  if(int){
    p = p +
      scale_color_manual(values = c("#254a66", "#ed160e"))
  }

  return(p)

}


effplot_vvar = function(df, use = "EOSV", focus = NULL){

  df = subset(df, int %in% c("no_int", use))
  df$int_name[df$int_name == "no_int"] = "No interaction model"
  df$int[df$int == "no_int"] = "No interaction model"
  df$int[df$int == "EOSV"] = "With interaction (EOSV)"
  df$int[df$int == "OSV"] = "With interaction (OSV)"

  # Labels for legend
  int_values = c(unique(df$int_name)[grepl("^No prev", unique(df$int_name))],
    unique(df$int_name)[grepl("^Prev", unique(df$int_name))])

  if(!is.null(focus)){df = subset(df, dep_var_g == focus)}

  p = ggplot(df, aes(x = dep_var_label, y = mean, color = int_name)) +
    geom_point(position = position_dodge(width = 1/3)) +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0,
      position = position_dodge(width = 1/3)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_wrap(~int) +
    theme_bw() +
    theme(panel.background = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          strip.text = element_text(size = 10),
          strip.background = element_blank()) +
    coord_flip() +
    labs(x = "", y = "\nEffect of violence (nearby OSV events)") +
    scale_color_manual(breaks = int_values,
      values = c("#000000", "#37668a", "#ed160e", "#000000", "#000000"))

  if(is.null(focus)){
  p = p +
    geom_vline(xintercept = 6.5, size = 0.25, color = "grey20") +
    geom_vline(xintercept = 11.5, size = 0.25, color = "grey20") +
    geom_vline(xintercept = 16.5, size = 0.25, color = "grey20")
  }


  return(p)

}
