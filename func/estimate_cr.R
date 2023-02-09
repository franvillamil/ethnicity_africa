estimate_cr = function(
  country_name,
  round,
  dep_var,
  violence_var,
  interaction = NULL,
  control_vars = c("urban", "sex", "econ_employment", "age", "dist_capital_km")
  ){

  # Limit dataset to country
  df = data[data$country_name == country_name & data$round == round,]

  # Remove control variables not found
  for(cv in control_vars){
    if(length(unique(df[, cv])) <= 1){control_vars = control_vars[control_vars != cv]}
  }

  # FIRST BARRIER - data on that round?
  if(nrow(df) > 0){

    # SECOND BARRIER - outcome & exposure variation?
    dep_var_variation = length(unique(df[, dep_var])) > 1
    vio_var_variation = prop.table(table(df[, violence_var]))[1] <= 0.97
    vio_var_variation = as.logical(coalesce(vio_var_variation, FALSE)) # just in case it's NA
    if(dep_var_variation & vio_var_variation){

      # If NO interaction
      if(is.null(interaction)){

        # Formula
        f = paste0(dep_var, " ~ ", paste(c(violence_var, control_vars), collapse = " + "))
        # Estimate
        m = glm(f, data = df, family = "binomial")

      } else { # if INTERACTION

        # THIRD BARRIER - interaction variation
        int_variation = length(unique(df[, interaction])) > 1
        int_variation = coalesce(int_variation, FALSE) # just in case it's NA
        if(int_variation){

          # Formula
          f = paste0(dep_var, " ~ ",
            paste(c(paste0(violence_var, " * ", interaction), control_vars), collapse = " + "))
          # Estimate
          m = glm(f, data = df, family = "binomial")

        } else { m = NULL }

      }

    } else { m = NULL }

  } else { m = NULL }

  # If no coefficient for the interaction, put to null
  if(!is.null(interaction) & !is.null(m)){
    coef_int_i = which(grepl(paste0(":", interaction), names(m$coefficients)))
    coef_int = m$coefficients[coef_int_i]
    if(is.na(coef_int)){ m = NULL }
  }

  # Return model
  return(m)

}
