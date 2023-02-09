estimate = function(
  dep_var,
  violence_var,
  interaction = NULL,
  control_vars = c("urban", "sex", "econ_employment", "age", "dist_capital_km"),
  fe = "country_round",
  model = "glm"){

  # Limit dataset to country-rounds with variation in exposure
  t = table(data[, fe], data[, violence_var])
  # no_violence_variation = names(t[, 2])[t[, 2] == 0]
  no_violence_variation = names(t[,2])[prop.table(t, 1)[,2] < 0.03]
  df = data[!data[, fe] %in% no_violence_variation, ]

  # Create formula
  if(is.null(interaction)){

    f = as.formula(paste0(dep_var, "~",
      paste(c(violence_var, control_vars, paste0("factor(", fe, ")")), collapse = "+")))

  } else {

    f = as.formula(paste0(dep_var, "~",
      paste(c(paste0(violence_var, "*", interaction),
        control_vars, paste0("factor(", fe, ")")), collapse = "+")))

    t_int = table(data[, fe], data[, interaction])
    no_variation_int = names(t_int[, 1])[t_int[,2] == 0 | t_int[,1] == 0]
    df = df[!df[, fe] %in% no_variation_int, ]

  }

  # if(nrow(df) > 0){

  # Estimate
  if(model == "glm"){
    m = glm(f, data = df, family = "binomial")
  }else if(model == "lm"){
    m = lm(f, data = df)
  }

  # } else {m = NULL}

  # Return model
  return(m)

}
