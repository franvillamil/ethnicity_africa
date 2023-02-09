violence_effects = function(
  dep_var,
  violence_var,
  estimates,
  confidence = 0.95){

  # Basic info
  scenarios = names(estimates)
  if(any(grepl("/", scenarios))){
    interaction = TRUE
    interaction_var = str_sub(str_split(scenarios[1], "/")[[1]][2], 1, -2L)
  } else {interaction = FALSE}

  # Names of each combination
  if(interaction){
    vio1_int0 = gsub("violence", violence_var, "violence1/int0")
    vio1_int0 = gsub("int", interaction_var, vio1_int0)
    vio1_int1 = gsub("violence", violence_var, "violence1/int1")
    vio1_int1 = gsub("int", interaction_var, vio1_int1)
    vio0_int0 = gsub("violence", violence_var, "violence0/int0")
    vio0_int0 = gsub("int", interaction_var, vio0_int0)
    vio0_int1 = gsub("violence", violence_var, "violence0/int1")
    vio0_int1 = gsub("int", interaction_var, vio0_int1)
  } else {
    vio1 = gsub("violence", violence_var, "violence1")
    vio0 = gsub("violence", violence_var, "violence0")
  }

  # Effect list
  if(interaction){
    violence_effect = list(
      estimates[[vio1_int0]] - estimates[[vio0_int0]],
      estimates[[vio1_int1]] - estimates[[vio0_int1]])
    names(violence_effect) = c(paste0(interaction_var, "0"), paste0(interaction_var, "1"))
  } else {
    violence_effect = list(
      estimates[[vio1]] - estimates[[vio0]])
      names(violence_effect) = "no_int"
  }

  # Add scenario name if specified
  # if(!is.null(scenario)){
  #   names(violence_effect) = paste(names(violence_effect), scenario, sep = "_")}

  # Select confidence level
  if(confidence == 0.95){
    confidence_lwr = 0.025
    confidence_upr = 0.975
  } else if (confidence == 0.9){
    confidence_lwr = 0.05
    confidence_upr = 0.95
  } else if (confidence == 0.99){
    confidence_lwr = 0.005
    confidence_upr = 0.995
  } else {stop("Confidence level??")}

  # Get mean and 95% intervals
  effects = data.frame(
    dep_var = rep(dep_var, length(names(violence_effect))),
    violence_var = rep(violence_var, length(names(violence_effect))),
    int = names(violence_effect),
    mean = map_dbl(violence_effect, mean),
    lwr = map_dbl(violence_effect, quantile, confidence_lwr),
    upr = map_dbl(violence_effect, quantile, confidence_upr),
    lwr90 = map_dbl(violence_effect, quantile, 0.05),
    upr90 = map_dbl(violence_effect, quantile, 0.95))

  # Simplify row names
  rownames(effects) = as.character(1:nrow(effects))

  # Return
  return(effects)

}
