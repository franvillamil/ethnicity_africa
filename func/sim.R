simulations = function(
  model,
  n_sim = 1000){

  # Model components
  f = as.character(formula(model))
  interaction = ifelse(grepl(" \\* ", f[3]), TRUE, FALSE)
  terms = str_split(f[3], " ")[[1]]
  coefs = model$coefficients
  outcome_var = f[2]
  violence_var = terms[grepl("expo_", terms)]
  if(!grepl("bin$", violence_var)){print("Watchout: violence variable is not binary")}
  if(interaction){interaction_var = terms[which(terms == "*")+1]}
  if(sum(terms == "*") > 1){stop("More than one interaction? Not supported")}
  terms = terms[!terms %in% c("*", "+")]

  ## COVARIATE MATRIX

  # Main vars
  mat = expand.grid(violence = as.numeric(0:1))
  if(interaction){
    mat = expand.grid(violence = as.numeric(0:1), interaction = as.numeric(0:1))
    mat$violence_x_interaction = mat$violence * mat$interaction
  }
  # Add further controls
  mat = cbind(mat, data.frame(sex = 0, urban = 0, econ_employment = 1,
    educ = 2, age = 35, dist_capital_km = 5))
  # Add intercept to X matrix
  mat$Intercept = 1
  names(mat)[names(mat) == "Intercept"] = "(Intercept)"
  # Add fixed effects (keep it at ref category)
  fe = names(coefs)[grepl("factor\\(", names(coefs))]
  mat = cbind(mat, matrix(0, ncol = length(fe), dimnames = list(NULL, fe)))
  # Change violence and eosv names
  names(mat) = gsub("violence", violence_var, names(mat))
  if(interaction){
    names(mat) = gsub("interaction", interaction_var, names(mat))
    names(mat) = gsub("_x_", ":", names(mat))
  }

  # Check that all variables are there and order them
  if(!all(names(coefs) %in% names(mat))){stop("Missing variables in X matrix")}
  mat = mat[,match(names(coefs), names(mat))]

  ## SIMULATE

  # VCOV matrix
  covmat = vcov(model)
  # Random draws of coefficients
  betadraw = mvrnorm(n = n_sim, mu = coefs, Sigma = covmat)

  # Labels for each scenario
  row_index = paste0(violence_var, mat[, violence_var])
  if(interaction){row_index = paste0(row_index, "/", interaction_var, mat[, interaction_var])}

  # Create list of point estimates and add them from sims
  estimates = vector("list", length(row_index))
  names(estimates) = row_index

  if("glm" %in% class(model)){

    for(i in 1:length(row_index)){
      name = row_index[i]
      estimates[[name]] = 1 / (1 + exp(-(betadraw %*% as.numeric(mat[i,]))))
    }

  } else {

    for(i in 1:length(row_index)){
      name = row_index[i]
      estimates[[name]] = betadraw %*% as.numeric(mat[i,])
    }

  }

  ## RETURN
  return(estimates)

}
