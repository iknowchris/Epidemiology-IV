# Perform stepwise backwards regression by p-value
stepwise_backwards <- function(dataset, outcome,
                               glm_family="gaussian",
                               p_value_lim=0.1,
                               categorical_to_group=NA,
                               talkative=FALSE){
  library(utils)
  library(stringr)
  library(aod)
  
  glm_family <- eval(parse(text=paste("glm_family <- ",glm_family,"()", sep="")))
  
  vars_of_interest <- names(dataset)[! names(dataset) %in% c(outcome)]
  
  continue_stepwise <- TRUE
  
  dataset <- dataset[complete.cases(dataset),]
  
  while (continue_stepwise==TRUE){
    # Get p-values from the complete model
    # Full model with all variables
    model_formula <-  as.formula(paste(outcome, "~",
                                       paste(vars_of_interest, collapse = " + ",
                                             sep = "")))
    model_results <- glm(formula=model_formula, family=glm_family, data=dataset)
    
    if(talkative){
      print(summary(model_results))
    }
    
    # get p_values, need to keep the variables if all p-values < p_value_limit; otherwise drop the smallest
    p_values <- summary(model_results)$coef[,"Pr(>|z|)"]
    p_values <- p_values
    
    # Process to get p-values
    # 1) get Wald statistic for all categorical variables
    # 2) get p-value from all non-categorical variables
    # 3) order them
    # 4) remove biggest > p_value_lim
    # 5) repeat until no more variables have a p_value above p_value_lim
    
    # Determine Wald Statistic for all categorical variables in the model
    cats_still_in_model <- vars_of_interest[vars_of_interest %in% categorical_to_group]
    
    # eventually will be a listing of all p-values
    wald_test_statistics <- data.frame(m_coeff = character(0), w_stat=numeric(0))
    
    if(length(cats_still_in_model) > 0){
      
      for (i in 1:length(cats_still_in_model)){
        positions_for_var_i <- which(str_detect(names(p_values),
                                                cats_still_in_model[i]))
        wald_stat <- wald.test(b = coef(model_results),
                               Sigma = vcov(model_results),
                               Terms = positions_for_var_i)$result$chi2[3]
        wald_test_statistics <- rbind(wald_test_statistics,
                                      data.frame(m_coeff=cats_still_in_model[i],
                                                 w_stat=wald_stat))
        wald_stat <- NA
        positions_for_var_i <- NA
      }
    }
    
    # Determine P-values for all non-categorical variables in the model
    non_cat_variables_still_in_model <- vars_of_interest[!vars_of_interest %in% categorical_to_group]
    
    if(length(non_cat_variables_still_in_model) > 0){
      
      for (i in 1:length(non_cat_variables_still_in_model)){
        positions_for_var_i <- which(str_detect(names(p_values),
                                                non_cat_variables_still_in_model[i]))
        wald_test_statistics <- rbind(wald_test_statistics,
                                      data.frame(m_coeff=non_cat_variables_still_in_model[i],
                                                 w_stat=as.numeric(p_values[positions_for_var_i])))
        positions_for_var_i <- NA
      }
    }
    
    # Sort the p-values and Wald Statistics, so the biggest is in the first row    
    wald_test_statistics <- wald_test_statistics[order(wald_test_statistics$w_stat,
                                                       decreasing = TRUE),]
    small_p_values <- p_values[p_values > p_value_lim]
    
    # If the biggest p-value is bigger than the p_value_limit, then remove it from future models
    if (wald_test_statistics[1,2] > p_value_lim){
      if (talkative){
        print(paste("Variable to remove: ", wald_test_statistics[1,1],
                    " p-value: ", wald_test_statistics[1,2]))
      }
      vars_of_interest <- vars_of_interest[! vars_of_interest %in% wald_test_statistics[1,1]]
    } else {
      continue_stepwise <- FALSE  # All variables should remain in the model
    }      
    
    if (length(vars_of_interest) == 0){
      print("All variables removed from the model because the p-value threshold was too high.")
      continue_stepwise <- FALSE
    }
    
  }
  
  return(model_results)
}