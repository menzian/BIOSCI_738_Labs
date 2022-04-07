#My function to extract differences, SED, LSDs, Lower 95% CIs, upper 95% CIs, t-statistics and p-values from predictmeans output
anas_awesome_function <- function(pm){
   ## treatment names
  nms <- colnames(pm$`Pairwise LSDs`) 
  ## comparison names
  c_names <- outer(nms, nms, function(x, y) paste(x, "-", y, sep = ""))
  ## unique comparison names
  names <- c_names[upper.tri(c_names)]
  
  ## Extracting Differences in means
  ## extract bit of predictmean output
  LSD <- pm$`Pairwise LSDs`
  #code to extract correct diagonal of LSD matrix (upper or lower)
  diffs <- t(LSD)[lower.tri(t(LSD))] 
  
  ## Extracting Standard Error of the Difference (SED)
  SED <- pm$`Standard Error of Differences`
  
  ## Extracting Least Significant Difference (LSD)
  ## extract bit of predictmean output
  LSD <- pm$`Pairwise LSDs`
  #code to extract correct diagonal of LSD matrix (upper or lower)
  LSD_from_matrix <- t(LSD)[upper.tri(t(LSD))]
  
  #Calculating Lower CI for multiple comparisons
  ## extract bit of predictmean output
  LSD <- pm$`Pairwise LSDs`
  #Calculate lower CI by subtracting LSDs from differences
  lower_CI <- diffs-LSD_from_matrix
  
  # Calculating Upper CI for multiple comparisons
  ## extract bit of predictmean output
  LSD <- pm$`Pairwise LSDs`
  #Calculate upper CI by adding LSDs to differences
  upper_CI <- diffs+LSD_from_matrix
  
  #Extracting t statistic
   ## extract bit of predictmean output
  t <- pm$`Pairwise p-value`
  #code to extract correct diagonal of LSD matrix (upper or lower)
  t_val <- t(t)[lower.tri(t(LSD))]
  
  #Extracting p statistic
  ## extract bit of predictmean output 
  t <- pm$`Pairwise p-value`
  #code to extract correct diagonal of LSD matrix (upper or lower)
  p_val <- t(t)[upper.tri(t(LSD))]
  

  ## create dataframe with needed elements
  results <- data.frame(Differences = diffs, SED =SED, LSDs=LSD_from_matrix, Lower95CI = lower_CI, Upper95CI = upper_CI, tstatistic = t_val, pvalue=p_val)
  ## append comparison names as rownames
  rownames(results) <- names
  ## return data frame
  return(results) 
}
