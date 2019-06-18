replaceNaWithNamedFactor = function(factor_col, name) {
  factor_levels = levels(factor_col)
  factor_levels[length(factor_levels) + 1] = name
  
  new_factor_col = factor(factor_col, levels = factor_levels)
  new_factor_col[is.na(new_factor_col)] = name
  return(new_factor_col)
}