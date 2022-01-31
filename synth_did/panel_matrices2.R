panel_matrices2 = function(panel, unit = 1, time = 2, outcome = 3, treatment = 4) {
  # TODO: add support for covariates X, i.e. could keep all other columns
  keep = c(unit, time, outcome, treatment)
  if (!all(keep %in% 1:ncol(panel) | keep %in% colnames(panel))) {
    stop("Column identifiers should be either integer or column names in `panel`.")
  }
  index.to.name = function(x) { if(x %in% 1:ncol(panel)) { colnames(panel)[x] } else { x } }
  unit = index.to.name(unit)
  time = index.to.name(time)
  outcome = index.to.name(outcome)
  treatment = index.to.name(treatment)
  keep = c(unit, time, outcome, treatment)
  
  panel = panel[keep]
  
  # Convert potential factor/date columns to character
  panel = data.frame(
    lapply(panel, function(col) {if (is.factor(col) || inherits(col, "Date")) as.character(col) else col}), stringsAsFactors = FALSE
  )
  val <- as.vector(table(panel[, unit], panel[, time]))
  
  
  treated.units = unique(panel[panel[, treatment] == 1, unit])
  treated.unit = panel[, unit] %in% treated.units
  panel = panel[order(treated.unit, panel[, unit], panel[, time]), ]
  num.years = length(unique(panel[, time]))
  num.units = length(unique(panel[, unit]))
  Y = matrix(panel[,outcome], num.units, num.years, byrow = TRUE,
             dimnames = list(unique(panel[,unit]), unique(panel[,time])))
  W = matrix(panel[,treatment], num.units, num.years, byrow = TRUE,
             dimnames = list(unique(panel[,unit]), unique(panel[,time])))
  N0 = num.units - sum(W[, num.years])
  T0 = num.years - sum(W[num.units, ])
  
  list(Y = Y, N0 = N0, T0 = T0, W = W)
}
