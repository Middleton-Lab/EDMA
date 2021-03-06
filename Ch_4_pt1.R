library(tidyverse)

dist

form <- function(M) {
  d <- dist(M)
  fm <- matrix(NA, nrow = nrow(M), ncol = nrow(M))
  fm[lower.tri(fm)] <- d
  diag(fm) <- 0
  row.names(fm) <- row.names(M)
  colnames(fm) <- row.names(M)
  return(fm)
}

form_diff_arith <- function(FM_B, FM_A) {
  AFDM <- FM_B - FM_A
  return(AFDM)
}

form_diff_rel <- function(FM_B, FM_A) {
  # Check for same order of rows
  
  FDM <- FM_B / FM_A
  FDM <- as.matrix(FDM[lower.tri(FDM)], ncol = 1)
  row_names <- t(combn(row.names(FM_B), 2)) %>% 
    as.data.frame() %>% 
    unite(col = "LM_Pair")
  row.names(FDM) <- row_names$LM_Pair

  return(FDM)
}

form_size <- function(FM) {
  LT <- as.numeric(FM[lower.tri(FM)])
  S <- prod(LT) ^ (1 / length(LT))
  return(S)
}

shape_matrix <- function(M) {
  FM <- form(M)
  SM <- FM / form_size(FM)
  return(SM)
}

##############################################################################

A <- matrix(c(0, 1, 0, 0, 0, 1), ncol = 2)
B <- matrix(c(0, 2, 0, 0, 0, 1), ncol = 2)

row.names(A) <- paste0("LM", 1:nrow(A))
row.names(B) <- paste0("LM", 1:nrow(B))

FM_A <- form(A)
FM_B <- form(B)

form_diff_arith(FM_B, FM_A)
form_diff_rel(FM_B, FM_A)

shape_matrix(A)
shape_matrix(B)

# Size difference
form_size(form(B)) / form_size(form(A))

# Form difference matrix
form_diff_arith(shape_matrix(B), shape_matrix(A))
