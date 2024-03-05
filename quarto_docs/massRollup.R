# Reusable wrapper functions for oml-vision
library(tidyverse)
library(jsonlite)
library(igraph)
library(jsonlite)
library(networkD3)

###############################################################################
massRollUp <- function(g, root, df_mass){
  
  # Analysis: Mass Rollup By Depth-First Traversal
  order <- dfs(g, V(g)[root], order.out = TRUE)$order.out
  
  for (v in order){
    # print(V(g)[v])
    children <- neighbors(g, v, mode="out")
    if( length(children) > 0) {
      # add mass
      mass <- sum(df_mass[is.element(df_mass$c1_localname, names(children)), "c1_mass"])
      df_mass[df_mass$c1_localname == names(V(g)[v]), "c1_mass"] <- mass
    }
  }
  
  return(df_mass)
}
