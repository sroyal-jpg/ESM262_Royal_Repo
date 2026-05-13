
# SIMPSON BIODIVERSITY INDEX FUNCTION

# CONTRACT
# inputs= number of individuals of each species (n), number of individuals of all species (N)
# output = biodiversity index
# parameters = (sum (n(n-1))) / (N((N-1)))

# SIMPSON BIODIVERSITY INDEX

#' @param species vector of species counts (n)
#' @param total number of individuals (N)
#' 
# function definition
biodiv_index = function(species, total) {
  
  result = sum(species * (species - 1)) / 
  (total * (total-1))
 
   return(result)
}

