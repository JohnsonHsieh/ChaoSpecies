SpeciesAbundance <-
function(data, method = c("all", "Homogeneous", "Chao", "CE", "Jackknife"), k = 10, conf = 0.95){
  method <- match.arg(method)
  return(list(BASIC.DATA = basicAbun(data, k)[[1]], Rare.Species.Group = RareSpeciesGroup(data, k), 
              SPECIES.TABLE = SpecAbunOut(data, method, k, conf)))
}
