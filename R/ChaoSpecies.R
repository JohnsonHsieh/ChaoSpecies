ChaoSpecies <-
function(data, datatype = c("abundance", "incidence"), k = 10, conf = 0.95){
  if (k != round(k) || k < 0) 
    stop("Error: The cutoff t to define less abundant species must be non-negative integer!")
  if (is.numeric(conf) == FALSE || conf > 1 || conf < 0) 
    stop("Error: confidence level must be a numerical value between 0 and 1, e.g. 0.95")
  data <- as.numeric(round(data))
  
  if (datatype == "abundance"){
      f <- function(i, data){length(data[which(data == i)])}
      if (f(1, data) == sum(data)){
        stop("Error: The information of data is not enough.")}
    SpeciesAbundance(data, method = "all", k = k, conf = conf)
    } else {
      dat <- data[-1]; Q <- function(i, data){length(data[which(data == i)])}
      if (Q(1, dat) == sum(dat)){
        stop("Error: The information of data is not enough.")}
    SpeciesIncidence(data, method = "all", k = k, conf = conf)  
    }
}
