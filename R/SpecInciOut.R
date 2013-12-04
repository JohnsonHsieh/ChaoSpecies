SpecInciOut <-
function(data, method = c("all", "Homogeneous", "Chao", "CE", "Jackknife"), k, conf){
  if (method == "all") {
    a <- SpecInciHomo(data, k, conf)
    b <- SpecInciChao2(data, k, conf)
    c <- SpecInciChao2bc(data, k, conf)
    d <- SpecInciModelh(data, k, conf)[[1]]
    e <- SpecInciModelh1(data, k, conf)[[1]]
    h <- SpecInciJack1(data, k, conf)
    i <- SpecInciJack2(data, k, conf)
    est.cv <- matrix(c("", "", "", SpecInciModelh(data, k, conf)[[2]], SpecInciModelh1(data, k, conf)[[2]], "", ""),
                     ncol = 1)
    colnames(est.cv) <- "Est.CV(rare)"
    out <- cbind(rbind(a, b, c, d, e, h, i), est.cv)
  }
  
  if (method == "Homogeneous")
    out <- SpecInciHomo(data, k, conf)
  if (method == "Chao")
    out <- rbind(SpecInciChao2(data, k, conf), SpecInciChao2bc(data, k, conf))
  if (method == "CE"){
    est.cv <- matrix(c(SpecInciModelh(data, k, conf)[[2]], SpecInciModelh1(data, k, conf)[[2]]), ncol = 1)
    colnames(est.cv) <- "Est.CV(rare)"
    out1 <- rbind(SpecInciModelh(data, k, conf)[[1]], SpecInciModelh1(data, k, conf)[[1]])
    out <- cbind(out1, est.cv)
  }
  if (method == "Jackknife")
    out <- rbind(SpecInciJack1(data, k, conf), SpecInciJack2(data, k, conf))
  
  return(out)
}
