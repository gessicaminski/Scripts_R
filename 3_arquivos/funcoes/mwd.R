########################################################
# Get the MCWD
# Delphine, 02/12/14
########################################################

mwd <- function(prec)  {

  # Get the maximum climatic water deficit as calculated in Mahli et al. 2009 PNAS
  # NB: December is assumed to be the wettest month, the soil is assumed to be saturated. So we start from January. 
  
  d <- dim(prec)  # d[1]: lon; d[2]: lat; d[3]: time
  
  epot = matrix(3.3*30, nrow = d[1], ncol = d[2]) # potential evapotranspiration fixed (100 mm/month or 3.3 mm/day)
  
  mwd  <- array(0, dim = c(d[1], d[2], floor(d[3]/12))) # 3rd dim: # of years
  
  for (i in 1:floor((d[3]/12))){
    #print(i)
    wd <- array(0, dim = c(d[1], d[2], 12))
    for (j in 1:12){
      aux = 12*(i-1)+j
 
      if (j == 1){
        wd[, , j] = -epot[ , ] + prec[ , , j]
      } else {
        wd[ , , j] = wd[ , , j-1] - epot[ , ] + prec[ , , aux]
        ind = which(wd[ , , j] > 0, arr.ind = TRUE)
        if (dim(ind)[1] != 0){   # to avoid errors with the dimension of ind
          for (k in 1:(nrow(ind))){
            wd[ind[k, 1], ind[k, 2], j] = 0
          }
        }
        
      }
      
    }
    mwd[ , ,i] = apply(wd, 1:2, min)
    #print(mwd[90:100,70:80, i])
    rm(wd, ind)
  }
  #print(dim(mwd))
  mwd[,,]
}