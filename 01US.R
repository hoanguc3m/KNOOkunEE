library(fatBVARS)
RhpcBLASctl::blas_set_num_threads(RhpcBLASctl::get_num_cores())

library(readxl)
dataraw <- read_excel("International data 211108.xlsx",
                                        col_types = c("text", "numeric", "numeric",
                                                      "numeric", "numeric", "numeric", "numeric", "numeric",
                                                      "numeric", "numeric", "numeric"))
dataraw$...1 <- seq(from = as.Date("1948/06/01"), to = as.Date("2021/06/01"), by = "quarter")

#######################################################################################
# US data 2021
#######################################################################################
{
  t_max <- sum(!is.na(dataraw$US_GROWTH))
  num_obs <- nrow(dataraw)
  Var1 <- dataraw$US_GROWTH[(num_obs - t_max + 1): num_obs]
  Var2 <- dataraw$US_D_U[(num_obs - t_max + 1): num_obs]
  Time <- dataraw$...1[(num_obs - t_max + 1): num_obs]


  #############################################
  # Bivariate
  #############################################
  QuarterData <- cbind(Var1, Var2)
  colnames(QuarterData) <- c("GDP", "Unemployment")
  p <- 2

  #############################################

  y <- tail(QuarterData, t_max-p)
  y0 <- head(QuarterData, p)
  K <- ncol(y)

  #############################################
  prior <- get_prior(y, p = p, dist="Gaussian", SV = T)
  inits <- get_init(prior, samples = 170000, burnin = 20000, thin = 10)
  US_2021_Chain6 <- BVAR.SV(y, K = K, p = p, dist = "Gaussian", y0 = y0, prior = prior, inits = inits)

  #############################################
  prior <- get_prior(y, p = p, dist="Student", SV = T)
  inits <- get_init(prior, samples = 170000, burnin = 20000, thin = 10)
  US_2021_Chain7 <- BVAR.SV(y, K = K, p = p, dist = "Student", y0 = y0, prior = prior, inits = inits)

  ###########################################################################
  prior <- get_prior(y, p = p, dist="Skew.Student", SV = T)
  inits <- get_init(prior,  samples = 170000, burnin = 20000, thin = 10)
  US_2021_Chain8  <- BVAR.SV(y, K = K, p = p, dist = "Skew.Student", y0 = y0, prior = prior, inits = inits)
  # ###########################################################################
  # prior <- get_prior(y, p = p, dist="OT", SV = T)
  # inits <- get_init(prior, samples = 170000, burnin = 20000, thin = 10)
  # US_2021_Chain13 <- BVAR.SV(y, K = K, p = p, dist = "OT", y0 = y0, prior = prior, inits = inits)
  #
  # ###########################################################################
  #
  # prior <- get_prior(y, p = p, dist="OST", SV = T)
  # inits <- get_init(prior, samples = 170000, burnin = 20000, thin = 10)
  # US_2021_Chain14 <-   BVAR.SV(y, K = K, p = p, dist = "OST", y0 = y0, prior = prior, inits = inits)

}

#######################################################################################
# US data 2019
#######################################################################################
{

  t_max <- sum(!is.na(dataraw$US_GROWTH))
  num_obs <- nrow(dataraw) - 6 # Take the data upto 2019
  Var1 <- dataraw$US_GROWTH[(nrow(dataraw) - t_max + 1): num_obs]
  Var2 <- dataraw$US_D_U[(nrow(dataraw) - t_max + 1): num_obs]


  #############################################
  # Bivariate
  #############################################
  QuarterData <- cbind(Var1, Var2)
  colnames(QuarterData) <- c("GDP", "Unemployment")
  p <- 2

  #############################################

  y <- tail(QuarterData, t_max-p)
  y0 <- head(QuarterData, p)
  K <- ncol(y)

  #############################################
  prior <- get_prior(y, p = p, dist="Gaussian", SV = T)
  inits <- get_init(prior, samples = 170000, burnin = 20000, thin = 10)
  US_2019_Chain6 <- BVAR.SV(y, K = K, p = p, dist = "Gaussian", y0 = y0, prior = prior, inits = inits)

  #############################################
  prior <- get_prior(y, p = p, dist="Student", SV = T)
  inits <- get_init(prior, samples = 170000, burnin = 20000, thin = 10)
  US_2019_Chain7 <- BVAR.SV(y, K = K, p = p, dist = "Student", y0 = y0, prior = prior, inits = inits)

  ###########################################################################
  prior <- get_prior(y, p = p, dist="Skew.Student", SV = T)
  inits <- get_init(prior, samples = 170000, burnin = 20000, thin = 10)
  US_2019_Chain8  <- BVAR.SV(y, K = K, p = p, dist = "Skew.Student", y0 = y0, prior = prior, inits = inits)
  # ###########################################################################
  # prior <- get_prior(y, p = p, dist="OT", SV = T)
  # inits <- get_init(prior, samples = 170000, burnin = 20000, thin = 10)
  # US_2019_Chain13 <- BVAR.SV(y, K = K, p = p, dist = "OT", y0 = y0, prior = prior, inits = inits)
  #
  # ###########################################################################
  #
  # prior <- get_prior(y, p = p, dist="OST", SV = T)
  # inits <- get_init(prior, samples = 170000, burnin = 20000, thin = 10)
  # US_2019_Chain14 <-   BVAR.SV(y, K = K, p = p, dist = "OST", y0 = y0, prior = prior, inits = inits)

}

save.image("US.RData")
library(fatBVARS)
load("US.RData")

ML_US_2019_Chain6 <- marginalLL(US_2019_Chain6, ndraws = 20000, numCores = 16)
ML_US_2019_Chain7 <- marginalLL(US_2019_Chain7, ndraws = 20000, numCores = 16)
ML_US_2019_Chain8 <- marginalLL(US_2019_Chain8, ndraws = 20000, numCores = 16)
# ML_US_2019_Chain13 <- marginalLL(US_2019_Chain13, ndraws = 20000, numCores = 16)
# ML_US_2019_Chain14 <- marginalLL(US_2019_Chain14, ndraws = 20000, numCores = 16)

ML_US_2021_Chain6 <- marginalLL(US_2021_Chain6, ndraws = 20000, numCores = 16)
ML_US_2021_Chain7 <- marginalLL(US_2021_Chain7, ndraws = 20000, numCores = 16)
ML_US_2021_Chain8 <- marginalLL(US_2021_Chain8, ndraws = 20000, numCores = 16)
# ML_US_2021_Chain13 <- marginalLL(US_2021_Chain13, ndraws = 20000, numCores = 16)
# ML_US_2021_Chain14 <- marginalLL(US_2021_Chain14, ndraws = 20000, numCores = 16)

# save(ML_US_2019_Chain6, ML_US_2019_Chain7, ML_US_2019_Chain8, ML_US_2019_Chain13, ML_US_2019_Chain14,
#      ML_US_2021_Chain6, ML_US_2021_Chain7, ML_US_2021_Chain8, ML_US_2021_Chain13, ML_US_2021_Chain14,
#      file = "/home/hoanguc3m/MEGA/Okun/GDPUNE/International/US_ML.RData")
save(ML_US_2019_Chain6, ML_US_2019_Chain7, ML_US_2019_Chain8,
     ML_US_2021_Chain6, ML_US_2021_Chain7, ML_US_2021_Chain8,
     file = "US_ML.RData")

##########################################################################################
rm(list = ls(all.names = TRUE))

library(fatBVARS)
load("US.RData")


t2019_max = nrow(US_2019_Chain7$y)
t2021_max = nrow(US_2021_Chain7$y)

n.ahead = 24

T2019_SV_irf_matrix <- array(NA, dim = c(K,K,n.ahead+1, t2019_max))
T2021_SV_irf_matrix <- array(NA, dim = c(K,K,n.ahead+1, t2021_max))

tmp2019 <- parallel::mclapply(c(1:t2019_max),
                              FUN = function(atT) {
                                get_irf(Chain = US_2019_Chain7, impulse.variable = 1, response.variable = 2, n.ahead = n.ahead, atT = atT)$mean_all
                                },
                              mc.cores = 16)
tmp2021 <- parallel::mclapply(c(1:t2021_max),
                              FUN = function(atT) {
                                get_irf(Chain = US_2021_Chain7, impulse.variable = 1, response.variable = 2, n.ahead = n.ahead, atT = atT)$mean_all
                                },
                              mc.cores = 16)
# load("/home/hoanguc3m/MEGA/Okun/GDPUNE/International/impl/US_IRF.RData")

for (atT in c(1:t2019_max)){
  T2019_SV_irf_matrix[,,,atT] <- tmp2019[[atT]]
}
for (atT in c(1:t2019_max)){
  T2021_SV_irf_matrix[,,,atT] <- tmp2021[[atT]]
}
save(tmp2019, tmp2021, file = "US_IRF.RData")

#rm(tmp1,tmp2,tmp3,tmp4,tmp5)

T_range <- (1):(t2019_max)
D_range <- head(Time, t2019_max)
# length(D_range) == length(T_range)

for (lags in c(1:n.ahead)){
  T2019_SV_irf_matrix[1,1,lags,] <- smooth.spline(1:t2019_max, T2019_SV_irf_matrix[1,1,lags, ], spar=0.1)$y
  T2019_SV_irf_matrix[1,2,lags,] <- smooth.spline(1:t2019_max, T2019_SV_irf_matrix[1,2,lags, ], spar=0.1)$y
  T2019_SV_irf_matrix[2,1,lags,] <- smooth.spline(1:t2019_max, T2019_SV_irf_matrix[2,1,lags, ], spar=0.1)$y
  T2019_SV_irf_matrix[2,2,lags,] <- smooth.spline(1:t2019_max, T2019_SV_irf_matrix[2,2,lags, ], spar=0.1)$y
}

library(plotly)


n.ahead <- 10
T2019_SV_irf_matrix <- T2019_SV_irf_matrix[,,1:(1+n.ahead),]

fig <- plot_ly(x = D_range,
               y = c(0:n.ahead),
               z = T2019_SV_irf_matrix[1,1,,T_range]) %>% add_surface() %>% layout(title = "",scene = list(xaxis = list(title = ""),yaxis = list(title = ""),zaxis = list(title = "") ))
htmlwidgets::saveWidget(fig, file = "US-T-SV-ImpG-ResG.html")

fig <- plot_ly(x = D_range,
               y = c(0:n.ahead),
               z = T2019_SV_irf_matrix[2,2,,T_range]) %>% add_surface() %>% layout(title = "",scene = list(xaxis = list(title = ""),yaxis = list(title = ""),zaxis = list(title = "") ))
htmlwidgets::saveWidget(fig, file = "US-T-ImpU-ResU.html")

fig <- plot_ly(x = D_range,
               y = c(0:n.ahead),
               z = T2019_SV_irf_matrix[1,2,,T_range]) %>% add_surface() %>% layout(title = "",scene = list(xaxis = list(title = ""),yaxis = list(title = ""),zaxis = list(title = "") ))
htmlwidgets::saveWidget(fig, file = "US-T-ImpU-ResG.html")

fig <- plot_ly(x = D_range,
               y = c(0:n.ahead),
               z = T2019_SV_irf_matrix[2,1,,T_range]) %>% add_surface() %>% layout(title = "",scene = list(xaxis = list(title = ""),yaxis = list(title = ""),zaxis = list(title = "") ))
htmlwidgets::saveWidget(fig, file = "US-T-ImpG-ResU.html")


