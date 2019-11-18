clado_est <- function(sim_output){
  est_params <- sim_output[["est_pars"]]
  lamc <- est_params[,1]
  lamc = as.data.frame(lamc)
  # median_lamc = median(lamc$lamc)
  # mean_lamc = mean(lamc$lamc)
  ggplot(lamc, aes(x=lamc)) + 
    geom_histogram(mapping = aes(y = ..density..),binwidth=0.08,colour = "grey",fill = "grey") + 
    # geom_density(position = "stack") + 
    geom_vline(aes(xintercept=mean(lamc, na.rm=T)),   # Ignore NA values for mean
               color="red", linetype="dashed", size=1) + 
    xlim(0,3)+
    xlab("cladogenesis")+
    ggtitle("estimated cladogenetic speciation rate")
  # return(median_lamc)
}

ext_est <- function(sim_output){
  est_params <- sim_output[["est_pars"]]
  mu <- est_params[,2]
  mu = as.data.frame(mu)
  ggplot(mu, aes(x=mu)) + 
    geom_histogram(mapping = aes(y = ..density..),binwidth=0.2,colour = "grey",fill = "grey") + 
    # geom_density(position = "stack") + 
    geom_vline(aes(xintercept=mean(mu, na.rm=T)),   # Ignore NA values for mean
               color="red", linetype="dashed", size=1) + 
    xlim(0,5)+
    xlab("extinction rate")+
    ggtitle("estimated extinction rate")
}

K_est <- function(sim_output){
  est_params <- sim_output[["est_pars"]]
  k <- est_params[,3]
  k = as.data.frame(k)
  ggplot(k, aes(x=k)) + 
    geom_histogram(mapping = aes(y = ..density..),binwidth=1,colour = "grey",fill = "grey") + 
    # geom_density(position = "stack") + 
    geom_vline(aes(xintercept=mean(k, na.rm=T)),   # Ignore NA values for mean
               color="red", linetype="dashed", size=1) + 
    xlim(0,25)+
    xlab("K")+
    ggtitle("estimated carrying capacity")
}


immig_est <- function(sim_output){
  est_params <- sim_output[["est_pars"]]
  gamma <- est_params[,4]
  gamma = as.data.frame(gamma)
  ggplot(gamma, aes(x=gamma)) + 
    geom_histogram(mapping = aes(y = ..density..),binwidth=0.02,colour = "grey",fill = "grey") + 
    # geom_density(position = "stack") + 
    geom_vline(aes(xintercept=mean(gamma, na.rm=T)),   # Ignore NA values for mean
               color="red", linetype="dashed", size=1) + 
    xlim(0,0.4)+
    xlab("immigration rate")+
    ggtitle("estimated immigration rate")
}

ana_est <- function(sim_output){
  est_params <- sim_output[["est_pars"]]
  lama <- est_params[,5]
  lama = as.data.frame(lama)
  ggplot(lama, aes(x=lama)) + 
    geom_histogram(mapping = aes(y = ..density..),binwidth=0.1,colour = "grey",fill = "grey") + 
    # geom_density(position = "stack") + 
    geom_vline(aes(xintercept=mean(lama, na.rm=T)),   # Ignore NA values for mean
               color="red", linetype="dashed", size=1) + 
    xlim(0,1)+
    xlab("anagenesis rate")+
    ggtitle("estimated anagenetic speciation rate")
}




