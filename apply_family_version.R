# Sample from the post distri of PS in each model 
# to obtain the final posterior distri of PS 
# line 374
bma_ps2 <- matrix(rep(0,nrow(beta)*nrow(data_comp1)),nrow=nrow(data_comp1))


nmodel <- length(bma_bus$postprob)
model_label <- 1:nmodel




post_prob <- bma_bus$postprob[1:nmodel]/sum(bma_bus$postprob[1:nmodel])




for (k in 1:nrow(beta))
{
  bma_ps2[,k] <- bps_all[[sample(model_label,1,prob=post_prob)]][,k]
}


test <- rep(list(1), nmodel)
for (i in 1:nmodel){
  test[[i]] <- as.formula(paste("bs_deg ~ ", paste(covar[variables[i,]==TRUE], collapse= "+")))
  }
test
