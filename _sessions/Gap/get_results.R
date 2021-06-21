options(stringsAsFactors = F)
require(googlesheets4)
require(choicepp)
require(tidyverse)

# load data
data = readRDS('~/Dropbox (2.0)/Work/Software/Shiny/exploreGap/data/data.RDS')

# experience
p_test = data$test[,c('outA1','outA2','probA1','probA2','outB1','outB2','probB1','probB2')]
pa_test = as.matrix(do.call(rbind,lapply(1:50,function(x) p_test)))

# repeated
pid_test = data$test$pid ; pid_test = rep(pid_test,50)
p_ns = table(pid_test) / length(pid_test)
pa_test = p_arrange(pa_test,2)

# short
pid_test_s = data$test$pid ; pid_test = rep(pid_test,1)
p_ns_s = table(pid_test_s) / length(pid_test_s)
pa_test_s = p_arrange(as.matrix(p_test),2)


# GET PREDICTIONS --------

d = read_sheet(ss = "19m0NSqmE6Wo1YnSfJkB_7BS6gF1roV9iGJ0leqZl-HQ")
d = d %>% mutate_all(unlist)
d = d %>% mutate(
  Alpha = as.numeric(Alpha),
  Lambda = as.numeric(Lambda),
  Gamma = as.numeric(Gamma),
  Recency = as.numeric(Recency),
  Noise = as.numeric(Noise)
  )
d$Noise[d$Noise > 1] = d$Noise[d$Noise > 1] / 1000

mses = c()
for(i in 1:nrow(d)){

  print(i)
    
  mean_n = d$`Per option sample size`[i]
  cpt_par = c(d$Alpha[i],d$Lambda[i],d$Gamma[i])
  noise = d$Recency[i]
  recency = d$Noise[i]
  
  # draw sample sizes
  lambda = 1/(mean_n-1)
  n = length(pid_test)
  sizes = sample(rexp(n,lambda))
  sizes = sizes[sample(n)]
  sizes = ceiling(sizes - .5) + 1
  smplz = sampl_n(pa_test,sizes)
  
  # edit experience
  exp_p = choicepp:::edit_exp_pos(smplz,pa_test,add_n = T,do_arrange = T)
  
  # choose
  choices = choicepp:::cpt_choice_pos(cpt_par,exp_p,type = 1,noise,recency)
  
  # eval fit
  crit = tapply(data$test$choice,data$test$pid,mean)
  pred = tapply(choices,pid_test,mean)
  
  mses[i] = sum((crit - pred)**2 * p_ns[names(crit)])
  
  }
mses

###### Exercise results<

plot_xplore = function(pars, mses, alpha = .1, q = .001){

  col = viridis::cividis(1)
  
  par(mfrow=c(3,2),mar=c(3,2,1,1))
  
  sel = which(mses <= quantile(mses,q))
  
  plot.new();plot.window(xlim=c(1,40),c(0,.15))
  points(pars[,1],mses,pch=16,col=rgb(0,0,0,alpha=alpha))
  mtext(c('Sample size','MSE'),side=c(1,2),line=c(2,.5))
  mtext(c(1,seq(10,40,10)),at=c(1,seq(10,40,10)),side=1,cex=.8)
  points(pars[sel,1],mses[sel],pch=16,col=col,cex=1.5)
  
  plot.new();plot.window(xlim=c(0,2),c(0,.15))
  points(pars[,2],mses,pch=16,col=rgb(0,0,0,alpha=alpha))
  mtext(c('Alpha','MSE'),side=c(1,2),line=c(2,.5))
  mtext(seq(0,2,.5),at=seq(0,2,.5),side=1,cex=.8)
  points(pars[sel,2],mses[sel],pch=16,col=col,cex=1.5)
  
  plot.new();plot.window(xlim=c(1,3),c(0,.15))
  points(pars[,3],mses,pch=16,col=rgb(0,0,0,alpha=alpha))
  mtext(c('Lambda','MSE'),side=c(1,2),line=c(2,.5))
  mtext(seq(1,3,.5),at=seq(1,3,.5),side=1,cex=.8)
  points(pars[sel,3],mses[sel],pch=16,col=col,cex=1.5)
  
  plot.new();plot.window(xlim=c(0,2),c(0,.15))
  points(pars[,4],mses,pch=16,col=rgb(0,0,0,alpha=alpha))
  mtext(c('Gamma','MSE'),side=c(1,2),line=c(2,.5))
  mtext(seq(0,2,.5),at=seq(0,2,.5),side=1,cex=.8)
  points(pars[sel,4],mses[sel],pch=16,col=col,cex=1.5)
  
  plot.new();plot.window(xlim=c(-1,1),c(0,.15))
  points(pars[,5],mses,pch=16,col=rgb(0,0,0,alpha=alpha))
  mtext(c('Recency','MSE'),side=c(1,2),line=c(2,.5))
  mtext(seq(-1,1,.5),at=seq(-1,1,.5),side=1,cex=.8)
  points(pars[sel,5],mses[sel],pch=16,col=col,cex=1.5)
  
  plot.new();plot.window(xlim=c(0,1),c(0,.15))
  points(pars[,6],mses,pch=16,col=rgb(0,0,0,alpha=alpha))
  mtext(c('Noise','MSE'),side=c(1,2),line=c(2,.5))
  mtext(seq(0,1,.25),at=seq(0,1,.25),side=1,cex=.8)
  points(pars[sel,6],mses[sel],pch=16,col=col,cex=1.5)
}

pdf('~/Dropbox (2.0)/Work/Teaching/Workshops/DfEWorkshop/xploreParticipantRes.pdf')
  plot_xplore(as.matrix(d[,-(1:2),drop=F]),mses,alpha=1,q=0)
  dev.off()

winners = d[which(mses == min(mses)),2]
winners

d$`Your name (real or fictional)`[order(mses)]
