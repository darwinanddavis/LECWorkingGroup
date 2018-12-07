## 6 Dec 2018
## LEC working group
## Rachel Penczykowski, J. Trevor Vannatta, Matt Malishev, Zoe Johnson

## STATE VARIABLES (units = biomass)
## N = nutrients
## P = plants
## S = susceptible ungulate hosts
## I = infected ungulate hosts

## PARAMETERS
## r = intrinsic growth rate of plants
## K = carrying capacity of plants
## a = rate of nutrient addition 
## l = nutrient loss rate
## fp = rate of plant nutrient uptake
## beta = transmission rate
## es = assimilation efficiency of susceptible hosts
## ei = assimilation efficiency of infected hosts
## fs = feeding rate of susceptible hosts
## fi = feeding rate of infected hosts
## d = background host death rate
## v = mortality rate from infection
## ws = rate of waste production from susceptible hosts
## wi = rate of waste production from infected hosts

wd <- "/Users/malishev/Documents/Emory/research/workshops/stl/model"
setwd(wd)

packages <- c("RCurl","RColorBrewer","viridis","deSolve","ggplot2") 
if (require(packages)) {
  install.packages(packages,dependencies = T)
  require(packages)
}
lapply(packages,library,character.only=T)

# pull plotting func
script <- getURL("https://raw.githubusercontent.com/darwinanddavis/plot_it/master/plot_it.R", ssl.verifypeer = FALSE)
eval(parse(text = script))
display.brewer.all()
# Set global plotting parameters
cat("plot_it( \n0 for presentation, 1 for manuscript, \nset colour for background, \nset colour palette 1. use 'display.brewer.all()', \nset colour palette 2. use 'display.brewer.all()', \nset alpha for colour transperancy, \nset font style \n)")
plot_it(0,"blue","Blues","YlOrRd",1,"mono") # set plot function params       
plot_it_gg("white") # same as above for ggplot   
par(mfrow=c(3,2))

betas <- seq(0.1,1,0.1)
ds <- seq(0.1,1,0.1)

for(b in betas){
  
}

# 1: lots of infected  
# high beta
beta <- 0.9
d <- 0.1

# 1: lots of healthy
# low beta
beta <- 0.1
d <- 0.1

# 3: lots of death  
# high death
beta <- 0.3
d <- 0.9

parameters<-c(r=0.2, K=100, a=500, l=5, fp=0.5, beta=beta, 
              es=0.1, ei=0.05, fs=0.2, fi=0.1,
              d=d, v=0.1, ws=0.05, wi=0.09)

state<-c(N=200, P=200, S=20, I=2)

NPSI<-function(t, state, parameters) { 
  with(as.list(c(state, parameters)),{
    
    dN.dt <- a - l*N - fp*N*P + (d+ws)*S + (d+v+wi)*I  
    dP.dt <- fp*N*r*P*(1-(P/K)) - P*(fs*S*+fi*I)
    dS.dt <- P*(es*fs*S + ei*fi*I) - beta*S - (d+ws)*S
    dI.dt <- beta*S - (d+v+wi)*I
    
    list(c(dN.dt, dP.dt, dS.dt, dI.dt))
  })
}# end npsi function

# global output
times <- seq(0, 100, by=0.01)
out <- ode(y=state, times=times, func=NPSI, parms=parameters)
out <- data.frame(out)
head(out)

# ------------------------------ global scenario -------------------------
#plot
# par(las=1,bty="n")
# plot(out, ylim=c(0,max(times)),col=colv,type="l",lwd=2,xlab="Time")

# ------------------------------ altered scenarios -------------------------
out <- within(out, rm("N")) # remove nutrient pool 
out$Sp <- out[,"S"] / out[,"I"] + out[,"S"]
out$Ip <- out[,"I"] / out[,"I"] + out[,"S"]
out$SI <- out[,"I"] + out[,"S"]
head(out)

# 1
#pdf("pi_death_beta.pdf",onefile = F,paper="a4")
plot(out[,c("time","P")], ylim=c(0,max(times)),col=colv[4],type="l",lwd=2,xlab="Time",ylab="Biomass")
title(paste0("Transmission = ",beta, "\nDeath = ",d))
# plot(out[,c("time","Sp")], ylim=c(0,max(times)),col=colv[4],type="l",lwd=2,xlab="Time",ylab="Number in population")
# points(out[,c("time","Ip")],col=colv2[5],type="l",lwd=2,xlab="",ylab="")
plot(out[,c("time","SI")], ylim=c(0,max(times)),col=colv[4],type="l",lwd=2,xlab="Time",ylab="Number in population")

title(main=paste0("Orange = Infected"))


