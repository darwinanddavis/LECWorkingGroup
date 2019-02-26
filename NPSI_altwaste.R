### NPSI alternative waste production ###

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
## dP = background plant death rate (nutrient leaching)
## v = mortality rate from infection
## ws = rate of waste production from susceptible hosts
## wi = rate of waste production from infected hosts


##########################################################################################
################################# User inputs for model ##################################
##########################################################################################



# set your working directory 
# E.g "/Users/malishev/dope_models/my_dope_model/"
wd <- "paste the path to where you saved the model here (with these quotes)"
setwd(wd)

# set parameter ranges (min 0, max 1)
beta_access <- 0.1 # choose your beta value you want to plot at the end
death_access <- 0.9 # choose your death value you want to plot at the end
colvv <- "orange" # choose your plot line colour

## initial conditions
N <- 200 # size of nutrient biomass in env 
P <- 200 # initial plants in env
S <- 20 # num of susceptible hosts
In <- 2 # num of infected hosts 
years <- 100 # number of years to run simulation
time.out <- 0.01 # simulation time step (0.01 = 1 year if years = 100) 

##########################################################################################
##################################### Setup simulation model #############################
##########################################################################################

# ---------------------- run the model from here # ---------------------- 
# load packages
packages <- c("RCurl","RColorBrewer","viridis","deSolve","ggplot2","dplyr","tibble","purrr","reshape2","tidyr","zoo","plyr","plotly","beepr") 
if (require(packages)) {
  install.packages(packages,dependencies = T)
  require(packages)
}
ppp <- lapply(packages,require,character.only=T)
if(any(ppp==F)){cat("\n\n\n ---> Check packages are loaded <--- \n\n\n")}

# pull plotting function
script <- getURL("https://raw.githubusercontent.com/darwinanddavis/plot_it/master/plot_it.R", ssl.verifypeer = FALSE)
eval(parse(text = script))
display.brewer.all()
# Set global plotting parameters
cat("plot_it( \n0 for presentation, 1 for manuscript, \nset colour for background, \nset colour palette 1. use 'display.brewer.all()', \nset colour palette 2. use 'display.brewer.all()', \nset alpha for colour transperancy, \nset font style \n)")
plot_it(0,"blue","Blues","YlOrRd",1,"mono") # set plot function params       
plot_it_gg("white") # same as above for ggplot   

# set param space
beta_pars <- seq(0.1,1,0.1) # transmission rate in model 
death_pars <- seq(0.1,1,0.1) # death rate in model

# desired outputs
out <- list()
out_master <- list() # NPSI output 
out_tibble <- tibble()
outplot <- list()
param_space <- list(beta_pars,death_pars) # summed parameter space 

# create empty list
out_master <- rep(
  list(structure(list(
    pars = numeric(), 
    outs = list()
  ),
  .Names = c("Parameter", "Output")))
  ,prod(as.numeric(summary(param_space)[,1]))
)
sc <- 1 # timer in simulation model 

##########################################################################################
################################### create simulation model  #################################

# to set pars as individual beta and death values
npsi_func <- function(waste){ # start npsi_func
  waste <- waste
  # ------- start simulation # ------- 
  for(beta in beta_pars){ # pass through beta values
    for(death in death_pars){ # pass through death values 
      parameters<-c(r=0.2, K=100, a=500, l=5, fp=0.5, beta=beta, 
                    es=0.1, ei=0.05, fs=0.2, fi=0.1,
                    d=death, v=0.1, ws=0.05, wi=0.09, dp=0.1)
      
      state<-c(N=N, P=P, S=S, I=In) # set initial conditions
      
      NPSI<-function(t, state, parameters) { 
        with(as.list(c(state, parameters)),{
          
          if(waste=="waste_sum"){
            dN.dt <- a - l*N - fp*N*P + dp*P + (d+(1-es)*fs*P)*S + (d+v+(1-ei)*fi*P)*I # summed waste with plant death
            dP.dt <- fp*N*r*P*(1-(P/K)) - P*(fs*S+fi*I) # plants produced  
            dS.dt <- P*(es*fs*S + ei*fi*I) - beta*S - d*S # susceptible host population change
            dI.dt <- beta*S - (d+v)*I # infected hosts population change
          } # summed waste with plant death      
          if(waste=="waste_drool"){
            dN.dt = a - l*N - fp*N*P + (d+ws)*S + (d+v+wi)*I + (1 - P*(es*fs*S + ei*fi*I)) # nutrient input from messy eating
            dP.dt <- fp*N*r*P*(1-(P/K)) - P*(fs*S+fi*I) # plants produced  
            dS.dt <- P*(es*fs*S + ei*fi*I) - beta*S - (d+ws)*S # susceptible host population change
            dI.dt <- beta*S - (d+v+wi)*I # infected hosts population change
          } # waste from messy eating per feeding bout
          if(waste=="waste_host"){
            dN.dt <- a - l*N - fp*N*P + (d+ws)*S + (d+v+wi)*I 
            dP.dt <- fp*N*r*P*(1-(P/K)) - P*(fs*S+fi*I) # plants produced  
            dP.dt <- fp*N*r*P*(1-(P/K)) - P*(fs*S*+fi*I) # plants produced  
            dS.dt <- P*(es*fs*S + ei*fi*I) - beta*S - (d+ws)*S # susceptible host population change
            dI.dt <- beta*S - (d+v+wi)*I # infected hosts population change
          } # nutrients in env
          list(c(dN.dt, dP.dt, dS.dt, dI.dt)) # compile outputs 
        })
      } # end npsi function
      
      # -------  global output # ------- 
      times <- seq(0, years, by=time.out) # set time horizon for simulation (years)
      out <- ode(y=state, times=times, func=NPSI, parms=parameters) # run simulation model
      out <- data.frame(out)
      # save outputs
      # out_master[[length(out_master) + 1]] <- out # working with out_master <- list()
      out_master[[sc]]$Output <- out # save output for each run
      out_master[[sc]]$Parameter[1] <- beta # save beta for each run
      out_master[[sc]]$Parameter[2] <- death # save death for each run 
      sc <- sc + 1
    } # end death pars   
  } # end beta pars    
  
  # -------  clean output # -------
  # save simulation model to global vector (tibble)
  out_tibble <- tibble(
    params = map(out_master, "Parameter"),
    outs = map(out_master, "Output")
  ) %>% 
    mutate(
      beta = map(params, 1),
      death = map(params, 2) 
    ) %>%
    select(beta, death, outs)
  
  # ------- plotting ----------
  # start save plot to local dir  
  pdf(paste0(getwd(),"/npsi_model_plot_",waste,".pdf"),onefile=T,width=10,height=8,paper="a4r") 
  outplot <- filter(out_tibble, death == death_access & beta == beta_access)
  outplot <- outplot$outs ; outplot <- as.data.frame(outplot) # clean output
  outplot$"Total host population" <- outplot[,"S"] + outplot[,"I"] # add sum host population
  # plot results
  layout(matrix(c(1,2,3,4,5,5), 2, 3, byrow = TRUE)) # set plot window
  colnames(outplot) <- c("Time", #1
                         "Nutrient biomass", #2
                         "Plant biomass", #3
                         "Hosts (susceptible)", #4
                         "Hosts (infected)", #5
                         "Total hosts") #6
  for (name in names(outplot)[c(3:5,2,6)]){ # start plot for product, suscep hosts, infec hosts, nutrients, and total hosts
    plot(outplot[,1],outplot[,name],type="l",las=1,bty="n",
         xlab="Time (years)",ylab=name,col=colvv,
         ylim=c(0,round_any(max(outplot[,name]),10,ceiling))
    )
    title(paste0(name,"\nbeta = ",beta_access," , death = ",death_access))
  } # end plot
  # add mean plot
  dev.off() # save output to dir
  cat(paste0("\n\n\nPlot is saved in \n",getwd(), "\nas npsi_model_plot_",waste,".pdf\n\n\n"))
  replicate(1,{beep(rep_len(8,1))}) # play sound when finished 
  return(out_tibble)
} # ------- end npsi_func 

# set waste outputs
# waste_host = waste produced per host (suscept or infect)  
# waste_drool = waste produced per feeding bout per host (suscept or infect)  
# waste_sum = summed waste as overall output (plus plant death)

### run model function 
out_tibble_ws <- npsi_func("waste_sum")
out_tibble_wd <- npsi_func("waste_drool")
out_tibble_wh <- npsi_func("waste_host")

################################### end simulation model  #################################
##########################################################################################

################################### plot results manually  #################################

### -------------------------- user defined params -------------------------------
# set parameter ranges (min 0, max 1)
beta_access <- 1 # choose your beta value you want to plot at the end
death_access <- 0.1 # choose your death value you want to plot at the end

### -------------------------- run code from here to plot 
#pdf(paste0(getwd(),"/npsi_model_plot_",ttl,".pdf"),onefile=T,width=10,height=8,paper="a4r") 
# then run this part to plot in your live R session
layout(matrix(c(1,2,3,4,5,5), 2, 3, byrow = TRUE)) # set plot window
wastes <- list(out_tibble_wh,out_tibble_wd,out_tibble_ws)
ttl_list <- c("Host waste","Drool waste","Summed waste")
out_names <- c("Time",
               "Nutrient biomass",
               "Plant biomass", 
               "Hosts (susceptible)",
               "Hosts (infected)",
               "Total hosts")
colvec<-brewer.pal(length(wastes),"Set2")

pf <- 1 # plot feature counter
pc <- 1 # plot number counter

for(out_tibble in wastes){  
  outplot <- filter(out_tibble, death == death_access & beta == beta_access)
  outplot <- outplot$outs ; outplot <- as.data.frame(outplot) # clean output
  outplot$"Total host population" <- outplot[,"S"] + outplot[,"I"] # add sum host population
  colnames(outplot) <- out_names
  for (name in names(outplot)[c(3:5,2,6)]){ # start plot
    plot(outplot[,1],outplot[,name],type="l",las=1,bty="n",
         xlab="Time (years)",ylab=name,
         col=colvec[pf],#lty=pf,
         ylim=c(0,round_any(max(outplot[,name]),10,ceiling))
    )
    title(paste0(name,"\nbeta = ",beta_access," , death = ",death_access))
  } # end plot loop
  #dev.off()
  title(paste0("\n\n\n",ttl_list[pc]))
  pf <- pf + 1; pc <- pc + 1
} # end wastes loop

### -------------------------- simulate all param values -------------------------------
out_names <- c("Time", #1
               "Nutrient biomass", #2
               "Plant biomass", #3
               "Hosts (susceptible)", #4
               "Hosts (infected)", #5
               "Total hosts") #6
# select output to plot
out_name <- out_names[5] 
# choose your death value you want to plot at the end
death_access <- 0.9

### -------------------------- run code from here to plot
#pdf(paste0(getwd(),"/npsi_model_plot_",ttl,".pdf"),onefile=T,width=10,height=8,paper="a4r") 
# then run this part to plot in your live R session
layout(matrix(c(1,2,3), 1, 3, byrow = TRUE)) # set plot window
wastes <- list(out_tibble_wh,out_tibble_wd,out_tibble_ws)
ttl_list <- c("Host waste","Drool waste","Summed waste")
pf <- 1 # plot feature counter
pc <- 1 # plot number counter
colvec<-brewer.pal(length(beta_pars),"RdYlGn") # !!! make sure length of colour palette is > beta_pars

# dp <- 0.2
# bp <- 0.2
for(out_tibble in wastes){
  for(beta_access in beta_pars){ 
    outplot <- filter(out_tibble, death == death_access & beta == beta_access)
    outplot <- outplot$outs ; outplot <- as.data.frame(outplot) # clean output
    outplot$"Total host population" <- outplot[,"S"] + outplot[,"I"] # add sum host population
    colnames(outplot) <- out_names 
    if(out_name==names(outplot[2]) | out_name==names(outplot[3])){ylim=c(0,200)}else{ylim=c(0,50)};ylim # set proper ylim
    if(pf==length(beta_pars)+1){pf <- 1} # reset pf to reset colour palette for new plot
    plot(outplot[,1],outplot[,out_name],type="l",las=1,bty="n",
         xlab="Time (years)",ylab=out_name,
         col=colvec[pf],#lty=pf,
         ylim=ylim
    )
    par(new=T)
    #dev.off()
    pf <- pf + 1
  } # end bbb loop
  pc <- pc + 1
  title(paste0(out_name,"\nbeta=",min(beta_pars)," to ",max(beta_pars),", death = ",death_access,"\n",ttl_list[pc-1]))
  text(years-40,ylim[2],paste0("b=",beta_pars[1]),col=colvec[1]) # low beta
  text(years-10,ylim[2],paste0("b=",max(beta_pars)),col=colvec[length(colvec)]) # high beta
} # end wastes loop


