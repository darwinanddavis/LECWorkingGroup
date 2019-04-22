# Nutrient Quota Host-Parasite Model: gut parasite disease transmission model in host-trematode systems with explicit nutrient exchange and recycling

# needs defining 
# q_P

# Changes
# F (food) has become R (resources), so F, h_F, and Q_F are now R, h_R, and Q_R, resp (R can't assign F).  
# Q_min and Q_max have become variables instead of being separate parameters for Q_H and Q_R

# **TO DO**    
# - Two candidate models, one gut parasite model where parasites subtract from nutrient reserve within host (pre host growth) and one tissue parasite model where parasites subtract from host biomass (post host growth).  
# - Find min and max bounds on nutrient quota for uptake rates (Q_minus and Q_plus)
# - Phosphorous content and stoich flexiblity (Paul Frost, Charlotte Narr, Nicole Wagner)      

## STATE VARIABLES (units = biomass)
# H = host consumer population (host biomass)    
# N = nutrients in the environment (biomass)    
# P = parasite population (within-host parasite biomass)  
# Q_H = nutrient quota in hosts (nutrient/carbon ratio)    
# Q_minus = min phosphorous to carbon ratio (1/Q_minus = high carbon to phosphorous ratio)
# Q_plus = max phosphorous to carbon ratio (1/Q_plus = low carbon to phosphorous ratio)
# Q_R = nutrient quota in food source (nutrient/carbon ratio) $Q_F \cdot$ is total nutrients in food    
# R = resources in the landscape (food biomass)    
# W = waste (recycled biomass)

## PARAMETERS
# alpha = virulence of parasite to hosts 
# beta = transmission rate of infection  
# d_H = background death rate of hosts
# d_P = background death rate of parasites 
# d_R = background death rate of resources
# f = functional feeding rate
# h_N = nutrient uptake efficiency by resources 
# h_R = resource uptake efficiency by hosts  
# K = carrying capacity of resources
# l = nutrient leaching 
# mu_P = maximum uptake of host nutrients by parasites  
# mu_R = maximum uptake of resources
# q_P = ???
# s = nutrient supply
# v_F = intrinsic growth rate of resources

##########################################################################################
################################# User inputs for model ##################################
##########################################################################################

# set your working directory 
# E.g "/Users/malishev/dope_models/my_dope_model/"
wd <- "paste the path to where you saved the model here (with these quotes)"
setwd(wd)

# set parameter ranges (min 0, max 1)
name <- "H" # choose output to plot (see state variables)  
param1_pars <- seq(0.1,1,0.1) # parameter 1 range in model
param2_pars <- seq(0.1,1,0.1) # parameter 2 range in model
param1_name <- "alpha" # parameter 1 to vary in model
param2_name <- "beta" # parameter 2 to vary in model
param1_select <- 0.1 # choose value you want to plot at the end for parameter 1
param2_select <- 0.9 # choose value you want to plot at the end for parameter 2
colv1 <- "orange" # choose plot line colour (param1)
colv2 <- "steel blue" # choose plot line colour (param2)

## initial conditions
N <- 200 # nutrient biomass in env 
H <- 20 # number of hosts in env 
P <- 200 # initial products in env
R <- 500 # food in env  
W <- 200 # waste (recycled biomass)  
Q_H <- 0.5 # nutrient quota in hosts (nutrient/carbon ratio)  
Q_R <- 0.5 # nutrient quota in resources (nutrient/carbon ratio)  
Q_minus <- 10 # max phos:carbon ratio
Q_plus <- 50 # min phos:carbon ratio
years <- 100 # number of years to run simulation
time.out <- 0.01 # simulation time step (0.01 = 1 year if years = 100) 

## additional parameters
alpha <- 0.9  # virulence of parasite to hosts
beta <-  0.5 # transmission rate of infection    
d_H <- 0.1 # background death rate of hosts
d_P <- 0.1 # background death rate of parasites  
d_R <- 0.1 # background death rate of resources
f <- 1 # functional feeding rate   
h_N <- 0.8  # nutrient uptake efficiency by food 
h_R <- 0.8 # food uptake efficiency by hosts  
K <- 100 # carrying capacity of within-host parasite biomass  
l <- 200 # nutrient leaching
mu_P <-  0.8 # maximum uptake of host nutrients by parasites  
mu_R <- 0.9 # maximum uptake of resources 
q_P <- 1 # 
s <- 500 # nutrient supply
v_R <- 0.9 # intrinsic growth rate of food 

##########################################################################################
##################################### Setup simulation model #############################
##########################################################################################

# ---------------------- run the model from here # ---------------------- 
# load packages
packages <- c("RCurl","RColorBrewer","viridis","deSolve","ggplot2","ggthemes","gridExtra","dplyr","tibble","purrr","reshape2","tidyr","zoo","plyr") 
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
plot_it(1,"blue","Blues","YlOrRd",1,"mono") # set plot function params       
plot_it_gg("white") # same as above for ggplot   

# desired outputs
out <- list()
out_master <- list() # nqhp output 
out_tibble <- tibble()
outplot <- list()
param_space <- list(param1_pars,param2_pars) # summed parameter space 

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
################################### start simulation model  ##############################

# start nqhp_func
nqhp_func <- function(){ 
  # ------- start simulation # ------- 
  for(param1 in param1_pars){ # start param1 loop
    for(param2 in param2_pars){ # start param2 loop
    parameters<-c(# alpha=alpha, beta=beta,
                  alpha=param1, beta=param2,
                  d_H=d_H, d_P=d_P, d_R=d_R,
                  h_N=h_N, h_R=h_R, 
                  f=f, K=K, l=l, 
                  mu_P, mu_R, 
                  q_P=q_P,
                  s=s, v_R=v_R
                  )
    
    # set initial conditions
    state<-c(W=W, N=N, R=R, Q_R=Q_R, H=H, Q_H=Q_H, P=P) 
    
    # define function inputs  
    NQHP<-function(t, state, parameters) { 
      with(as.list(c(state, parameters)),{
  
        ################# state variable ODEs ############### 
        # ------------------------------------ waste in env 
        dW.dt <- d_R * R * Q_R +
                d_H * H * Q_H + 
                alpha * P * Q_H +
                d_P * P * q_P +
                alpha * P^2 / H * (K + 1 / K) * q_P
        # ------------------------------------ nutrient growth
        dN.dt <- s - v_R * (N / h_N + N) * 
                ((Q_R * Q_plus) - Q_R) / ((Q_R * Q_plus) - (Q_R * Q_minus)) * 
                R + W - (l * N) 
        # ------------------------------------ resource growth 
        dR.dt <- mu_R * (1 - (Q_R * Q_minus) / Q_R) * R - 
                (d_R * R) - 
                (f * R * H / h_R + R) 
        # ------------------------------------ resource nutrient quota 
        dQ_R.dt <- v_R * (N / h_N + N) * 
                  ((Q_R * Q_plus) - Q_R) / ((Q_R * Q_plus) - (Q_R * Q_minus)) - 
                  mu_R * (1 - (Q_R * Q_minus) / Q_R) * Q_R
        # ------------------------------------ host population growth 
        dH.dt <- f * R / h_R + R * (1 - (Q_H * Q_minus) / Q_H) * H - 
                d_H * H - 
                mu_P * P - 
                alpha * P 
        # ------------------------------------ host nutrient quota 
        dQ_H.dt <- f * R * Q_R / h_R + R * 
                  ((Q_H * Q_plus) - Q_H) / ((Q_H * Q_plus) - (Q_H * Q_minus)) -
                  f * R / h_R + R * (1 - (Q_H * Q_minus) / Q_H) * Q_H -
                  mu_P * Q_H * (P / H)
        # ------------------------------------ within-host parasite growth  
        dP.dt <- beta * mu_P * (1 - q_P / Q_H) * P * H - 
                d_P * P -
                alpha * (P^2 / H) * (K + 1 / K) 
        
        list(c(dW.dt, dN.dt, dR.dt, dQ_R.dt, dH.dt, dQ_H.dt, dP.dt)) # compile outputs 
      })
    } # end nqhp function
  
    # ---------------------  global output
    times <- seq(0, years, by=time.out) # set time horizon for simulation (years)
    out <- ode(y=state, times=times, func=NQHP, parms=parameters) # run simulation model
    out <- data.frame(out)
    # save outputs
    # out_master[[length(out_master) + 1]] <- out # working with out_master <- list()
    out_master[[sc]]$Output <- out # save output for each run
    out_master[[sc]]$Parameter[1] <- param1 # save param1 for each run 
    out_master[[sc]]$Parameter[2] <- param2 # save param2 for each run 
    sc <- sc + 1
    } # end param2 loop  
    } # end param1 loop  
 
  # ---------------------  clean output 
  # save simulation model to global vector (tibble)
  out_tibble <- tibble(
    params = map(out_master, "Parameter"),
    outs = map(out_master, "Output")
  ) %>% 
    mutate(
      param1 = map(params, 1),
      param2 = map(params, 2)
    ) %>%
    select(param1, param2, outs)
  
  # --------------------- plotting 
  # start save plot to local dir  
  pdf(paste0(getwd(),"/nqhp_model_plot.pdf"),onefile=T,width=10,height=8,paper="a4r")
  outplot <- filter(out_tibble, param1 == param1_select & param2 == param2_select)
  outplot <- outplot$outs ; outplot <- as.data.frame(outplot) # clean output
  # plot results
  layout(matrix(c(1,2,3,4,5,5), 2, 3, byrow = TRUE)) # set plot window
  colnames(outplot) <- c("Time", #1
                         "Waste", #2
                         "Nutrients", #3
                         "Resources", #4
                         "Resource nutrient quota", #5
                         "Hosts", #6
                         "Host nutrient quota", #7
                         "Parasites" #8
                         ) 
  for (name in names(outplot)[c(2:4,6,8)]){ # start plot 
    plot(outplot[,1],outplot[,name],type="l",las=1,bty="n",
         xlab="Time (years)",ylab=name,col=colv1,
         # ylim=c(0,round_any(max(outplot[,name]),10,ceiling))
    )
    title(paste0(name,"\n",param1_name," = ",param1_select))
  } # end plot
  # add mean plot
  dev.off() # save output to dir
  cat(paste0("\n\n\nPlot is saved in \n",getwd(), "\nas nqhp_model_plot.pdf\n\n\n"))
  return(out_tibble)
} # ------- end nqhp_func 

### run model function 
out_tibble <- nqhp_func()

################################### end simulation model  #################################
##########################################################################################

################################### plot results manually  #################################

# set parameter ranges (min 0, max 1)
param1_select <- 0.1 # choose your param1 value you want to plot at the end
param2_select <- 0.9 # choose your param2 value you want to plot at the end

# then run this part to plot in your live R session
outplot <- filter(out_tibble, param1 == param1_select & param2 == param2_select)
outplot <- outplot$outs ; outplot <- as.data.frame(outplot) # clean output

layout(matrix(c(1,2,3,4,5,5), 2, 3, byrow = TRUE)) # set plot window
colnames(outplot) <- c("Time", #1
                       "Waste", #2
                       "Nutrients", #3
                       "Resources", #4
                       "Resource nutrient quota", #5
                       "Hosts", #6
                       "Host nutrient quota", #7
                       "Parasites" #8
                       ) 
for (name in names(outplot)[c(2:4,6,8)]){ # start plot 
  plot(outplot[,1],outplot[,name],type="l",las=1,bty="n",
       xlab="Time (years)",ylab=name,col=colv1,
       # ylim=c(0,round_any(max(outplot[,name]),10,ceiling))
  )
  title(paste0(name,"\n",param1_name," = ",param1_select))
} # end plot
