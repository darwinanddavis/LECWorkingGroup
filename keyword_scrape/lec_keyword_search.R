# required files
# LEC100testrecords.txt
# search_term_inputs.txt
# article_col_names.txt
# lec_keyword_search.R   

##########################################################################
##########################################################################
##########################################################################

# load packages (run once) ------------------------------------------------

install.packages("pacman")
require(pacman)
p_load(dplyr,purrr,readr)

# user inputs -------------------------------------------------------------

# set working dir
setwd("/Users/malishev/Documents/Emory/research/workshops/stl/journal_data")

# Step 1 ----------------------------------------------------------------------

# Enter either Title or Abstract to search for the keywords
extract1 <- "Title" 

# Step 2 ------------------------------------------------------------------

# Now enter what data you want to get out of the final results
# For example, if you want to know what year in which the resulting papers were published,
# type in "Year"
# use any search term you specified in the search_terms_input file
extract2 <- "Year"

##########################################################################
##########################################################################
##########################################################################

# run rest of code from here ----------------------------------------------
# read in file ----------------------------------------------------------

fh <- "LEC100testrecords.txt"
anomalies <- 1 # save csv file that   

ww <- readLines("search_term_inputs.txt") # set working dir
wd <- setwd(ww[1]) 
tt <- read.delim(paste0(wd,"/",fh),header=T,sep="\t")
tt %>% str
# set column names for df from file
df_colnames <- read.delim("article_col_names.txt",header=F,strip.white = T,sep=",",colClasses = "character")[1,] %>% as.character
colnames(tt) <- df_colnames

# view structure of data frame
tt %>% str
# randomly sample entries to see if contents align with the above col names
tt[sample(126,1),]

# key terms ---------------------------------------------------------------

keyterms <- read.delim("search_term_inputs.txt",header=F,skip=1,strip.white = T,sep=",",colClasses = "character")[1,] %>% as.character
keyterms <- keyterms %>% as.character ; keyterms_neat <- keyterms
keyterms <- paste(keyterms,collapse="|"); keyterms_neat # combine all the search terms 

final <- tt[grep(keyterms, tt[,extract1], ignore.case = T),] #
length(final[,extract1]) # get number of results
tt[final[,extract1],extract1] # show raw outputs 

### Remove NA columns  
rm_na <- grep("NA", names(tt), ignore.case = F)
final[,colnames(final[,rm_na])] <- list(NULL)

# extract isolated data col
ww <- as.list(ww); names(ww) <- ww
col_final <- ww[extract2] %>% as.character()
if(col_final!=""){final[,col_final]} # return as vector

# remove duplicate entries
final <- unique(final)

## Save output to file  
fho <- "keyword_final.csv"
write_csv(final,paste0(wd,"/",fho))

## Save isolated results data to file  
# final[col_final] # return as data frame column    
if(col_final!=""){
  final_isolated <- final[,col_final] # return as vector
  final_isolated <- as.data.frame(final_isolated); colnames(final_isolated) <- col_final
  fho_iso <- "keyword_final_isolated.csv"
  write.csv(final_isolated,paste0(wd,"/",fho_iso))
  cat(rep("\n",2),"Your results are saved as\n\n",fho_iso,"\n\n in","\"",wd,"\"","\n\n showing just",col_final,"data",rep("\n",2))
}

## Anomalies
### Here are the entries that contain the default term "<Go to ISI>" in the title column from the original data 
if(anomalies==1){
  gti <- "<Go to ISI>"
  search_func_gti <- grep(gti, tt[,"Title"], ignore.case = T)
  final_gti <- tt[search_func_gti,] #
  length(final_gti[,"Title"]) # get number of results
  tt[final_gti[,"Title"],"Title"] # confirm against raw data
  cat("Rows from original data where entries occur:\n",search_func_gti) # rows from original data where entries occur

  ### Remove NA columns  
  rm_na <- grep("NA", names(tt), ignore.case = F)
  final_gti[,colnames(final_gti[,rm_na])] <- list(NULL)
  
  ### Save anomalies to file
  fho_isi <- "keyword_final_anomalies.csv" # save these anomalies to local dir
  write.csv(final_gti,paste0(wd,"/",fho_isi))
  cat(rep("\n",2),"Your results are saved as\n\n",fho_isi,"\n\n in","\"",wd,"\"",rep("\n",2))
}

# final 
cat(rep("\n",2),"Your results are saved as\n\n",fho,"\n\n in","\"",wd,"\"","\n\n using the following search terms:\n\n",keyterms_neat,rep("\n",2),
"Total papers when searching",col2search,":",length(final[,col2search]))

# q()

# ------------------- lab 
# tt <- read.delim(paste0(ww,"/LEC100testrecords.txt"),sep=" ");str(tt)
# colnames(tt) <- as.character(1:40) # when sep=" "

# potential alternative grep function
# https://stackoverflow.com/questions/5823503/pattern-matching-using-a-wildcard/5823670

## dplyr approach 
# final_d <- filter(tt, grepl(keyterms, tt[,col2search]))

# some problem entries for optimisation 
# tt[10,]
# tt[57,]

