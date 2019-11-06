# extract text from pdfs

### before running ###
# 1. delete first summary page of PDF if present
# 2. set search terms for each output 

#################################### load packages
packages <- c("pdftools","tidyverse","stringr") 
if (require(packages)) {
  install.packages(packages,dependencies = T)
  require(packages)
}
ppp <- lapply(packages,require,character.only=T); if(any(ppp==F)){cat("\n\n\n ---> Check packages are loaded properly <--- \n\n\n")}

#################################### set wd
wd <- "/Users/malishev/Documents/Emory/research/workshops/stl/journal_data/meta_analysis/"
setwd(wd)

colC <- list() # create empty list for relevance data (colC in LEC data extraction sheet)
unreadable_pdf <- list() # list for storing unreadable pdfs
colC_names <- c("PaperID","C")

# set col names for output 
rv_names <- c(
  "Relevance",
  "Line number",
  "Parasite type",
  "Line number",
  "Response variable",
  "Line number",
  "Effect variance",
  "Line number",
  "Sample size",
  "Line number",
  "P val",
  "Line number"
)
rv_names %>% class

rv_names <- paste(
  "Relevance",
  
  "Parasite type",
  
  "Response variable",
  
  "Effect variance",
  
  "Sample size",
  "P val",sep=",Line number,"
)


#################################### read in pdfs from dir  
f <- 1
file_list<-list.files(paste0(wd,"matts_pdfs/")) # list files
file_list
pdf_list<-as.list(rep(1,length(file_list))) # empty list
for (f in 1:length(pdf_list)){ # read in pdfs
  p <- pdf_text(paste0(wd,"matts_pdfs/",file_list[f])) # read pdf
  p <- read_lines(p) # convert to txt file
  pdf_list[[f]]<-p# save to master pdf list  
}
names(pdf_list) <- file_list # name list elements as paprer IDs
file_list # show files in dir
if(length(pdf_list)!=length(file_list)){cat("\n\n Check all pdfs in directory have been loaded \n\n")}

######################################## choose paper to scrape 
# loop through files in pdf dir 
nn <- 1

for(nn in 1:length(file_list)){ 
  fh <- file_list[nn]; fh 
  # only scrape PDFs with content that can be read
   if(pdf_list[fh][[fh]][1]==""){ # if the first line is empty
     nn <- nn + 1
     fh <- file_list[nn]
     unreadable_pdf[nn] <- fh # add troublesome pdfs to list to remove 
     unreadable_pdf <- unlist(unreadable_pdf); unreadable_pdf <- unreadable_pdf[!is.na(unreadable_pdf)]
   }
   cat(fh,"\n",nn,"\n")
     
  
  ######################################## make response variable df
  # rv <- data.frame(#row.names=file.list,
  #                  "PaperID"=file_list,
  #                  "C"=character(length=length(file_list)),
  #                  "Host genus"=character(length=length(file_list)),
  #                  "Host species"=character(length=length(file_list)),
  #                  "Parasite type"=character(length=length(file_list)),
  #                  "Response variable"=character(length=length(file_list)),
  #                  "Sample size"=numeric(length=length(file_list)),
  #                  "Direction of response to more infection"=numeric(length=length(file_list)),
  #                  "Effect size type"=numeric(length=length(file_list)),
  #                  "Value"=numeric(length=length(file_list)),
  #                  "Variance type"=numeric(length=length(file_list)),
  #                  "Variance"=numeric(length=length(file_list)),
  #                  "lower CI"=numeric(length=length(file_list)),
  #                  "upper CI"=numeric(length=length(file_list)),
  #                  "Total sample size (N)"=numeric(length=length(file_list)),
  #                  "df"=numeric(length=length(file_list)),
  #                  "ndf"=numeric(length=length(file_list)),
  #                  "ddf"==numeric(length=length(file_list)),
  #                  "p-val"=numeric(length=length(file_list)),
  #                  "N infected"=numeric(length=length(file_list)),
  #                  "N reduced or no infection"=numeric(length=length(file_list)),
  #                  stringsAsFactors=F)
  
  ###### more relevant data frame 
  # rv <- data.frame(#row.names=file.list,
  #   "PaperID"=fh,
  #   "Relevance"=character(length=length(fh)),
  #   "Parasite type"=character(length=length(fh)),
  #   "Effect variance"=character(length=length(fh)),
  #   "Sample size"=numeric(length=length(fh)),
  #   "Variance type"=numeric(length=length(fh)),
  #   "P val"=numeric(length=length(fh)),
  #   stringsAsFactors=F)
  
  #################################### read in title or abstract terms
  title_abstract_terms <- as.character(read.delim("title_abstract_terms.txt",header=F,strip.white = T,sep=",",colClasses = "character")[1,])
  title_abstract_terms <- as.character(title_abstract_terms);title_abstract_terms_neat <- title_abstract_terms
  title_abstract_terms <- paste(title_abstract_terms,collapse="|");title_abstract_terms_neat # combine all the search terms 
  
  single_paper <- 1 # read in files from dir or individual papers
  if(single_paper==1){
    p1 <- pdf_list[[fh]] # read in individual paper 
  
    # ID paper, Nth line
    line_number <- 100
    p1[line_number] 
  }
  
  # isolate title and abstract in paper
  ta_length <- 1:50 # set number of lines for title and abstract in pdf
  p1_ta <- p1[ta_length] # first 80 lines of pdf 
  
  # remove whitespace
  str_squish(p1)
  
  ########################################    1. check whether title and abstract has key terms
  if(single_paper==0){
    for(kt in file_list){
      # this is for the master pdf
      relevance_return <- grep(title_abstract_terms,pdf_list[[kt]][ta_length],ignore.case = T) # scrape title and abstract for terms in protocol doc
      }
    }else{
      # this is for reading individual files
      relevance_return <- grep(title_abstract_terms,p1_ta,ignore.case = T) # scrape title and abstract for terms in protocol doc
    } # end single_paper == 0
    
  # if(length(relevance_return)<1){ # if search terms return nothing, set data for that paper to NAs 
    #   rv_sub <- subset(rv,"PaperID"==kt) 
    #   rv_sub[,3:length(rv_sub)] <- NA
    #   rv_sub$Relevance <- "No"
    #   # replace rv with this kt with rv sub values (NAs)
    #   } 
  if(length(relevance_return)>1){ # if search terms return nothing, set data for that paper to NAs 
    relevance_final <- "YES"
  }else{
    relevance_final <- "MAYBE"}
  
  ########################################    2. scrape files for data  ########################################
  
  ######################################### parasite type
  nematode_terms <- as.character(c("nemat*","Nemat*","helmin*","Helmin*","cestod*","Cestod*","tremat*","Tremat*"));nematode_terms_neat <- nematode_terms
  nematode_terms <- paste(nematode_terms,collapse="|");nematode_terms_neat # combine all the search terms 
  
  #### choose paper
  p1_nematode <- pdf_list[[fh]] # read in individual paper 
  
  # this is for reading individual files
  nematode_return <- grep(nematode_terms,p1_nematode,ignore.case = T) # scrape title and abstract for terms in protocol doc
  nematode <- p1_nematode[nematode_return] # return terms in paper
  # nematode <- p1_nematode[nematode_return[1]] # get just first instance of term 
  # nematode_final <- paste( unlist(nematode), collapse='') # turn into one character string
  # nematode_final <- strsplit(nematode_terms,nematode_text) # remove everything but the key term (in progress ... )
  # nematode_final[[1]] # turn into char
  
  ######################################### response variable
  response_terms <- as.character(c("BCI","bci","body mass","bodymass","weig*","feedi*","feeding rate","uptak* rat*","mortal*","surviv*","defeca*","fecun*","urinat*","nutrie*","soil","plant biomass"));response_terms_neat <- response_terms
  response_terms <- paste(response_terms,collapse="|");response_terms_neat # combine all the search terms 
  
  #### choose paper
  p1_response_var <- pdf_list[[fh]] # read in individual paper 
  
  # this is for reading individual files
  response_var_return <- grep(response_terms,p1_response_var,ignore.case = T) # scrape title and abstract for terms in protocol doc
  response <- p1_response_var[response_var_return] # return terms in paper
  
  ######################################### effect size variance 
  effect_var_terms <- c("SE", "SD", "CI"); effect_var_terms_neat <- effect_var_terms
  effect_var_terms <- paste(effect_var_terms,collapse="|")
  
  p1_effect <- pdf_list[[fh]] # select paper
  
  effect_var_return <- grep(effect_var_terms,p1_effect,ignore.case = F) # scrape title and abstract for terms in protocol doc
  effect_var <- p1_effect[effect_var_return] # return all outputs
  # effect_var_final <- paste(unlist(effect_var), collapse='')# turn into one character string
  
  ######################################### sample size 
  n_val_terms <- c("n =", "n=", "N =", "N="); n_val_terms_neat <- n_val_terms
  n_val_terms <- paste(n_val_terms,collapse="|")
  
  p1_nval <- pdf_list[[fh]] # select paper
  
  nval_return <- grep(n_val_terms,p1_nval,ignore.case = F) # scrape title and abstract for terms in protocol doc
  nval <- p1_nval[nval_return] # return all outputs
  # pval_final <- paste(unlist(pval), collapse='')# turn into one character string
  
  
  ######################################### p value 
  p_val_terms <- c("p =", "p=", "p <", "p<", "p >", "p>","P =", "P=", "P <", "P<", "P >", "P>"); p_val_terms_neat <- p_val_terms
  p_val_terms <- paste(p_val_terms,collapse="|")
  
  p1_pval <- pdf_list[[fh]] # select paper
  
  pval_return <- grep(p_val_terms,p1_pval,ignore.case = F) # scrape title and abstract for terms in protocol doc
  pval <- p1_pval[pval_return] # return all outputs
  # pval_final <- paste(unlist(pval), collapse='')# turn into one character string
  
  ######################################## save output
  
  out_list <- list(relevance_final,relevance_return, nematode, nematode_return, response, response_var_return, effect_var, effect_var_return, nval, nval_return, pval, pval_return) # merge results into list
  n.obs <- sapply(out_list, length) # get number of obs for each element
  seq.max <- seq_len(max(n.obs)) # fill in missing number of cols to make below matrix the same length
  rv <- sapply(out_list, "[", i = seq.max) # turn into matrix
  rv <- as.data.frame(rv) # convert to df
  colnames(rv) <- rv_names # name cols 
  
  # write out to dir
  fout <- paste0(as.character(strsplit(fh,".pdf")),".csv") # create file name 
  write.csv(rv,fout) # save to dir 
  
  # # write relevance file
  # colC[nn] <- relevance_final # column in LEC data extraction worksheet
  # # colC <- sapply(colC, "[", i = length(colC)) # turn into matrix
  # colC_final <- data.frame("PaperID" = fh,"C" = colC) # convert to df
  # colnames(colC_final) <- colC_names # name cols 
  # write.csv(colC_final,"relevance.csv") # save to dir 
  
} # end loop
unreadable_pdf # pdfs that can't be read 

