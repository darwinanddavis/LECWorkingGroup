# Quantifying effects of parasites on ecosystem nutrient cycling  
    
<img src="https://raw.githubusercontent.com/darwinanddavis/LECWorkingGroup/master/lec.jpeg" alt=" " width=1000 height=500>  

**[Living Earth Collaborative Center for Biodiversity](https://livingearthcollaborative.wustl.edu/) Working Group**    
**Washington University**    
**St. Louis, MO, USA**       
**December 4-7, 2018 and May 5–10, 2019**         

Participants:  
Amanda Koltz (co-organizer), Washington University in St. Louis, USA  
Rachel Penczkowski (co-organizer), Washington University in St. Louis, USA  
Sharon Deem (co-organizer), Institute for Conservation Medicine, St. Louis Zoo, USA  
Vanessa Ezenwa (co-organizer), University of Georgia, USA  
Susan Kutz, University of Calgary, Canada   
Brandon Barton, Mississippi State University, USA  
Zoe Johnson, Mississippi State University, USA  
Aimee Classen, University of Vermont, USA  
J. Trevor Vannatta, Purdue University, USA  
Matt Malishev, Emory University, USA  
David Civitello, Emory University, USA  
Daniel Preston, University of Wisconsin-Madison, USA  
Maris Brenn-White, St. Louis Zoo, USA    

## Overview    

Develop a predictive framework for estimating indirect effects of parasites on ecosystem nutrient cycling. Ideas behind this research were developed at the Living Earth Collaborative working group on the effects of parasites on ecosystem nutrient cycling, Washington University, St. Louis, MO, USA, December 4-7, 2018.      

## Instructions for running the model    

:one: [Download `R`](https://cran.r-project.org/mirrors.html) and [`RStudio`](https://www.rstudio.com/products/rstudio/download/) for your operating system.        
:two: [Download the model file (right click here and 'Save link as')](https://github.com/darwinanddavis/LECWorkingGroup/raw/master/NPSI.R?raw=true) and run the simulation model in `RStudio`.  
:three: Follow the instructions at the beginning of the `R` file to run the model.        

## Instructions for keyword scrape bot    

The bot reads a .txt file of literature entries resulting from a keyword search term query output from a Web of Science database search and returns a new file containing user defined search terms.  

Required files:  

LEC100testrecords.txt  
search_term_inputs.txt  
article_col_names.txt  
lec_keyword_search.R   
lec_keyword_search.pdf    

:one: Enter either Title or Abstract to search for the keywords.    

```{r}
extract1 <- "Title" 
```  

:two: Enter what data you want to get out of the final results. For example, if you want to know what year in which the resulting papers were published, type in "_Year_".  

Use any search term you specified in the _search_terms_input.txt_ file.  

```{r}
extract2 <- "Year"  
```  

:three: Follow the instructions in the _lec_keyword_search.pdf_ file.    

## Instructions for meta-analysis scrape bot  

The bot scrapes PDF articles (.pdf) based on user defined search terms and returns a local file of meta-analysis data.  

Example search terms:  

_mortality, surviv*, fecund*, body condit*, body, body mass, feed*, feeding rate, feeding amount, waste, faec*, fece*, urin*, ecosystem, plant, soil, nutrie*_  

Example outputs from meta-analysis:    

```{r}
"Relevance",
"Parasite type",
"Response variable",
"Effect variance",
"Sample size",
"P val"
```    

Required files:  

metaanalysis_readpdf.R  
title_abstract_terms.txt    

:one: Follow the instructions in the _metaanalysis_readpdf.R_ model file.           
:two: Set the working path to the dir containing the PDF files for scraping  
    
## :pig: Troubleshooting  

All troubleshooting and bugs can be sent as a git issue or to matthew.malishev [at] emory.edu.     

## References  

## Maintainer  
**Matt Malishev**   
:mag: [Website](https://www.researchgate.net/profile/Matt_Malishev)    
:bird: [@darwinanddavis](https://twitter.com/darwinanddavis)  
:email: matthew.malishev [at] gmail.com    

