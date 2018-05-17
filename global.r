library(shiny)
library(DT)
library(shinydashboard)
library(stringr)
library(timeSeries)
library(knitr)
library(ggplot2)   
library(tools)
library(FirebrowseR)


#######
# DB  #
#######

synonyms = read.csv('data/cancer_types.csv', header = TRUE,sep="\t")
cohorts = read.table('data/tcga_cohorts.csv', header = FALSE,sep=",",stringsAsFactors=F)

##############
# FUNCTIONS  #
##############
myFun <- function(n = 5000) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}

cosmicLink <- function(gene,var) {
  var2= sub("\\*","",var)
  paste('<a href="http://cancer.sanger.ac.uk/cosmic/search?q=',paste(gene,var2,sep="+AND+"),'" target="_blank">',var,'</a>')
  
}

geneLink  <- function(gene) {
  if (length(gene)==0){return(NULL)}
  paste(paste('<a href="https://pmkb.weill.cornell.edu/search?utf8=✓&search=',gene,'" target="_blank">','PMKB','</a>',sep=""),
  paste('<a href="http://dgidb.genome.wustl.edu/genes/',gene,'" target="_blank">','DGIdb','</a>',sep=""),sep=",")
}

ctLink  <- function(ct) {
  if (length(ct)==0){return(NULL)}
  paste('<a href="https://clinicaltrials.gov/ct2/show/',ct,'" target="_blank">',ct,'</a>',sep="")

}

refLink <- function(ref) {
  if (length(ref)==0){return(links = c())}
  links = c()
  for (x in 1:length(ref)){
    refs=strsplit(ref[x],",")[[1]]
    for (i in 1:length(refs)){
      if (grepl("^\\d",refs[i])){
        links[x] = paste('<a href="https://www.ncbi.nlm.nih.gov/pubmed/?term=',refs[i],'" target="_blank">',refs[i],'</a>')
      }
      if (grepl("ASCO",refs[i])){
        num=strsplit(gsub("[^0-9]","",refs[i]),split = "")[[1]]
        year=num[1:4]
        year=paste(year,collapse = "")
        abstr=num[-(1:4)]
        abstr=paste(abstr,collapse = "")
      #  links[x] = paste('<a href="http://ascopubs.org/doi/abs/10.1200/jco.',year,'.33.15_suppl.',abstr,'" target="_blank">',refs[i],'</a>')
        links[x] = paste('<a href="https://scholar.google.com/scholar?q=%20',paste("ASCO",year,"(abstr",abstr,")",sep="+"),'%20&btnG=&hl=es&as_sdt=0%2C5','" target="_blank">',refs[i],'</a>')
        
      }
      if (grepl("AACR",refs[i])){
        num=strsplit(gsub("AACR| |\\(abstr|\\(abstract|\\)","",refs[i]),split = "")[[1]]
        year=num[1:4]
        year=paste(year,collapse = "")
        abstr=num[-(1:4)]
        abstr=paste(abstr,collapse = "")
       # links[x] = paste('<a href="http://cancerres.aacrjournals.org/search/abstract%252B',abstr,'%20pubyear%3A',year,'%20numresults%3A10%20sort%3Arelevance-rank%20format_result%3Astandard','" target="_blank">',refs[i], '</a>')
        links[x] = paste('<a href="https://scholar.google.com/scholar?q=%20',paste("AACR",year,"(abstr",abstr,")",sep="+"),'%20&btnG=&hl=es&as_sdt=0%2C5','" target="_blank">',refs[i],'</a>')
      }
      if (grepl("FDA",refs[i])){
        links[x] = paste('<a href="https://www.fda.gov/Drugs/ScienceResearch/ResearchAreas/Pharmacogenetics/ucm083378.htm','" target="_blank"> FDA </a>')
      }
      if (!grepl("^\\d|ASCO|AACR|FDA",refs[i])){
        links[x] = paste('<a href="https://scholar.google.com/scholar?q=',gsub(" ","+",refs[i]),'&btnG=&hl=es&as_sdt=0%2C5','" target="_blank">',refs[i],'</a>')
      }
    }
  }
  return(links)
}

helpPopup <- function(title, content,
                      placement=c('right', 'top', 'left', 'bottom'),
                      trigger=c('click', 'hover', 'focus', 'manual')) {
  tagList(
    singleton(
      tags$head(
        tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })")
      )
    ),
    tags$a(
      href = "#", class = "btn btn-mini", `data-toggle` = "popover",
      title = title, `data-content` = content, `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1],
      
     # tags$i(class="icon-question-sign")
      icon("info-circle")
    )
  )
}

