## Load helper functions
source("helpers/get_druggable.r")
source("helpers/get_levels.r")

shinyServer(function(input, output, session) {


  ########################################################
  ######### UPLOAD TAB ###################################
  ########################################################

  # Button to go to "upload" tab
  observeEvent(input$upload_button, {
    updateTabItems(session, "tab", "upload")
  })
  
  # Button to go to "tcga" tab
  observeEvent(input$tcga_button, {
    updateTabItems(session, "tab", "tcga")
  })
  
  ###############
  ## REACTIVES ##
  ###############

  # Genomic Upload User
  UPLOAD1 = reactive({
    typeof(input$inputSNV)
     inFile <- input$inputSNV
     if (is.null(inFile)){return(NULL)}
     read.csv(inFile$datapath, sep=input$sep, col.names = c("Gene","Variant Type","aa change",input$fields), header=input$header1)
   })

  UPLOAD2 = reactive({
    inFile2 <- input$inputCNV
    if (is.null(inFile2)){return(NULL)}
    read.csv(inFile2$datapath,  sep=input$sep2, col.names = c("Gene","CN Type",input$fields2), header=input$header2)
  })

  UPLOAD3 = reactive({
    inFile3 <- input$inputTX
    if (is.null(inFile3)){return(NULL)}
    read.csv(inFile3$datapath,sep=input$sep3,col.names = c("Gene1","Gene2"), header=input$header3)
  })

  UPLOAD_DB = reactive({
    if (input$gdkd=='no'){GDKD = 'no';v_gdkd="not used"}
    if (input$gdkd=='default'){GDKD = read.delim("data/GDKD.csv",sep="\t");v_gdkd="v20.0"}
    if (input$gdkd=='other'){
      if (is.null(input$file_gdkd)){return(NULL)}
      GDKD=read.xlsx2(input$file_gdkd$datapath, header=TRUE, sheetIndex = 1,colIndex = c(1:45))
      # Small changes
      GDKD$Gene  = gsub(" ", "", GDKD$Gene)
      for (c in grep("PMID",colnames(GDKD))){
        GDKD[,c] = gsub("\\^|_","",GDKD[,c])
        gdkd_agg   = aggregate(GDKD, by = list(GDKD$Disease,GDKD$Gene,GDKD$Description,GDKD$Association_1,GDKD$Therapeutic.context_1),
                               FUN = function(X) paste(unique(X), collapse=", "))[,6:50]
        GDKD=gdkd_agg
        v_gdkd=input$file_gdkd$name
      }
    }
    if (input$civic=='no'){CIVIC = 'no';v_civic="not used"}
    if (input$civic=='default'){CIVIC = read.delim("data/CIViC.csv",sep="\t");v_civic="01-May-2018"}
    if (input$civic=='other'){
      if (is.null(input$file_civic)){return(NULL)}
      CIVIC = read.delim(input$file_civic$datapath, header=T,stringsAsFactors = F,sep="\t",quote = "")
      CIVIC <- CIVIC[which(CIVIC$evidence_type=="Predictive"),]
      civic_agg  = aggregate(CIVIC, by = list(CIVIC$gene,CIVIC$disease,CIVIC$drugs,CIVIC$evidence_level,CIVIC$clinical_significance),
                             FUN = function(X) paste(unique(X), collapse=", "))[,6:41]
      CIVIC=civic_agg
      v_civic=input$file_civic$name
      
    }
    return(list(GDKD,CIVIC,v_gdkd,v_civic))

  })
  
  ## Druggable
  DruggableUPLOAD = eventReactive(input$action,{
    if (input$action > 0 && (!is.null(UPLOAD1()) || !is.null(UPLOAD2())|| !is.null(UPLOAD3()) ) ) {
      #Cancer type
      cancer <- input$cancer
      cancer_GDKD <-  unique(as.character(na.omit(synonyms[grep(cancer,synonyms$Shiny),"knowledge"])))
      cancer_CIVIC<- sapply(cancer,function(x) as.character(na.omit(synonyms[grep(x,synonyms$Shiny,ignore.case = T),"civic"])[1]))
      cancer_CIVIC <- paste(cancer_CIVIC,collapse = ",")
      cancer_CIVIC <- unique(strsplit(cancer_CIVIC,",")[[1]])


      #### MAF & CNV
      SNV <- UPLOAD1()[,1:3]
      CNV <- UPLOAD2()[,1:2]
      TX <- UPLOAD3()[,1:2]
      if (!is.null(TX)){TX <- apply(TX,1,paste,collapse="-")}
      GDKD <- UPLOAD_DB()[[1]]
      CIVIC <- UPLOAD_DB()[[2]]
      
      
      #### GDKD DB
      druggableGDKD = data.frame()
      if(GDKD != 'no'){
      druggableGDKD = match_SNV_GDKD(SNV,GDKD)
      druggableGDKD = rbind(druggableGDKD,match_CNV_GDKD(CNV,GDKD))
      druggableGDKD = rbind(druggableGDKD,match_WT_GDKD(SNV,CNV,cancer_GDKD,GDKD),match_TX_GDKD(TX,GDKD))
      rownames(druggableGDKD) = NULL
      }
      #### CIVIC
      druggableCIVIC = data.frame()
      if(CIVIC != 'no'){
      druggableCIVIC           = match_SNV_CIVIC(SNV)
      druggableCIVIC           = unique(rbind(druggableCIVIC,match_CNV_CIVIC(CNV)))
      rownames(druggableCIVIC) = NULL
      }
      
      #### TARGET DB
      druggableTARGET = data.frame()
      druggableTARGET = match_TARGET_MERIC(SNV,CNV,TX)
      
      #########################################################
      ## Classify filtered variants by Levels of Evidence    ##
      #########################################################
      
      ### KNWOLEDGE
      levelsGDKD = c()
      levelsGDKD = get_levels_GDKD(druggableGDKD,cancer_GDKD)
      #print(levelsGDKD)
      
      ### CIVIC
      levelsCIVIC = c()
      levelsCIVIC = get_levels_CIVIC(druggableCIVIC,cancer_CIVIC)
      #print(levelsCIVIC)
      
      levels = merge_levels(levelsGDKD,levelsCIVIC)
     # print(levels)
      # Homogeneize/clean the final table
      table = clean_levels(levels,synonyms,sort_by="drug_freq")
      #print(table)
      return(list(druggableTARGET,druggableGDKD,druggableCIVIC,table))
    } 
    else{return()}
  })
  
  
  ## Formatting output tables
  outUPLOAD <- reactive({
    L       = c(input$LevelAUPLOAD,input$LevelBUPLOAD)
    validate(need(try(!is.null(L)),"Select one level"))
    pattern = paste(L,collapse = "|")
    rows    = grep(pattern,DruggableUPLOAD()[[4]]$level)
    if(length(rows)!=0){
      df           = DruggableUPLOAD()[[4]][rows,]
      df$`Pat Var` = gsub("NEW","<font color='red'>",df$`Pat Var`)
      Links        = geneLink(df$Gene)
      df$Gene      = paste(df$Gene," (",Links,")",sep="")
      Ref          = refLink(df$Ref)
      df           = cbind(df[,1:7],Ref,df[,"level"])
      colnames(df) = colnames(DruggableUPLOAD()[[4]])
      res          = grep("resistance|no|decrease",unique(df[,"Predicts"]),value=TRUE)
      sens         = grep("resistance|no|decrease",unique(df[,"Predicts"]),value=TRUE,invert = T)

      datatable(df,selection="single",rownames=F,escape=F,options=list(scrollX = TRUE)) %>%
        formatStyle('Predicts',
          backgroundColor = styleEqual(c(sens,res), c(rep('#66b3ff',length(sens)),rep('#FF704D',length(res))))
          ) %>%
        formatStyle('level',
          backgroundColor = styleEqual(c("A1","A2","A3","B1","B2","B3"), c('#1A237E','#1976D2','#81D4FA','#1B5E20','#4CAF50','#AED581'))
          )
    }
    else{return()}
  })

  
  
  #############
  ## OUTPUTS ##
  #############

  output$mafUPLOAD <- DT::renderDataTable({
   # if(is.null(UPLOAD1())){return(datatable(data.frame(c("No SNVs have been provided yet")),rownames=F,colnames=""))}
    validate(need(try(!is.null(UPLOAD1())),"No SNVs have been provided yet"))

    datatable(cbind(UPLOAD1()[,c(1,2)],
                    cosmicLink(UPLOAD1()[,1],UPLOAD1()[,3]),
                    UPLOAD1()[,setdiff(1:ncol(UPLOAD1()),1:3)]),
      colnames   = colnames(UPLOAD1()),
      selection  = "single",  
      escape     = FALSE ,
      rownames   = F,
      options    = list(lengthMenu = list(c( 10, 20, 30, -1), c("10", "20", "30", "All")), 
      pageLength = 5))
  })
  output$cnvUPLOAD <- DT::renderDataTable({
   # if(is.null(UPLOAD2())){return(datatable(data.frame(c("No CNVs have been provided yet")),rownames=F,colnames=""))}
    validate(need(try(!is.null(UPLOAD2())),"No CNVs have been provided yet"))

    datatable(UPLOAD2(),
      selection  = "single",
      rownames   = F,
      options    = list(lengthMenu = list(c(5, 10, 25, -1), c("5", "10", "25", "All")),
      pageLength = 5))
  })
  output$txUPLOAD <- DT::renderDataTable({
   # if(is.null(UPLOAD3())){return(datatable(data.frame(c("No fusions have been provided yet")),rownames=F,colnames=""))}
    validate(need(try(!is.null(UPLOAD3())),"No fusions have been provided yet"))

    datatable(UPLOAD3(),
      selection  = "single",
      rownames   = F,
      options    = list(lengthMenu = list(c(5, 10, 25, -1), c("5", "10", "25", "All")), 
      pageLength = 5))
  })
  
  ## Figure
  output$figureUPLOAD <- renderPlot({

    validate(need(try(!is.null(DruggableUPLOAD()[[4]]) &&  nrow(DruggableUPLOAD()[[4]])!=0),
     "Sorry, no actionable variants were found."))

    A                     = DruggableUPLOAD()[[4]]
    df                    = expand.grid(c("1.Approved","2.Clinical","3.Preclinical"), c("B.Other\nCancers","A.Same\nCancer"))
    df$value              = c(nrow(A[A$level=="B1",]),
                              nrow(A[A$level=="B2",]),
                              nrow(A[A$level=="B3",]),
                              nrow(A[A$level=="A1",]),
                              nrow(A[A$level=="A2",]),
                              nrow(A[A$level=="A3",]))
    df$value[df$value==0] = NA
    df$value              = as.numeric(df$value)
    
    g = ggplot(df, aes(Var1, Var2)) +
      geom_point(aes(size = value), colour = c("#1B5E20","#4CAF50","#AED581","#1A237E","#1976D2","#81D4FA")) +
      theme_classic() + 
      xlab("") + 
      ylab("")
    g = g + 
      labs(title="     FINDINGS BY LEVEL")+
      scale_size_continuous(range=c(5,10)) + 
      geom_text(
        data = df, 
        aes(x = Var1, y = Var2, label = value),
        size = 5, 
        vjust = 0.5, 
        hjust = 0.5,
        colour="white",
        fontface="bold") + 
      theme(
        plot.title = element_text(size=10),
        legend.position="none",
        axis.text.x = element_text(size=10,angle = 45,hjust = 1),
        axis.text.y.right = element_text(size=10,angle = 45,vjust = 1), 
        plot.margin = margin(0.5, 0, 0, 1, "cm"),
        plot.background = element_rect(fill = "#f4f4f4",colour = "#8c8c8c",size =1))+
      scale_y_discrete(position = "right") 
      coord_fixed(ratio = 0.75)
    g
    
  })
  
  ## Target targetUPLOAD
  output$othersUPLOAD = DT::renderDataTable({
    
    validate(need(try(!is.null(DruggableUPLOAD()[[1]]) && nrow(DruggableUPLOAD()[[1]])!=0 ), "No other actionable variants found"))
    
    datatable(DruggableUPLOAD()[[1]],
      selection = "single",
      rownames  = F,
      escape    = F,
      options=list(scrollX = TRUE)) 
  })
  
  ## By Levels
  output$UPLOAD = DT::renderDataTable({
   # validate(need(try(outUPLOAD() != "" ), ""))
    validate(need(try(DruggableUPLOAD()[[4]] != ""), "No actionable variants found"))
   
    outUPLOAD()
    })

  ## Download PDF
  output$reportUPLOAD = downloadHandler(
    filename = paste(input$ID,'_MTBReport.pdf',sep=''),
    content  = function(file) {
      wd = getwd()
      f = paste(wd,"/tmp/",sep="")
      s = myFun(1)
      report = file.path(wd, "/helpers/Report_UPLOAD-knitr.Rnw")
      #Sweave2knitr(report,output = paste(f,s,".Rnw",sep=""))     
      knit(report,  output =paste(f,s,".tex",sep="") )
      setwd(f)
      tryCatch(texi2pdf(paste(f,s,".tex",sep=""),clean=T), error=function(e){write.csv(paste(e,paste(f,s,".tex",sep="")), file)})
      file.copy(paste(f,s,".pdf",sep = ""), file, overwrite = TRUE)
      unlink(paste(f,s,".Rnw",sep = ""))
     # unlink(paste(f,s,".tex",sep=""))
      unlink(paste(f,s,".pdf",sep = ""))
      setwd(wd)
    }
  )  

  ## Download CSV
  output$csvUPLOAD = downloadHandler(
    filename = paste(input$ID,'_MTBReport.csv',sep=''),
    content = function(file) {
      write.csv(DruggableUPLOAD()[[4]], file,row.names = F)
  })    

  ######################################################
  ######### TCGA TAB ###################################
  ######################################################
  output$x1 = DT::renderDataTable({
    datatable(cohort_table(),
              rownames = F,
              selection = 'single', 
              options=list(lengthMenu = list(5, c("5")),pageLength=5,scrollX = TRUE)) %>% 
    formatStyle(names(cohort_table()), cursor = 'pointer')
  })

  ###############
  ## REACTIVES ##
  ###############
  ## Table cohort
  cohort_table = reactive({
    Samples.Clinical(format = "csv",  cohort=strsplit(input$cohort," - ")[[1]][1],page_size = 25000)[,grep("tcga_participant_barcode|histological_type|pathologic_stage|clinical_stage|acute_myeloid_leukemia_calgb_cytogenetics_risk_category",
                                                                                                           colnames(Samples.Clinical(format = "csv",  cohort=strsplit(input$cohort," - ")[[1]][1],page_size = 25000)))]
  })

  ## Clinical
  clinical = reactive({
    r = input$x1_rows_selected
    if (!is.null(r)) {
      patient = cohort_table()[r,1]
      clinical_data=Samples.Clinical(format = "csv",tcga_participant_barcode = patient)
      return(as.vector(clinical_data[1,]))
    }
  })

  ## Genomic
  TCGA = reactive({
    r = input$x1_rows_selected
    if (!is.null(r)) {
      patient = cohort_table()[r,1]
      maf=try(Analyses.Mutation.MAF(format = "csv",
                                   tcga_participant_barcode = patient)[,c("Hugo_Symbol","Variant_Classification","Protein_Change")],silent=T)
      cnv = try(Analyses.CopyNumber.Genes.Thresholded(format = "csv",
                                                   tcga_participant_barcode = patient)[,c("gene","cn_alteration")],silent=T)
      if(is.data.frame(maf)){
      maf$Protein_Change=gsub("p.","",maf[,"Protein_Change"])
      } else {maf=NULL}
      if(is.data.frame(cnv)){
      cnv[cnv[,2]==-1,2]<-"low level deletion"
      cnv[cnv[,2]==1,2]<-"low level amplification"
      cnv[cnv[,2]==-2,2]<-"biallelic inactivation"
      cnv[cnv[,2]==2,2]<-"biallelic amplification"
      cnv                 = cnv[which(cnv[,2] != 0),]
      cnv                 = unique(cnv)
      } else {cnv=NULL}
      return(list(maf,cnv))
    }
  })

  ## Druggable
  Druggable = eventReactive(input$action2,{
    r = input$x1_rows_selected
    if (!is.null(r) && (!is.null(TCGA()[[1]]) || !is.null(TCGA()[[2]]) )) {
 #     if (input$action > 0 && (!is.null(UPLOAD1()) || !is.null(UPLOAD2())|| !is.null(UPLOAD3()) ) ) {
      #Cancer type
      cancer      <- strsplit(input$cohort," - ")[[1]][1]
      cancer_GDKD      = unique(as.character(synonyms[grep(cancer,synonyms$tcga_cancer,ignore.case = T),"knowledge"]))
      cancer_CIVIC     = sapply(cancer,function(x) as.character(na.omit(synonyms[grep(x,synonyms$tcga_cancer,ignore.case = T),"civic"])[1]))
      cancer_CIVIC     = paste(cancer_CIVIC,collapse = ",")
      cancer_CIVIC     = unique(strsplit(cancer_CIVIC,",")[[1]])


      #### MAF & CNV
      SNV <- TCGA()[[1]]
      CNV <- TCGA()[[2]]
      #### GDKD DB
      druggableGDKD = data.frame()
      druggableGDKD = match_SNV_GDKD(SNV)
      druggableGDKD = rbind(druggableGDKD,match_CNV_GDKD(CNV))
      druggableGDKD = rbind(druggableGDKD,match_WT_GDKD(SNV,CNV,cancer_GDKD))
      rownames(druggableGDKD) = NULL

      #### CIVIC
      druggableCIVIC = data.frame()
      druggableCIVIC           = match_SNV_CIVIC(SNV)
      druggableCIVIC           = unique(rbind(druggableCIVIC,match_CNV_CIVIC(CNV)))
      rownames(druggableCIVIC) = NULL
      
      
      #### TARGET DB
      druggableTARGET = data.frame()
      druggableTARGET = match_TARGET_MERIC(SNV,CNV)
      
      if( nrow(druggableGDKD) == 0 & nrow(druggableCIVIC) == 0 ) return(list(druggableTARGET,druggableGDKD,druggableCIVIC,data.frame()))
      
      #########################################################
      ## Classify filtered variants by Levels of Evidence    ##
      #########################################################
      
      
      ### KNWOLEDGE
      levelsGDKD = c()
      levelsGDKD = get_levels_GDKD(druggableGDKD,cancer_GDKD)
      #print(levelsGDKD)
      
      ### CIVIC
      levelsCIVIC = c()
      levelsCIVIC = get_levels_CIVIC(druggableCIVIC,cancer_CIVIC)
      #print(levelsCIVIC)
      
      levels = merge_levels(levelsGDKD,levelsCIVIC)
      
      # Homogeneize/clean the final table
      table = clean_levels(levels,synonyms,sort_by="drug_freq")
      print(table)
      return(list(druggableTARGET,druggableGDKD,druggableCIVIC,table))
    }
    else{return()}
  })

  ## Formatting output tables
  outTCGA <- reactive({
    L       = c(input$LevelA,input$LevelB)
    validate(need(try(!is.null(L)),"Select one level"))
    pattern = paste(L,collapse = "|")
    rows    = grep(pattern,Druggable()[[4]]$level)
    if(length(rows)!=0){
      df           = Druggable()[[4]][rows,]
      df$`Pat Var` = gsub("NEW","<font color='red'>",df$`Pat Var`)
      Links        = geneLink(df$Gene)
      df$Gene      = paste(df$Gene," (",Links,")",sep="")
      Ref          = refLink(df$Ref)
      df           = cbind(df[,1:7],Ref,df[,"level"])
      colnames(df) = colnames(Druggable()[[4]])
      res          = grep("resistance|no|decrease",unique(df[,"Predicts"]),value=TRUE)
      sens         = grep("resistance|no|decrease",unique(df[,"Predicts"]),value=TRUE,invert = T)

      datatable(df,selection="single",rownames=F,escape=F,options=list(scrollX = TRUE)) %>%
        formatStyle('Predicts',
          backgroundColor = styleEqual(c(sens,res), c(rep('#66b3ff',length(sens)),rep('#FF704D',length(res))))
          ) %>%
        formatStyle('level',
          backgroundColor = styleEqual(c("A1","A2","A3","B1","B2","B3"), c('#1A237E','#1976D2','#81D4FA','#1B5E20','#4CAF50','#AED581'))
          )
    }
    else{return()}
  })

  
  #############
  ## Outputs ##
  #############

  output$clinical= renderUI({
    HTML(
      paste("<br><b>",names(clinical())[grep("tcga_participant_barcode|age_at|gender|metastasis|pathologic_|histological_type|pathologic_stage|clinical_stage|vital_status|acute_myeloid_leukemia_calgb_cytogenetics_risk_category|KRAS|NRAS|BRAF|PIK3",names(clinical()))],": </b>",
             clinical()[,grep("tcga_participant_barcode|age_at|gender|metastasis|pathologic_|histological_type|pathologic_stage|clinical_stage|vital_status|acute_myeloid_leukemia_calgb_cytogenetics_risk_category|KRAS|NRAS|BRAF|PIK3",names(clinical()))]      )

    )
  })
  output$maf= DT::renderDataTable({
    validate(need(try(!is.null(TCGA()[[1]])),"No SNVs available for this patient"))
   # caption="Single nucleotide variants of the patient's tumor:",
    datatable(cbind(TCGA()[[1]][,c(1,2)],cosmicLink(TCGA()[[1]][,1],TCGA()[[1]][,3])),colnames = c("Gene","Variant Type","aa change"),
              selection="single",  escape = FALSE ,rownames=F,options=list(lengthMenu = list(c(5, 10, 25, -1), c("5", "10", "25", "All")), pageLength=5))
  })
  output$cnv= DT::renderDataTable({
    validate(need(try(!is.null(TCGA()[[2]])),"No SNVs available for this patient"))
    datatable(TCGA()[[2]],
              selection="single",rownames=F,options=list(lengthMenu = list(c(5, 10, 25, -1), c("5", "10", "25", "All")), pageLength=5))
  })
  
  ## Figure
  output$figure <- renderPlot({
    validate(need(try(!is.null(Druggable()[[4]]) &&  nrow(Druggable()[[4]])!=0), "Sorry, no actionable variants were found."))
    A                     = Druggable()[[4]]
    df                    = expand.grid(c("1.Approved","2.Clinical","3.Preclinical"), c("B.Other\nCancers","A.Same\nCancer"))
    df$value              = c(nrow(A[A$level=="B1",]),nrow(A[A$level=="B2",]),nrow(A[A$level=="B3",]),nrow(A[A$level=="A1",]),nrow(A[A$level=="A2",]),nrow(A[A$level=="A3",]))
    
    df$value[df$value==0] = NA
    df$value              = as.numeric(df$value)
    
    g = ggplot(df, aes(Var1, Var2)) + geom_point(aes(size = value), colour = c("#1B5E20","#4CAF50","#AED581","#1A237E","#1976D2","#81D4FA")) +
      theme_classic() + xlab("") + ylab("")
    g = g + labs(title="     FINDINGS BY LEVEL")+
      scale_size_continuous(range=c(5,10)) + 
      geom_text(data = df, aes(x = Var1, y = Var2, label = value), 
                size = 5, vjust = 0.5, hjust = 0.5,colour="white", fontface="bold") + 
      theme(plot.title = element_text(size=10),legend.position="none",axis.text.x = element_text(size=10,angle = 45,hjust = 1),
            axis.text.y.right = element_text(size=10,angle = 45,vjust = 1), plot.margin = margin(0.5, 0, 0, 1, "cm"),
            plot.background = element_rect(fill = "#f4f4f4",colour = "#8c8c8c",size =1))+scale_y_discrete(position = "right") 
    coord_fixed(ratio = 0.75)
    g
    
  })

  
  ## Target 
  output$others = DT::renderDataTable({
    
    validate(need(try(!is.null(Druggable()[[1]]) && nrow(Druggable()[[1]])!=0 ), "No other actionable variants found"))
    
    datatable(Druggable()[[1]],
      selection = "single",
      rownames  = F,
      escape    = F,
      options=list(scrollX = TRUE)) 
  })
  
  ## By Levels
  output$resultsTCGA = DT::renderDataTable({
   # validate(need(try(outUPLOAD() != "" ), ""))
    validate(need(try(Druggable()[[4]] != ""), "No actionable variants found"))
   
    outTCGA()
    })

  ## Report PDF
  output$report = downloadHandler(
    filename =  function() {
      paste("MTBReport", ".pdf", sep="")},
    content = function(file) {
      wd = getwd()
      f = paste(wd,"/tmp/",sep="")
      s=myFun(1)
      report <- file.path(wd, "/helpers/Report_tcga-knitr.Rnw")
      knit(report,  output =paste(f,s,".tex",sep=""))
      setwd(f) 
      texi2pdf(paste(f,s,".tex",sep=""),clean=T)
      file.copy(paste(f,s,".pdf",sep=""), file, overwrite = TRUE)
      #report <- "/home/jperera/shinyapps/app/Report_tcga.Rnw"
     # Sweave2knitr(report,output = paste(f,s,".Rnw",sep=""))
     # setwd(f)     
     # tryCatch(knit(paste(f,s,".Rnw",sep=""),  output =paste(f,s,".tex",sep="") ), error=function(e){write.csv(paste(e,paste(f,s,".tex",sep="")), file)})
     # setwd(f)
     # tryCatch(texi2pdf(paste(f,s,".tex",sep=""),clean=T), error=function(e){write.csv(paste(e,paste(f,s,".tex",sep="")), file)})
     # file.copy(paste(f,s,".pdf",sep=""), file, overwrite = TRUE)
      unlink(paste(f,s,".Rnw",sep=""))
      unlink(paste(f,s,".tex",sep=""))
      unlink(paste(f,s,".pdf",sep=""))
      setwd(wd)
    }
  )
  output$csv = downloadHandler(
    filename = paste(input$ID,'_MTBReport.csv',sep=''),
    content = function(file) {
      write.csv(Druggable()[[4]], file,row.names = F)
    }
  )



})
