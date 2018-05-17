
my_autocomplete_list <-sort(unique(as.character(synonyms$Shiny)))

sidebar <- dashboardSidebar(
  sidebarMenu(id="tab",
    tags$head(tags$style(HTML('.disclaimer_narrow {padding: 7px;background-color: #34444c ;border-radius: 25px;margin:10px;position:fixed;width:180px;bottom:0px;}
    .main-sidebar .user-panel, .sidebar-menu, .sidebar-menu>li.header {white-space: normal;overflow: hidden;}'))),
    menuItem("Home", tabName = "home",icon = icon('home')),
    menuItem("Upload patient data", tabName = "upload", icon = icon('upload')),
    menuItem("Explore TCGA dataset", tabName = "tcga", icon = icon('navicon')),
    HTML("<p class='disclaimer_narrow'>
          <strong>Disclaimer:</strong> This resource is intended for research use only and should not be used for medical or profesional advice. We make no guaran-tee of the comprehensive-ness, reliability or accuracy of the information on this website. You assume full re-sponsibility for all risks as-sociated with using this website.
          </p>" 
         )
  )
)
#shiny-tab-home {width:1200px;padding: 10px 80px;}
body <- dashboardBody(
  tags$head(tags$style(
    HTML('.h1, .h2, .h3, .h4, .h5, .h6, h1, h2, h3, h4, h5, h6 {line-height: 1.5;margin-top: 0px;}
          .h5, h5 {font-size: 14px;}
          .disclaimer {background-color: #f4f4f4;width:700px;border-radius: 25px;padding: 10px;}
          .content-wrapper {background-color: #FFF;}
          .main-header .logo {font-size:19px;}
          .main-sidebar {width: 200px;}
          .navbar { margin-left: 200px; } .content-wrapper, .main-footer, .right-side { margin-left: 200px; }
          .nav-tabs {background-color:#bfbfbf;}
          .nav-tabs-custom .nav-tabs li.active {border-top-color: #009933}
          .nav-tabs-custom .tab-content {padding:  10px 40px 10px 40px;background: #f4f4f4}
          .nav-tabs-custom {border: 1px solid #8c8c8c;}
          .btn-default:hover {background-color: #009933}
          .box {border-top: 3px solid rgba(255,255,255,.15);box-shadow: 0 1px 0px rgba(0, 0, 0, 0)}

          #x1, #x2, #mafUPLOAD, #cnvUPLOAD, #mafHDB, #cnvHDB td {line-height:70%;}
          #clinical {text-align:left;}
          #maf td {padding: 2px 6px;}
          #cnv td {padding: 2px 6px;}
	  #resultsTCGA td , #others td , #UPLOAD td , #othersUPLOAD td{padding: 1px 6px;}

          .dataTables_paginate {font-size:9px;}
          .dataTables_length {display: none;}
          .dataTables_wrapper no-footer  {vertical-align: center;}
          .popover-content {
              color: gray;
              font-size: 11px;
          }
        ') #close HTML
       )), #close tags styles
  tabItems(
    tabItem("home",
      fluidRow(
        column(width = 9,
          box(width = 12,
            HTML("
              <center>
              <img src='Logo.jpg' style='width:120px;float: left'>
              <img src='logos.png' style='width:70px;float: right'>
              <h1><strong>Interactive Molecular Tumor Board Report</strong> </h1>
              <h4><strong> - Tool to browse clinically actionable variants of cancer patients -</strong> </h1>
              <h4><i>developed by J&uacutelia Perera-Bel & Tim Bei&szligbarth</h4></i>
              <hr>"),
            box(width = 12,
                h4("Go directly to:"),
                actionButton("upload_button","Upload Patient Data"),
                actionButton("tcga_button","Explore TCGA dataset"),
                br()
                ),
            HTML("
                <h4>1. The user provides somatic variants (<a href='snv_legend.png' target='_blank'>SNVs</a> , <a href='cnv_legend.png' target='_blank'>CNVs</a>, <a href='tx_legend.png' target='_blank'>fusions</a>) of the patient</h4>
                <img src='man_icon.png' style='width:50px'>
                <img src='dna.png' style='width:80px'>
                <img src='upload.png' style='width:60px'>
                <br>
                <br>
                <h4>2. The tool searches in public databases for clinically actionable somatic variants</h4>
                <img src='database_search.png' style='width:60px'>
                <!-- <img src='wordcloud2.png' style='width:250px'> -->
                <img src='man_drug.png' style='width:50px'>
                <br>
                <br>
                <h4>3. The user can browse the patient-specific results in the web interface and also download <a href='Report.pdf' target='_blank'>pdf reports</a></h4>
                <img src='web_browser3.png' style='width:110px'> 
                <img src='download_file2.png' style='width:100px'> 
                </center>" 
                 )
          ) # close box 12
        ), # close column 9
        column(width=3,
          verticalLayout(
            box(title="iMTB Report information",width = 12,status = "primary",solidHeader = T,
              HTML("The <b>Interactive Molecular Tumor Board Report (iMTB Report)</b> is a tool designed to bring together all efforts and knowledge on predictive biomarkers. The tool is oriented at identifying actionable somatic variants of a patient's genomic profile.<br> More detailed information about the method can be found in <a target='_blank' href='https://doi.org/10.1186/s13073-018-0529-2'>our publication (Perera-Bel et al., 2018).</a>")
              ), # close box Info
            box(title="News and Updates",width = 12,status = "primary",solidHeader = T,
             HTML("
             Shiny app source code available in <a target='_blank' href='https://github.com/jperera-bel/iMTB-Report'>GitHub</a>
             <hr>
             Functionality corresponds to latest version of <a target='_blank' href='https://github.com/jperera-bel/MTB-Report'>MTB-Report (v1.1.0)</a>
             <hr>
             Current databases supported:
              <ul>
                <li><a target='_blank' href='https://www.synapse.org/#!Synapse:syn2370773'>Gene Drug Knowledge Database</a>, v20.0</li>
                <li><a target='_blank' href='https://civic.genome.wustl.edu/'>CIViC</a>, 01-May-2018</li>
                <li><a target='_blank' href='http://archive.broadinstitute.org/cancer/cga/target'>TARGET</a>, v3</li>
                <li><a target='_blank' href='https://doi.org/10.1093/jnci/djv098'>Meric-Bernstam et al., 2015</a></li>
              </ul>
             ")
            ) # close box News
          ) # close VerticalLayout
        ) # close columns 3
      ) # close Fluidrow
    ), # closes tabItem 1
    tabItem("upload",
      HTML("<h2><strong>Generate MTB-Report of user defined data</strong> </h2>"),
      box(width = 10,
        HTML("<h4>1. First of all, please upload clinical and genomic data of the patient. 
          <br>
          <font color='red'>*</font>
          You have to specify <b>cancer type</b> and upload <b>at least one type of genomic data</b> (<i>SNVs, CNVs or fusions</i>):</h4>"
         )
      ),
     box(width = 12,
      fluidRow(
        tabBox(id="uploadtab",width = 6,side="left",title=strong("Upload Data"),
          tabPanel(title="1. Clinical",
            h5("Cancer type is the only information required by the method, which is used to classify actionable variants into Levels of Evidence. Any other information is merely for completness of the final PDF report."),
            fluidRow(
              column(width=6,
                     selectInput("cancer",label=HTML("Select cancer type<font color='red'>*</font>"),choices=my_autocomplete_list,width = "250px",selected="unspecified"),
                     textInput("ID",label="Type in patient ID",width = "250px"),
                     radioButtons("gender",label="Select gender",choices=c("Female","Male","Other"),width = "250px",selected="none"),
                     selectInput("stage",label="Select cancer stage",choices=c("not specified","I","II","III","IV"),width = "130px")
                     ),
              column(width=6,
                     textInput("prev_ther",label="Type in previous therapies",width = "250px"),
                     radioButtons("tissue",label="Select tissue type",choices=c("Fresh Frozen","FFPE","Other"),width = "250px",selected="none"),
                     textInput("tumor_content",label="Type in tumor content (%)",width = "170px")
                     )
            )
          ),
         tabPanel(title=HTML("SNVs"),
            HTML("<b> Upload file with <u>S</u>ingle <u>N</u>ucloetide <u>V</u>ariants (SNVs). Check format <a href='snv_legend.png' target='_blank'>here</a> </b>"),
            br(),
            br(),
            h5("This tool does not check the quality of the variants, so please make sure they are validated and fullfilling your own requirements"),
            fluidRow(
              column(width=5,
               fileInput("inputSNV",label=HTML("<a href='snv.csv' target='_blank'>Click to get example file</a>"),width = "250px"),
               h5(em("File extensions accepted are: .xls, .xlsx, .csv, .dat, .tsv"))
              ),
              column(width=3,
                radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),','),
                checkboxInput(inputId = 'header1', label = 'Header', value = F)
                ),
              column(width=4,checkboxGroupInput('fields', 'Extra Columns',choices = 
                                  c("Variant Freq.","Quality"))
              ),
             HTML("")
          )),
         tabPanel(title="3. CNVs",
            HTML("<b> Upload file with <u>C</u>opy <u>N</u>umber <u>V</u>ariations (CNVs). Check format <a href='cnv_legend.png' target='_blank'>here</a> </b>"),
            br(),
            br(),
            h5("This tool does not check the quality of the variants, so please make sure they are validated and fullfilling your own requirements"),
            fluidRow(
              column(width=5,
                     fileInput("inputCNV",label=HTML("<a href='cnv.csv' target='_blank'>Click to get example file</a>"),width = "250px"),
                     h5(em("File extensions accepted are: .xls, .xlsx, .csv, .dat, .tsv"))
                     ),
              column(width=3, 
                radioButtons('sep2', 'Separator',c(Comma=',',Semicolon=';', Tab='\t'),','),
                checkboxInput(inputId = 'header2', label = 'Header', value = F)
              ),
              column(width=4,
                     checkboxGroupInput('fields2', 'Extra Columns',choices = 
                                          c("Segment Mean","Size (Kb)")))
          )),
         tabPanel(title="4. Fusions",
            HTML("<b> Upload file with fusions. Check format <a href='tx_legend.png' target='_blank'>here</a> </b>"),
            br(),
            br(),
            h5("This tool does not check the quality of the variants, so please make sure they are validated and fullfilling your own requirements"),
            fluidRow(
              column(width=8,
                     fileInput("inputTX",label=HTML("<a href='fusions.csv' target='_blank'>Click to get example file</a>"),width = "250px"),
                     h5(em("File extensions accepted are: .xls, .xlsx, .csv, .dat, .tsv"))

                     ),
              column(width=4,
                     radioButtons('sep3', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),','),
                     checkboxInput(inputId = 'header3', label = 'Header', value = F)
                     )
            )),
         tabPanel(title="5. Databases",
            h5(strong(c("Please select the databases used by the report"))),
            br(),
            fluidRow(
              column(width=4,
                     radioButtons('gdkd', em('Gene-Drug Knowledge Database (GDKD)'),
                                  c('v20.0'='default',none='no',other='other'),'default'),
                       conditionalPanel(
                         condition = "input.gdkd == 'other'",
                         fileInput("file_gdkd",label=h6(a(href='https://www.synapse.org/#!Synapse:syn2627707','Please upload a file from repository',target="_blank")),width = "250px")
                       )),
              column(width=4,
                     radioButtons('civic', em('Clinical Interpretations of Variants in Cancer (CIViC)'),
                                  c('01-May-2018'='default',none='no',other='other'),'default'),
                     conditionalPanel(
                                    condition = "input.civic == 'other'",
                                    fileInput("file_civic",label=h6(a(href='https://civicdb.org/releases','Please upload a file from repository',target="_blank")),width = "250px")
                                  ))
              # column(width=4,
              #        radioButtons('cgi', em('BioMarkers from Cancer Genome Interpreter (CGI)'),
              #                     c('last updated 02/08/2017'='default',none=';',other='other'),'default'),
              #        conditionalPanel(
              #                       condition = "input.cgi == 'other'",
              #                       fileInput("file_cgi",label=h6(a(href='https://www.cancergenomeinterpreter.org/biomarkers','Please upload a file from repository',target="_blank")),width = "250px")
              #                     ))
              
            )
         )
       ), # close Upload Data tabBox

       tabBox(width=6,
        side="left",
        title = strong("Browse Genomic Data"),
        tabPanel(title="SNVs ",h5("Single nucleotide variants of the patient's tumor:"),DT::dataTableOutput("mafUPLOAD"),br()),
        tabPanel(title="CNVs ",h5("Copy number variants of the patient's tumor:"),DT::dataTableOutput("cnvUPLOAD"),br()),
        tabPanel(title="Fusions ",h5("Gene fusions of the patient's tumor:"),DT::dataTableOutput("txUPLOAD"),br())
      ) # close Genomic Data tabBox
     ) # close FluidRow
    ), # close box 12

    conditionalPanel(condition=
      "typeof output.mafUPLOAD != 'undefined' || 
      typeof output.cnvUPLOAD != 'undefined' ||
      typeof output.txUPLOAD != 'undefined'",
      box(width = 10,
        HTML(" <h4>2. If the genomic data looks correct, <b>launch the MTB report</b> by clicking the button. 
            <br>The tool will search in public databases for clinically relevant information:</h4>"
            ),
        actionButton("action","Launch/Refresh MTB report",icon = icon("refresh"))
      )
    ), # close conditionalPanel to display ActionButtion
    br(),
    fluidRow(
      conditionalPanel(
        condition = "(input.action > 0 )",
        box(solidHeader = T, width = 12,collapsible = F,
             h3(strong("  Browse clinically relevant genomic data of the selected patient:")),
             HTML("<h4> You can now explore the filtered variants divided into 6 
              <a target='_blank' href='levels.png'>levels of evidence</a>, 
              which determine the actionability of the variant.
              <br>     
              You can also download a .pdf or .csv report with the results. 
              For more information on the method we forward you to our 
              <a target='_blank' href='https://doi.org/10.1186/s13073-018-0529-2'>publication.</a> </h4> "
              ),
             br(),
             box(solidHeader = T, width = 12,collapsible = F,
                 column(width=3,                                          
                        checkboxGroupInput(
                          "LevelAUPLOAD","Evidence on SAME cancer type:",
                          c("A1) FDA & Guidelines"="A1","A2) Clinical Trials"="A2","A3) Pre-clinical"="A3"),selected="none"
                          ),
                        checkboxGroupInput(
                          "LevelBUPLOAD","Evidence on OTHER cancer types:",
                          c("B1) FDA & Guidelines"="B1","B2) Clinical Trials"="B2","B3) Pre-clinical"="B3"),selected="none"
                          ),
                        plotOutput(outputId = "figureUPLOAD",height = "170px" , width = "80%"
                          ),
                        br(),
                        checkboxInput("checkothersUPLOAD","Display other genes without evidence level",value = FALSE),
                        conditionalPanel(
                          condition=" typeof output.othersUPLOAD != 'undefined' || typeof output.figureUPLOAD != 'undefined'",
                          h5(strong("Download results")),
                          downloadButton('reportUPLOAD','Download report (.pdf)'),br(),
                          downloadButton('csvUPLOAD','Download report (.csv)')
                        )
                 ),
                 column(width=9,
                  # Display Results
                    DT::dataTableOutput('UPLOAD'),
                  # Display Other Results
                    conditionalPanel(
                      condition = "input.checkothersUPLOAD",
                      DT::dataTableOutput('othersUPLOAD')
                    )
                 )
             )
        ) # close box with results
      ) # close conditionalPanel responding to ActionButtion
    ) # close FluidRow
   ), #closes tabItem 2

   tabItem("tcga",
    h2(strong("Explore The Cancer Genome Atlas Dataset")),
    br(),
    box(width = 10,
     HTML(" <h4>1. You can browse up to 34 cancer cohorts. Data is dowloaded using 
      <a href='https://github.com/mariodeng/FirebrowseR' target='_blank'>FirebrowseR</a> package 
      (R client for <a href='http://firebrowse.org/api-docs/' target='_blank'>Broad Firehose Web API</a>)</h4>")
    ),
    box(width = 12,
     fluidRow(
      tabBox(
        title = strong("Select Patient"),
        tabPanel(title="TCGA",
           h5("Please select one cohort from the list below:"),
         # selectInput("cohort","Select cohort",choices = apply(Metadata.Cohorts(format="csv"), 1 , paste , collapse = " - " )),
          selectInput("cohort","Select cohort",choices = paste(cohorts$V1,cohorts$V2, sep = " - " )),
            HTML("<h5>Please select one row from the table and explore its clinically relevant data:</h5>"),
            DT::dataTableOutput('x1')

         )
      ), # close tabBox Select Patient
      conditionalPanel(
       condition = "input.x1_rows_selected.length >0 ",
       tabBox(
         side="left",
         title = strong("Patient Data"),
          tabPanel(title="SNVs ",h5("Single nucleotide variants of the patient's tumor:"),DT::dataTableOutput('maf')),
          tabPanel(title="CNVs ",h5("Copy number variants of the patient's tumor:"),DT::dataTableOutput('cnv')),
          tabPanel(title="Clinical",h5("Main clinical features of the patient's tumor:"),uiOutput('clinical'))
       )
      ) # close ConditionalPanel display genomic
     ) # close FluidRow
    ), # close box 12
    conditionalPanel(
      condition="input.x1_rows_selected.length >0 && (typeof output.maf != 'undefined' || typeof output.cnv != 'undefined')",
      box(width = 10,
        HTML(" <h4>2. The tool will search in public databases for clinically relevant information. To see the results, 
           launch the MTB report by clicking the button. <br>If you select another patient, please remember to click again
           the button to refresh the results. </h4>"),
        actionButton("action2","Launch/Refresh MTB report",icon = icon("refresh"))
      )
    ), # close ConditionalPanel Launch MTB button
    br(),
    br(),
    br(),
    br(),
    fluidRow(
      conditionalPanel(
        condition = "input.x1_rows_selected.length >0 && input.action2 > 0 && (typeof output.maf != 'undefined' || typeof output.cnv != 'undefined')",
        box(solidHeader = T, width = 12,collapsible = F,
             h3(strong("  Browse clinically relevant genomic data of the selected patient:")),
             HTML("<h4> You can now explore the filtered variants divided into 6 
              <a target='_blank' href='levels.png'>levels of evidence</a>, 
              which determine the actionability of the variant.
              <br>     
              You can also download a .pdf or .csv report with the results. 
              For more information on the method we forward you to our 
              <a target='_blank' href='https://doi.org/10.1186/s13073-018-0529-2'>publication.</a> </h4> "
              ),
             br(),
             box(solidHeader = T, width = 12,collapsible = F,
                 column(width=3,                                          
                        checkboxGroupInput(
                          "LevelA","Evidence on SAME cancer type:",
                          c("A1) FDA & Guidelines"="A1","A2) Clinical Trials"="A2","A3) Pre-clinical"="A3"),selected="none"
                          ),
                        checkboxGroupInput(
                          "LevelB","Evidence on OTHER cancer types:",
                          c("B1) FDA & Guidelines"="B1","B2) Clinical Trials"="B2","B3) Pre-clinical"="B3"),selected="none"
                          ),
                        plotOutput(outputId = "figure",height = "170px" , width = "80%"
                          ),
                        br(),
                        checkboxInput("checkothers","Display other genes without evidence level",value = FALSE),
                        conditionalPanel(
                          condition=" typeof output.other != 'undefined' || typeof output.figure != 'undefined'",
                          h5(strong("Download results")),
                          downloadButton('report','Download report (.pdf)'),br(),
                          downloadButton('csv','Download report (.csv)')
                        )
                 ),
                 column(width=9,
                  # Display Results
                    DT::dataTableOutput('resultsTCGA'),
                  # Display Other Results
                    conditionalPanel(
                      condition = "input.checkothers",
                      DT::dataTableOutput('others')
                    )
                 )
             )
        ) # close box with results
      ) # close conditionalPanel responding to ActionButtion
    ) # close FluidRow

   ) #closes tabItem 3


  ) #closes tabItems
) #closes dashboardbody


# Put them together into a dashboardPage
dashboardPage(
  skin = "green",
  dashboardHeader(title="MTB Report",titleWidth = "200px"),
  sidebar,
  body
)
