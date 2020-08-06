#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(shinythemes)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinycssloaders)
library(ggplot2)
library(DT)
library(BioCircos)
library(shiny)
library(shinyWidgets)
library(RColorBrewer)





shinyUI(
  navbarPage(fluid =TRUE, position = c("static-top"), 
             
             "MPNST Surveyor", theme = shinytheme("cerulean"),
             
             
             tabPanel("HOME",
                      
                    
                      setBackgroundColor(
                        color = c("#FFFFFF", "#EFFFFF"),
                        gradient = "linear",
                        direction = "bottom"
                      ),
                      
                      #ensures that each header is set as the same style font 
                      
                      tags$head(tags$style('h1 {color: black; font-family: Verdana}')),
                      
                      br(),
                      
                     
                      
                      
                      fluidRow(
                        column(6,
                               
                               h1(strong("Welcome to the MPNST Surveyor"), style = "font-size:34px; text-align: center"),
                              
                               br(),
                               br(),
                               br(),
                               
                              
                               
                               p("The Genomics of MPNST (GeM) Consortium, made possible by an anonymous philanthropic 
                                    gift to the Multidisciplinary Neurofibromatosis Program at Boston Children's Hospital, 
                                    is a multi-institutional descriptive study of clinical, pathological, and molecular 
                                    features of malignant peripheral nerve sheath tumors (MPNSTs), a rare soft-tissue sarcoma 
                                    that is the most common malignancy affecting persons with Neurofibromatosis Type 1 (NF1)." 
                                 , style = "font-family: 'Verdana'; font-size:17px; text-align: center; color:black; LINE-HEIGHT:37px"),
                               
                    

                               br(),
                               br(),
                               
                               p( "Other NF1-related tumors such as plexiform neurofibromas (PN) and atypical neurofibromas 
                                    (AN) are precursors of MPNSTs, and will also be studied in some cases. Paired normal 
                                    non-tumor specimens (i.e., peripheral blood) will also be studied for comparison. 
                                    The GeM Consortium has employed a variety of molecular analysis techniques to improve 
                                    the biological understanding of NF1-related tumors."
                                  , style = "font-family: 'Verdana'; font-size:17px; text-align: center; color:black; LINE-HEIGHT:37px"),
                               
                               
                        ),
                               
                               #each of these tags$a commands loads an image that is a hyperlink to the respective websites 
                               #corresponding to each image 
                        

                        column(1),
                      
                        column(5,
                               
                               br(),
                               br(),
                               br(),
                               
                               
                               
                               tags$a(
                                 href="https://www.nfresearch-childrens.org/",
                                 tags$img(src="NF-Research-Initiative-logo-black.png",
                                          
                                          width="215",
                                          height="50")
                               ),
                               
                               
                               tags$a(
                                 href="https://www.nfresearch-childrens.org/gem-consortium",
                                 tags$img(src="GeM-Consortium-logo-black.png",
                                          
                                          width="215",
                                          height="60")
                               ),
                               
                               
                               tags$a(
                                 href="http://www.childrenshospital.org/",
                                 tags$img(src="BCHlogomotto_horizontal_300dpi.jpg",
                                          
                                          width="240",
                                          height="55")
                               ),
                               
                               
                               # tags$img(src="BCH.png",
                               #          
                               #          width="400",
                               #          height="100"),
                            
                               
                               
                               tags$img(src="DFCI.png",
                                        
                                        width="170",
                                        height="45"),
                               
                               tags$img(src="EMBL-EBI.jpg",
                                        
                                        width="125",
                                        height="70"),
                               
                               
                               tags$img(src="HMS.png",
                                        
                                        width="130",
                                        height="45"),
                               
                               tags$img(src="Huntsman.jpg",
                                        
                                        width="130",
                                        height="95"),
                               
                               tags$img(src="Lifespan_0.jpg",
                                        
                                        width="125",
                                        height="90"),
                               
                               
                               tags$img(src="MGH.jpg",
                                        
                                        width="125",
                                        height="70"),
                               
                               tags$img(src="Moffitt.png",
                                        
                                        width="110",
                                        height="95"),
                               
                               tags$img(src="Mt.Sinai.jpg",
                                        
                                        width="125",
                                        height="85"),
                               
                               tags$img(src="Nagoya.jpg",
                                        
                                        width="125",
                                        height="90"),
                            
                               tags$img(src="NYU.jpg",
                                        
                                        width="125",
                                        height="95"),
                               
                               tags$img(src="RNOH.jpg",
                                        
                                        width="125",
                                        height="110"),
                               
                               tags$img(src="WUSTL.jpg",
                                        
                                        width="125",
                                        height="110"),
                               )
                        ),  
    
                      
             ),
             tabPanel(
               
               "SAMPLES",
               
               
               div(DT::dataTableOutput("sampletable"), style = 'overflow-x: scroll')
               
             ),
             
             tabPanel("EXPLORE",
                      
                      
                      
                      # sidebarLayout(
                      #   
                      #   
                      #   sidebarPanel( width=3, 
                      #                 
                      #                 h1(id="cn-data-heading", "CN Data"),
                      #                 tags$style(HTML("#cn-data-heading{color: black;font-family: Verdana; font-size:30px;}")),
                      #                 
                      #                 
                      #                 br(),
                      #                 
                      #                 
                      #                 helpText("This choice will actually be made above along with the BioCircos selection"),
                      #                 
                      #                 selectizeInput(multiple=F,'donor', 'Please click on the donor ID to view the data of your choosing: ',
                      #                            
                      #                             sort(unique(d$donor_unique_id))
                      #                             
                      #                             )
                      #                 
                      #   ),
                      #   
                      #   
                      #   
                      #   mainPanel(
                      #     
                      #     dataTableOutput("selected_var2")
                      #     
                      #     
                      #   )
                      # )
                      
                      
                      fluidRow(sidebarPanel(width=12,h1("Interactive circos plots reporting SNVs, 
																																												indels, total CNV, minor CN (LOH), and SV calls",
                                                        style="text-align: center; font-size:31px;"))),
                      
                      
                      fluidRow(
                        #	 shiny::column(3,
                        sidebarPanel(width=3,
                                     wellPanel(
                                       #h5("The selection boxes below only affect the plots below them (not the bubble plots above)",
                                       #   style="font-style:'Open Sans';"),
                                       # type chromo
                                       #selectizeInput( 'chromo_type', 'Chromothripsis categories?', 
                                       #				choices = c("After polyploidization","Before polyploidization","Canonical without polyploidization","No chromothripsis","With other complex events"), multiple = TRUE, 
                                       #					selected=c("After polyploidization","Before polyploidization","Canonical without polyploidization","With other complex events") ),
                                       #tags$button(class='btn btn-info dropdown-toggle',
                                       #tags$button(class="btn btn-info dropdown-toggle", #btn btn-info disabled",
                                       #div(
                                       # pickerInput(inputId = "chromo_type", label = "Chromothripsis categories", 
                                       #             choices = c("After polyploidization","Before polyploidization","Canonical without polyploidization","No chromothripsis","With other complex events"),
                                       #             selected=c("After polyploidization","Before polyploidization","Canonical without polyploidization","With other complex events"),
                                       #             options = list( `actions-box` = TRUE, `size`= 5, 
                                       #                             style = "btn-info",
                                       #                             `selected-text-format` = "count > 3" ), 
                                       #             multiple = TRUE )
                                       #)
                                       #,
                                       
                                       
                                       # actionButton("selectall", "Select all types:"),
                                       
                                       #uiOutput('cancer_type2'),
                                       #textInput("text", "Enter string to search", "foo"),
                                       #actionButton("go", "Search"),
                                       uiOutput("donor_choice"),
                                       uiOutput("chr_selection_circos"),
                                       
                                       
                                       
                                       
                                       #),
                                       
                                       
                                       
                                       
                                       #tags$br(),
                                       
                                       #h3('Interactive circos plots reporting SNVs, indels, total CNV, minor CN (LOH), and SV calls')
                                       #,
                                       #fluidRow(
                                       
                                       #sidebarPanel(width=3,
                                       h5("Circos plot options",style="font-weight: bold;"),
                                       checkboxInput( inputId='show_patho_indels', label='Show pathogenic indels',value = FALSE),
                                       checkboxInput( inputId='show_nopatho_indels', label='Show non-pathogenic indels',value = FALSE),
                                       checkboxInput( inputId='show_patho', label='Show pathogenic SNVs',value = FALSE),
                                       # "nonsynonymous SNV","stopgain","stoploss","frameshift deletion","frameshift insertion"
                                       checkboxInput( inputId='show_nopatho', label='Show non-pathogenic SNVs',value = FALSE),
                                       # range for distanc
                                       sliderInput(step = 50000,"range_dist", "Range intermutation distance:", 
                                                   min = 0, max = 6000000, value = c(0,1000000)),
                                       
                                       # show genes?
                                       checkboxInput( inputId='show_genes', label='Show gene track',value = TRUE),
                                       
                                       # show chromothripsis track?
                                       #checkboxInput( inputId='show_chromo_track', label='Highlight chromothripsis regions',value = TRUE)#,
                                       
                                       #   #checkboxGroupInput
                                       #   selectizeInput( 'chr_selection_circos', 'Chromosome selection for circos plot', 
                                       # 				 choices = as.character(c(1:22,"X")), 
                                       # 				 multiple = TRUE, selected=as.character(c(1,22,"X")) )
                                       
                                       ### do this with renderUI so the chrs that appear are those with chromothripsis
                                       #uiOutput("chr_selection_circos")
                                       
                                       
                                       
                                       
                                       
                                       
                                       #pickerInput(inline = F,multiple=T,'chr_selection_circos', options = list(`actions-box` = TRUE),
                                       #            'Chromosome selection for circos plot', as.character(c(1:22,"X")),selected=as.character(c(22,"X")) )
                                       
                                     )
                        ),
                        mainPanel(width=9,
                                  div(style="display: inline-block;",
                                      dropdownButton(
                                        div(style="display: inline-block;",
                                            tags$h3("Track information (from outside to inside)",style = "display: inline-block;")),
                                        
                                        # cytobands
                                        div( tags$h4(tags$b("Track 1. GRCh38 cytobands"))),
                                        
                                        # intermutation distance
                                        div(tags$h4(tags$b("Track 2. Intermutation distance"),style = "display: inline-block;"),
                                            tags$br(),                                         
                                            tags$h4("This track displays the intermutation distance for:
																																																(i) pathogenic indels (large green dots),
																																																(ii) nonpathogenic indels (small black dots), (iii) pathogenic SNVs (large blue dots), 
																																																and (iv) nonpathogenic SNVs (small dots coloured according to the type of substitution)
																																																and SNVs. 
																																																Pathogenic mutations are defined as 
																																																nonsynonymous, stopgain, stoploss, frameshift deletion,frameshift insertion. 
																																																The y-axis, which can be modified using the slider on the left-hand side, represents the distance to the next mutation in the genome.")),
                                        
                                        # chromothripsis region
                                        tags$br(),  
                                        div(tags$h4(tags$b("Track 2. Chromothripsis regions"),style = "display: inline-block;"),
                                            tags$h4("High-confidence chromothripsis regions are indicated by a yellow bar on the same track as the SNVs and indels.
																																																			Low-confidence regions are indicated with a green bar. Please see the tables below the circos plot for further information.")),
                                        
                                        # CN
                                        tags$br(),  
                                        div(tags$h4(tags$b("Track 3. Total copy number (CN)"),style = "display: inline-block;"),
                                            tags$h4("The total CN values are displayed in black. This track has a light blue background.")),
                                        
                                        # minor CN
                                        tags$br(),  
                                        div(tags$h4(tags$b("Track 4. Minor copy number"),style = "display: inline-block;"),
                                            tags$h4("Loss-of-heterozygosity (LOH) regions 																																		  are shown in red. This track has a grey background.")),
                                        
                                        # genes 
                                        tags$br(),  
                                        div(tags$h4(tags$b("Track 5. Gene annotations"),style = "display: inline-block;"),
                                            tags$h4("Tumor suppressors and oncogenes are displayed in blue and red, respectively.")),
                                        
                                        # SVs 
                                        tags$br(),  
                                        div(tags$h4(tags$b("Track 6. Structural variations. "),style = "display: inline-block;"),
                                            tags$h4("Duplication-like SVs, deletion-like SVs, head-to-head and tail-to-tail inversions 
																																																						are depicted in blue, orange, black, and green, respectively.")),
                                        
                                        # NOTE
                                        tags$br(),  
                                        div(tags$h4(tags$b("NOTE"),style = "display: inline-block;"),
                                            tags$h4("Please note that if you display the SNVs/indels for multiple chromosomes at the same time the plot
																																																									might take a few seconds to load. Please be patient.. Only the chromosomes harboring chromothripsis are shown
																																																									for the selected tumor (other chromosomes can be selected by using the menu on the
												
                                      																																																			left-hand side).")),
                                        
                                        
                                        
                                        circle = TRUE, status = "info", icon = icon("info-circle"), width = "950px",
                                        tooltip = tooltipOptions(title = "Track details",placement="bottom"))),
                                  
                                  #includeHTML("test_igv.html"),
                                  div(style="display: inline-block; padding: 10px;",
                                      dropdownButton(
                                        div( withSpinner(DT::dataTableOutput("table_donor_info")), 
                                             style = "overflow-x: scroll;"),
                                        circle = TRUE, status = "info", icon = icon("user-md"), width = "500px",
                                        tooltip = tooltipOptions(title = "Patient information and clinical data")
                                      )),
                                  
                                  withSpinner(
                                    BioCircosOutput("biocirc", height='900px',width="1300px")
                                    #plotOutput("plot_chromo_gg",height='600px')
                                    #)
                                  ),
                                  #					  div(style="display: inline-block; padding: 0px;",
                                  #		withSpinner(DT::dataTableOutput("table_SNVs"))
                                  #		)
                        ),
                        
                        
                        
                      ),
                      
                     
                      
                      tags$h4("SNVs", style = "font-size:25px; color: black; font-family: Verdana"),
                      fluidRow( #sidebarPanel(width=12,
                        shiny::column(12,
                                      
                                      div(
                                        div(
                                          DT::dataTableOutput("table_SNVs"),  style = "overflow-x: scroll; display: inline-block; padding: 0px;")
                                        
                                      ))
                      ),
                      
                      br(),
                      br(),
                      
                      tags$h4("INDELs", style = "font-size:25px; color: black; font-family: Verdana"),
                      fluidRow( #sidebarPanel(width=12,
                        shiny::column(12,
                                      
                                      div(
                                        div(
                                          DT::dataTableOutput("table_INDELs"),  style = "overflow-x: scroll; display: inline-block; padding: 0px;")
                                        
                                      ))
                      ),
                      
                      br(),
                      br(),
                      
                      
                      tags$h4("CN Data", style = "font-size:25px; color: black; font-family: Verdana"),
                      fluidRow( #sidebarPanel(width=12,
                        shiny::column(12,
                                      
                                      div(
                                        div(
                                          DT::dataTableOutput("table_CN"),  style = "overflow-x: scroll; display: inline-block; padding: 0px;")
                                        
                                      ))
                      )
                      
          
             ),
             
             
             tabPanel("MUTATIONS",
                      
                      
                      
                      
                      
                      
                      
                      sidebarLayout(
                        
                        
                        sidebarPanel( width=3,
                                      
                                      h1(id="mutation-data-heading", "Mutation Data"),
                                      tags$style(HTML("#mutation-data-heading{color: black;font-family: Verdana; font-size:30px;}")),
                                      
                                      
                                      br(),
                                      
                                      
                                      helpText("This page allows for the exploration of mutation data through INDEL and SNV data"),
                                      
                                      selectInput("var",
                                                  label = "Please click SNV or INDEL to view the data of your choosing: ",
                                                  choices = c("SNV",
                                                              "INDEL"),
                                                
                                                  selected = "SNV"),
                                      
                                    # uiOutput("chr_selection_circos"), 
                                      selectInput("donor_choice_mutation", multiple=TRUE,

                                                  label = "Please choose a donor to view the corresponding data: ",
                                                  
                                                  choices = c(sort(unique(d$donor_unique_id))),
                                                  
                                                  selected = "BCH_001_GAPFIL7MH73G_GAPFIOP89E4M")
                                      
                                      
                        ),
                        
                        
                        
                        
                        
                        mainPanel(
                          
                          div(dataTableOutput("selected_mutation"), style = 'overflow-x: scroll')
                          
                          
                        )
                      )
                      
                      
                      
                      
                      
             ),
             tabPanel("cBioPortal",
                      
                    
                             
                             tags$a(
                               href="https://www.cbioportal.org/study/summary?id=mpnst_mskcc", 
                               tags$img(src="cBioPortalLogo.png.jfif", 
                                       
                                        width="850",
                                        height="310")
                             )
                      
                      
                    
                  
                      
                      ),
             tabPanel("FAQ")
             
  )
  
 
)

