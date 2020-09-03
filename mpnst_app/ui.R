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
library(shinyWidgets)
library(shinycssloaders)
library(ggplot2)
library(BioCircos)
library(RColorBrewer)
source("load_data.R")


### shiny UI page 


shinyUI(
  navbarPage(fluid =TRUE, position = c("static-top"), id = "nav_bar_page",
             
             "MPNST Surveyor", theme = shinytheme("cerulean"),
             
        
             
             tabPanel("HOME",
                      
                      ## sets background color to a gradient from white on top to light blue on the bottom
                      
                      setBackgroundColor(
                        color = c("#FFFFFF", "#EFFFFF"),
                        gradient = "linear",
                        direction = "bottom"
                      ),
                      
                    
                      #<link rel="stylesheet" type="text/css" href="footer.css" media="screen"/>
                      
                      #ensures that each h1 header is set as the same style font 
                      
                      tags$head(tags$style('h1 {color: black; font-family: Verdana}')),
                      
                      
                      ## attached HTML and CSS document for the homepage
                      
                      
                      div(class = "home_page",
                          includeHTML("home_page.html")
                      )
                
                   
             ),
             tabPanel(
               
               "SAMPLES",
               
               h2("This data table displays clinical and genomic information about the patients in the GeM cohort:" , style="font-family: Verdana; font-weight: bolder; font-size: 11pt; color: black; margin-left:5px;"),
               
               br(),
               
               h5("Please use the filter boxes to refine your search.", style="font-family: Verdana; font-size: 11pt; color: black; margin-left:5px;"),
               
               br(),
               
               div(DT::dataTableOutput("sampletable"), style = 'overflow-x: scroll; padding: 25px; font-size: 83%; width = 61%')
    
             ),
             
             tabPanel("EXPLORE", id="biocircos_panel",
                      
                    div(style = 'overflow-x: scroll;',
                      
                        fluidRow(sidebarPanel
                      (width=12, id="sidebar_circos_top",
                      
                      h1("Interactive circos plots reporting SNVs, 
											indels, total CNV, minor CN (LOH), and SV calls",
                          style="text-align: center; font-size:26px;"))),
                      
                      
                      ### HTML commands to change the background color of the side panels
                      
                      
                      tags$head(tags$style(
                            HTML('
                                 #sidebar_circos_top{
                                    background-color: #FFFFFF;
                                 }
                                 
                                 #sidebar_circos_bottom{
                                    background-color: #FFFFFF;
                                 }
                                 
                                 #sidebar_circos {
                                    background-color: #FFFFFF;
                                 }
                                 #wellPanel_circos {
                                    background-color: #FFFFFF
                                 }
                        
                                body, label, input, button, select { 
                                  font-family: "Verdana"; color:black;
                                }')
                                              )),
                      
                      fluidRow(
                        #	 shiny::column(3,
                        sidebarPanel(width=3, id="sidebar_circos",
                                     wellPanel(id="wellPanel_circos",
                                               
                                               
                                       helpText("*Please be patient, as the circos plots take time to render hypermutated samples.", style="font-family: Verdana; padding: 10px;"),        
                                               
                                  
                                       uiOutput("donor_choice"),
                                       uiOutput("chr_selection_circos"),
                                  
                                      
                                       
                                       #h3('Interactive circos plots reporting SNVs, indels, total CNV, minor CN (LOH), and SV calls')
                                       #,
                                       #fluidRow(
                                       
                                       #sidebarPanel(width=3,
                                       
                                       
                                       h5("Circos plot options*",style="font-weight: bold;"),
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
                                       
                                       
                                       br(),
                                    
                                       
                                       h5("Scroll here to view the associated data below", style="color:black; font-family: Verdana;"),
                                       
                                       br(),
                                       
                                       h5("*Note: scrolling within the BioCircos chart activates zoom functionality", style="color:black; font-family: Verdana;"),
                                       
                                       br()
                                       # br(),
                                       # 
                                       # actionLink("link_to_reload_biocircos", "Reload BioCircos")
                                    
                                       
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
                      
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
            
                                 
                      tags$h1("The data tables below provide SNV, INDEL, and CN data from the selected donor:",
                      style="text-align: left; font-size:26px;"),
                      
        
                      
                      
                      helpText("*Please note that low confidence mutations are defined as those that overlap with known polymorphisms 
                         from dbSNP (build 51).", style="font-family: Verdana; padding: 11px;"),
                      
              
                     
                      
                      tags$h4("SNVs", style = "font-size: 25px; color: black; font-family: Verdana; padding: 20px;"),
                      fluidRow( #sidebarPanel(width=12,
                        shiny::column(12,
                                      
                                      div(
                                        div(
                                          DT::dataTableOutput("table_SNVs"), style = "overflow-x: scroll; padding: 25px; font-size: 83%; width = 70%;")
                                        
                                         
                                      
                                      ),
                                      
                                      ## offers the user opportunity to clear the bamsnap image below snv table 
                                      
                                      actionButton(
                                        inputId = "clear_bamsnap_image_snv",
                                        label = "Clear Raw Sequencing Read",
                                        
                                      ),
                                      
                                      actionButton(
                                        inputId = "show_bamsnap_image_snv",
                                        label = "Show Raw Sequencing Read"
                                      ),
                                      
                                      
                                      h5("To view raw sequencing reads for each mutation, select a cell in the 'Start' column of the loaded data table. This will produce an image here:", 
                                         style="font-family: Verdana; font-size: 10pt; color: black; padding: 10px;"),
                                      
                                      
                                      div(id="bamsnap_image_snv_div",       
                                          uiOutput("bamsnap_SNV_explore")
                                      ),
                                      
                                      div(id="snv_div",
                                          
                                      )
                                
                              )

                      ),
                      
                      br(),
                      br(),
                      
                 
                                    
                                         
                      
                      tags$h4("INDELs", style = "font-size: 25px; color: black; font-family: Verdana; padding: 20px;"),
                      fluidRow( #sidebarPanel(width=12,
                        shiny::column(12,
                                      
                                      div(
                                        div(
                                          
                                          DT::dataTableOutput("table_INDELs"), style = "overflow-x: scroll; padding: 25px; font-size: 83%; width = 70%;"),
                                  
                                      ),
                                      
                                      
                                      actionButton(
                                        inputId = "clear_bamsnap_image_indel",
                                        label = "Clear Raw Sequencing Read",
                                      
                                      ),
                                      
                                      actionButton(
                                        inputId = "show_bamsnap_image_indel",
                                        label = "Show Raw Sequencing Read"
                                      ),
                                      
                                      
                                      h5("To view raw sequencing reads for each mutation, select a cell in the 'Start' column of the loaded data table. This will produce an image here:", 
                                         style="font-family: Verdana; font-size: 10pt; color: black; padding: 10px;"),
                                      
                                      
                                      div(id="bamsnap_image_indel_div",       
                                          uiOutput("bamsnap_INDEL_explore")
                                      ),
                                      
                                      div(id="indel_div",
                                          
                                      )
                                      
                                      )
                      ),
                      
                      br(),
                      br(),
                      
                      
                      tags$h4("CN Data", style = "font-size: 25px; color: black; font-family: Verdana; padding: 20px;"),
                      fluidRow( #sidebarPanel(width=12,
                        shiny::column(12,
                       
                                      
                                      div(
                                        div(
                                          DT::dataTableOutput("table_CN"),  style = "overflow-x: scroll; padding: 25px; font-size: 83%; width = 70%;")
                                        
                                      ))
                      )
             )
          
             ),
             
             
             tabPanel("MUTATIONS",
                      
                      
                      ### HTML appended to modify the background color of the sidepanel
                      
                      tags$head(tags$style(
                        HTML('
                                 #sidebar_mutation {
                                    background-color: #FFFFFF;
                                 }
                                
                        
                                body, label, input, button, select { 
                                  font-family: "Verdana"; color:black;
                                }')
                      )),
                      
                      
                  h2("The reactive data table below provides the opportunity to query SNV or INDEL data from multiple tumor samples" , style="font-family: Verdana; font-weight: bolder; font-size: 13pt; 
                     padding: 10px; color: black;"),
                  
                  h5("Select a cell in the 'Start' column of the data table to view a raw sequencing read below:", 
                     style="font-family: Verdana; font-size: 11.5pt; color: black; padding: 10px;"),
                  
                  
                  br(),
                      
                      sidebarLayout(
                        
                        
                        sidebarPanel( width=3, id="sidebar_mutation",
                                      
                                      h1(id="mutation-data-heading", "Mutation Data"),
                                      tags$style(HTML("#mutation-data-heading{color: black;font-family: Verdana; font-size:30px;}")),
                                      
                                      
                                      br(),
                                      
                                      
                                      pickerInput("var", multiple=FALSE,
                                                  label = "Please click SNV or INDEL to view the data of your choosing: ",
                                                  choices = c("SNV",
                                                              "INDEL"),
                                                
                                                  selected = "SNV"),
                                      
                                      
                                      pickerInput("chrs", multiple=FALSE,
                                                  label = "View all chromosomes, or make individual selections:",
                                                  choices = c("All",
                                                              "Select Chrs"),
                                                  
                                                  selected = "All"),
                                      
                                      
                                      conditionalPanel(
                                        
                                       condition = "input.chrs == 'Select Chrs'",
                                       
                                       uiOutput("mutation_data_chr_picker")
                                        
                                      ),
                                      
                                      
                                      helpText("*Note: When querying large amounts of data, please be patient while the table is processing,
                                               especially when viewing all donors."),
                                      
                                      pickerInput("mutation_select_choice",
                                                  label = "View all samples, or select donors: ",
                                                  
                                                  choices = c("All","Select Donors"),
                                                  
                                                  selected = "Select Donors"
                                                  ),
                                 
                                      ## drop down menu for the user to choose which samples to view 
                                      
                                      conditionalPanel(condition = "input.mutation_select_choice == 'Select Donors'",
                                      
                                      pickerInput("donor_choice_mutation", multiple=TRUE,

                                                  label = "Please choose at least one donor to view the corresponding data: ",

                                                  choices = c(sort(unique(d$donor_unique_id))),
                                                  
                                                  #options = list(`actions-box` = TRUE),

                                                  selected = "BCH_001_S4FU683F_S7EH61A2"),
                                      
    
                                     
                                     actionButton(
                                       inputId = "clear_donor_selections",
                                       label = "Reset Donors"
                                     )
                                     
                                      ),
                                     
                                     helpText("*Please note that low confidence mutations are defined as those that overlap with 
                                              known polymorphisms from dbSNP (build 51).")
                        
                        ),
                        
                        
                        
                        
                        
                        mainPanel(
                          
                          
                          conditionalPanel(
                          
                          condition = "input.chrs == 'All'",
                          
                          div(dataTableOutput("selected_mutation_all_chr"), 
                              style = 'overflow-x: scroll; font-size: 83%; width = 69%', options=list(autoWidth=TRUE)),
                          
                          br(),
                          
                          
                          actionButton(
                            inputId = "clear_bamsnap_image_all",
                            label = "Clear Raw Sequencing Read",
                           # style="color: #fff; background-color: #e95420"
                          ),
                          
                          actionButton(
                            inputId = "show_bamsnap_image_all",
                            label = "Show Raw Sequencing Read"
                          ),
                          
                          
                          h5("To view raw sequencing reads for each mutation, select a cell in the 'Start' column of the loaded data table. This will produce an image here:", 
                             style="font-family: Verdana; font-size: 10pt; color: black; padding: 10px;"),
                          
                          
                          div(id="bamsnap_image_selected_all_div",       
                              uiOutput("bamsnap_image_selected_all")
                          ),
                          
                          div(id="all_div",
                              
                              )
                          
                          ),
                          
                          conditionalPanel(
                            
                          condition = "input.chrs == 'Select Chrs'",
                          
                          div(dataTableOutput("selected_mutation_chr_sort"), 
                              style = 'overflow-x: scroll; font-size: 83%; width = 69%', options=list(autoWidth=TRUE)),
                          
                          br(),
                          
                          actionButton(
                            inputId = "clear_bamsnap_image_chr",
                            label = "Clear Raw Sequencing Read"
                          ),
                          
                          actionButton(
                            inputId = "show_bamsnap_image_chr",
                            label = "Show Raw Sequencing Read"
                          ),
                          
                          h5("To view raw sequencing reads for each mutation, select a cell in the 'Start' column of the loaded data table. This will produce an image here:", 
                             style="font-family: Verdana; font-size: 10pt; color: black; padding: 10px;"),
                          
                              
                         div(id="bamsnap_image_selected_chrs_div",       
                          uiOutput("bamsnap_image_selected_chrs")
                         ),
                         
                         div(id="chr_div",
                        
                             )
                         
                         
                          )  
                        )
                      )
                      
                      
                      
                      
                      
             ),
             tabPanel("cBioPortal",
                      
                      h2("By clicking on this image, you will arrive at a private instance of cBioPortal containing curated CN, 
                         SNV, survival data and more:" , style="font-family: Verdana; font-weight: bolder; font-size: 12pt; 
                         padding: 10px; padding-left: 20px; color: black;"),
                      
                      h3("Copy-number in cBioPortal is according to GISTIC 2.0. Values: -2 = homozygous deletion; 
                               -1 = hemizygous deletion; 0 = neutral / no change; 1 = gain; 2 = high level amplification.", 
                         style="font-family: Verdana; font-size: 10pt; padding: 10px; padding-left: 20px; color: black;"),
                      
                      br(), 
                             
                             tags$a(
                               href="http://18.206.76.181:8080/", 
                               tags$img(src="cbioportal_logo.png", 
                                       
                                        width="730",
                                        height="265")
                             )
                      ),
             
             div(class = "footer",
                 includeHTML("footer.html")
             ),
             
             tags$style(type="text/css", "body {padding-bottom: 225px;}")
             
  )

)
  
 



