library(ggplot2)
library(DT)
library(BioCircos)
library(shiny)
library(shinyWidgets)
library(RColorBrewer)
library(data.table)
library(shinyjs)
source("load_data.R")
source("biocircos_plots.R")
#colours_ICGC= read.table("ICGC_colours_2.txt",sep="\t",header=F,stringsAsFactors = F,comment.char="")
#d$colour_ICGC = colours_ICGC$V2[match(d$histo,colours_ICGC$V1)]



function(input, output, session) {
  
  
  
  
  ##### SAMPLES TAB ----------------------------------------------------------------------------------------------
  
  
  #import file for the sample data table with clinical and genomic data 
  
  
  sample_data_table <- read.delim('combined_sample_genomic_table.txt')
  
  
  #describes the output of the function "sampletable", which renders a data table using 
  #the file "firsttabledata"
  
  output$sampletable <- DT::renderDataTable({ 
    
          ## reorder and rename columns
          colnames(sample_data_table)[which(names(sample_data_table) == "donor_unique_id")] <- "Donor.ID"
          colnames(sample_data_table)[which(names(sample_data_table) == "NF1_somatic")] <- "NF1_Somatic"
          colnames(sample_data_table)[which(names(sample_data_table) == "NF1_somatic_minor_cn")] <- "NF1_Somatic_Minor_CN"
          colnames(sample_data_table)[which(names(sample_data_table) == "NF1_somatic_total_cn")] <- "NF1_Somatic_Total_CN"
          colnames(sample_data_table)[which(names(sample_data_table) == "NF2_somatic")] <- "NF2_Somatic"
          colnames(sample_data_table)[which(names(sample_data_table) == "NF2_somatic_minor_cn")] <- "NF2_Somatic_Minor_CN"
          colnames(sample_data_table)[which(names(sample_data_table) == "NF2_somatic_total_cn")] <- "NF2_Somatic_Total_CN"
          colnames(sample_data_table)[which(names(sample_data_table) == "TP53_somatic_minor_cn")] <- "TP53_Somatic_Minor_CN"
          colnames(sample_data_table)[which(names(sample_data_table) == "TP53_somatic_total_cn")] <- "TP53_Somatic_Total_CN"
          colnames(sample_data_table)[which(names(sample_data_table) == "SUZ12_somatic")] <- "SUZ12_Somatic"
          colnames(sample_data_table)[which(names(sample_data_table) == "SUZ12_somatic_minor_cn")] <- "SUZ12_Somatic_Minor_CN"
          colnames(sample_data_table)[which(names(sample_data_table) == "SUZ12_somatic_total_cn")] <- "SUZ12_Somatic_Total_CN"
          colnames(sample_data_table)[which(names(sample_data_table) == "EED_somatic")] <- "EED_Somatic"
          colnames(sample_data_table)[which(names(sample_data_table) == "EED_somatic_minor_cn")] <- "EED_Somatic_Minor_CN"
          colnames(sample_data_table)[which(names(sample_data_table) == "EED_somatic_total_cn")] <- "EED_Somatic_Total_CN"
          colnames(sample_data_table)[which(names(sample_data_table) == "CDKN2A_somatic")] <- "CDKN2A_Somatic"
          colnames(sample_data_table)[which(names(sample_data_table) == "CDKN2A_somatic_minor_cn")] <- "CDKN2A_Somatic_Minor_CN"
          colnames(sample_data_table)[which(names(sample_data_table) == "CDKN2A_somatic_total_cn")] <- "CDKN2A_Somatic_Total_CN"
          colnames(sample_data_table)[which(names(sample_data_table) == "NF1_germline")] <- "NF1_Germline"
          colnames(sample_data_table)[which(names(sample_data_table) == "NF2_Germline")] <- "NF2_Germline"
          colnames(sample_data_table)[which(names(sample_data_table) == "GeMID_primary_regions")] <- "Primary_Regions_GeMID"
          colnames(sample_data_table)[which(names(sample_data_table) == "normal_sample_name")] <- "Normal_Sample_Name"
          colnames(sample_data_table)[which(names(sample_data_table) == "tumour_sample_name")] <- "Tumour_Sample_Name"
          
          
          DT::datatable(sample_data_table, filter = list(
          position = 'top', clear = FALSE),  options = list(ordering=F))
    
  })
  
  

 ## EXPLORE TAB ------------------------------------------------------------------------------------------------
  
  
    ## this function is used to generate the associated clinical data for each selected donor 
    ## that is available via BioCircos plots 
    
    
    selectedData <- reactive({
          d[which(d$donor_unique_id  == input$donor & d$Chr == input$chromosome),]
    })
    
    
    ## renders data from only the selected donor 
    
    selectedData_donor_info <- reactive({
          out = unique(clinical[which(clinical$donor_unique_id  == input$donor),])
          out = t(unique(out))
          colnames(out)="Value"
          return(out)
    })
    
    
    # Table with donor information (e.g. age, etc..) 
    # can be accessed in the biocircos plots if the user clicks on the 'clinical data' icon to the top right of the plot 
    
    
    output$table_donor_info <- DT::renderDataTable({
          DT::datatable(selectedData_donor_info(),style = 'bootstrap',options = list(sDom  = '<"top">lrt<"bottom">ip') )
    })
    
    
    
    # output$find_case <- DT::renderDataTable({
    #   DT::datatable(selectedData_find_case(), style = 'bootstrap',filter = list(position = "top") )
    # })
    # 
    
    
    
    
    
    
  
    ## generates a list of all the donors (samples) by drawing all unique strings from the 'donor_unique_id' 
    ## column of the combined clinical and genomic data table 
    
    get_donor_list = function(d,input){
        
              d_unique = sort(unique(d$donor_unique_id))
              return(d_unique)
              
    }
    
    
    ## dropdown menu option so the user can select which sample to view 
    
    output$donor_choice <- renderUI({
             selectizeInput(multiple=F,'donor', 'Donor ID', 
                           #get_with2(d,input)[[1]]
                           #get_with(d,input)
                           
                           get_donor_list(d,input)
                           
                        )
    })
    
    
    ## menu for the user to select which chrs to be rendered in circos plot
    
     output$chr_selection_circos  <- renderUI({
             pickerInput( 'chr_selection_circos', 'Chromosome selection for circos plot',
                          choices = as.character(paste0("chr",c(1:22,"X"))),
                          #selected=get_with2(d,input)[[2]],
                          selected=as.character(paste0("chr",c(1:22,"X"))),
                          options = list( `actions-box` = TRUE, size = 5,
                                          style = "btn-info",
                                          `selected-text-format` = "count > 3" ), multiple = TRUE )
         
     })
    
     
     ### event order for circos plots functions 
     
    observeEvent(c(
             input$range_dist,
             input$show_patho_indels, 
             input$show_nopatho_indels,
             input$show_patho,
             input$show_nopatho,
             #input$show_chromo_track,
             input$show_genes,
             input$donor,
             # input$cancer_type2,input$chromo_type #, 
              input$chr_selection_circos
      	   #input$donor,
             # input$chromo_type ### XXXX input$donor added input$donor input$cancer_type2,
          ), { output$biocirc <- renderBioCircos({
              plot_biocirc(input)#$chrs_selection_circos) XXXXXX
             # plot_biocirc(input2,input)#$chrs_selection_circos)
            })
    
        } 
    
    )


	# Tables for SNVs, INDELs, CN
    observeEvent(c(
           input$range_dist,
           input$show_patho_indels, 
           input$show_nopatho_indels,
           input$show_patho,
           input$show_nopatho,
           #input$show_chromo_track,
           input$show_genes,
           input$donor,
    	   input$chr_selection_circos
	   ), {
	     
	     
	 output$table_SNVs <- DT::renderDataTable({
      
      
      ## reorder and rename columns 
      
      ##change df from reactive because shiny doesn't allow changes to col names for reactive
      m_table_1 <- loadsnvtable()
      
      ## reorder and rename columns
      colnames(m_table_1)[which(names(m_table_1) == "sample")] <- "Sample"
      colnames(m_table_1)[which(names(m_table_1) == "avsnp147")] <- "Avsnp147"
      colnames(m_table_1)[which(names(m_table_1) == "Sift_pred")] <- "Sift_Pred"
      colnames(m_table_1)[which(names(m_table_1) == "Polyphen2_HDIV_pred")] <- "Polyphen2_HDIV_Pred"
      colnames(m_table_1)[which(names(m_table_1) == "LRT_pred")] <- "LRT_Pred"
      colnames(m_table_1)[which(names(m_table_1) == "MutationTaster_pred")] <- "Mutation_Taster_Pred"
      colnames(m_table_1)[which(names(m_table_1) == "MutationAssessor_pred")] <- "Mutation_Assessor_Pred"
      colnames(m_table_1)[which(names(m_table_1) == "FATHMM_pred")] <- "FATHMM_Pred"
      colnames(m_table_1)[which(names(m_table_1) == "PROVEAN_pred")] <- "PROVEAN_Pred"
      colnames(m_table_1)[which(names(m_table_1) == "CADD_raw")] <- "CADD_Raw"
      colnames(m_table_1)[which(names(m_table_1) == "DANN_score")] <- "DANN_Score"
      colnames(m_table_1)[which(names(m_table_1) == "fathmm-MKL_coding_pred")] <- "Fathmm-MKL_Coding_Pred"
      colnames(m_table_1)[which(names(m_table_1) == "MetaSVM_pred")] <- "Meta_SVM_Pred"
      colnames(m_table_1)[which(names(m_table_1) == "Chr_CN_segment")] <- "Chr_CN_Segment"
      colnames(m_table_1)[which(names(m_table_1) == "Start_CN_segment")] <- "Start_CN_Segment"
      colnames(m_table_1)[which(names(m_table_1) == "End_CN_segment")] <- "End_CN_Segment"
      colnames(m_table_1)[which(names(m_table_1) == "Total_CN_normal")] <- "Total_CN_Normal"
      colnames(m_table_1)[which(names(m_table_1) == "Minor_CN_normal")] <- "Minor_CN_Normal"
      colnames(m_table_1)[which(names(m_table_1) == "Total_CN_tumour")] <- "Total_CN_Tumour"
      colnames(m_table_1)[which(names(m_table_1) == "Minor_CN_tumour")] <- "Minor_CN_Tumour"
      colnames(m_table_1)[which(names(m_table_1) == "gender")] <- "Gender"
      colnames(m_table_1)[which(names(m_table_1) == "callers")] <- "Callers"
      colnames(m_table_1)[which(names(m_table_1) == "NumCallers")] <- "Num_Callers"
      colnames(m_table_1)[which(names(m_table_1) == "dista")] <- "Distance"
      colnames(m_table_1)[which(names(m_table_1) == "label")] <- "Label"
      colnames(m_table_1)[which(names(m_table_1) == "tumour_VAF")] <- "Tumour_VAF"
      colnames(m_table_1)[which(names(m_table_1) == "tumour_DP")] <- "Tumour_DP"
      colnames(m_table_1)[which(names(m_table_1) == "normal_VAF")] <- "Normal_VAF"
      colnames(m_table_1)[which(names(m_table_1) == "normal_DP")] <- "Normal_DP"
      colnames(m_table_1)[which(names(m_table_1) == "confidence")] <- "Confidence"
      
      
      
      
      # move Sample column to the front 
      setcolorder(m_table_1, c("Sample",colnames(m_table_1)[!(colnames(m_table_1) %in% c("Sample"))]))
      
      
          DT::datatable(m_table_1, filter = list(position = "top", clear = FALSE), options=list(pageLength = 10, lengthMenu = c( 10, 25, 100), scrollX = T ),
                        selection=list(mode="single", target="cell"))
    })
	 
	
	 
	 output$table_INDELs <- DT::renderDataTable({
	   
	   ## reorder and rename columns 
	   
	   ##change df from reactive because shiny doesn't allow changes to col names for reactive
	   m_table_1 <- loadindeltable()
	   
	   ## reorder and rename columns
	   colnames(m_table_1)[which(names(m_table_1) == "sample")] <- "Sample"
	   colnames(m_table_1)[which(names(m_table_1) == "avsnp147")] <- "Avsnp147"
	   colnames(m_table_1)[which(names(m_table_1) == "Sift_pred")] <- "Sift_Pred"
	   colnames(m_table_1)[which(names(m_table_1) == "Polyphen2_HDIV_pred")] <- "Polyphen2_HDIV_Pred"
	   colnames(m_table_1)[which(names(m_table_1) == "LRT_pred")] <- "LRT_Pred"
	   colnames(m_table_1)[which(names(m_table_1) == "MutationTaster_pred")] <- "Mutation_Taster_Pred"
	   colnames(m_table_1)[which(names(m_table_1) == "MutationAssessor_pred")] <- "Mutation_Assessor_Pred"
	   colnames(m_table_1)[which(names(m_table_1) == "FATHMM_pred")] <- "FATHMM_Pred"
	   colnames(m_table_1)[which(names(m_table_1) == "PROVEAN_pred")] <- "PROVEAN_Pred"
	   colnames(m_table_1)[which(names(m_table_1) == "CADD_raw")] <- "CADD_Raw"
	   colnames(m_table_1)[which(names(m_table_1) == "DANN_score")] <- "DANN_Score"
	   colnames(m_table_1)[which(names(m_table_1) == "fathmm-MKL_coding_pred")] <- "Fathmm-MKL_Coding_Pred"
	   colnames(m_table_1)[which(names(m_table_1) == "MetaSVM_pred")] <- "Meta_SVM_Pred"
	   colnames(m_table_1)[which(names(m_table_1) == "Chr_CN_segment")] <- "Chr_CN_Segment"
	   colnames(m_table_1)[which(names(m_table_1) == "Start_CN_segment")] <- "Start_CN_Segment"
	   colnames(m_table_1)[which(names(m_table_1) == "End_CN_segment")] <- "End_CN_Segment"
	   colnames(m_table_1)[which(names(m_table_1) == "Total_CN_normal")] <- "Total_CN_Normal"
	   colnames(m_table_1)[which(names(m_table_1) == "Minor_CN_normal")] <- "Minor_CN_Normal"
	   colnames(m_table_1)[which(names(m_table_1) == "Total_CN_tumour")] <- "Total_CN_Tumour"
	   colnames(m_table_1)[which(names(m_table_1) == "Minor_CN_tumour")] <- "Minor_CN_Tumour"
	   colnames(m_table_1)[which(names(m_table_1) == "gender")] <- "Gender"
	   colnames(m_table_1)[which(names(m_table_1) == "callers")] <- "Callers"
	   colnames(m_table_1)[which(names(m_table_1) == "NumCallers")] <- "Num_Callers"
	   colnames(m_table_1)[which(names(m_table_1) == "dista")] <- "Distance"
	   colnames(m_table_1)[which(names(m_table_1) == "label")] <- "Label"
	   colnames(m_table_1)[which(names(m_table_1) == "tumour_VAF")] <- "Tumour_VAF"
	   colnames(m_table_1)[which(names(m_table_1) == "tumour_DP")] <- "Tumour_DP"
	   colnames(m_table_1)[which(names(m_table_1) == "normal_VAF")] <- "Normal_VAF"
	   colnames(m_table_1)[which(names(m_table_1) == "normal_DP")] <- "Normal_DP"
	   colnames(m_table_1)[which(names(m_table_1) == "confidence")] <- "Confidence"
	   
	   
	   
	   
	   # move Sample column to the front 
	   setcolorder(m_table_1, c("Sample",colnames(m_table_1)[!(colnames(m_table_1) %in% c("Sample"))]))
	   
	   
	   DT::datatable(m_table_1, filter = list(position = "top", clear = FALSE), options=list(pageLength = 10,lengthMenu = c( 10, 25, 100), scrollX = T ),
	                 selection=list(mode="single", target="cell"))
	 })

	 
	 
	 ## outputs a data table with the CN data corresponding to the donor selected in circos plot 
	 
	 output$table_CN <- DT::renderDataTable({
	   DT::datatable(loadcntable(), filter = list(position = "top", clear = FALSE), options=list(pageLength = 10,lengthMenu = c( 10, 25, 100), scrollX = T ),
	                 colnames = c('Chr', 'Start', 'End', 'Orientation', 'Gene','Total CN','Minor CN'))
	 })
    

  
	}
	)
    
    ## reactive function to load snvs corresponding with the donor selected for the circos plot (loads below circos plot)
    
    loadsnvtable <- reactive({
      donor = as.vector( mapping_IDs$sample[which(as.vector(mapping_IDs$fixed_sample_IDs) == input$donor)] )
      patho_file = paste0("./snvs_patho/",donor,".rds")
      patho = tryCatch( readRDS(patho_file) ,error=function(e) NULL)
      nopatho_file = paste0("./snvs_nopatho/",donor,".rds")
      nopatho = tryCatch( readRDS(nopatho_file) ,error=function(e) NULL)
      nopatho = rbind(patho, nopatho)
      rownames(nopatho) = 1:nrow(nopatho)
      if (!is.null(nopatho)){
        idxnow = which(nopatho$Chr %in% input$chr_selection_circos)
        nopatho = nopatho[idxnow,]
      }
      rownames(nopatho) = 1:nrow(nopatho)
      return(nopatho)
      
    })
    
    ## loads bamsnap raw sequencing reads for a selected mutation below the snv data table 
    
    output$bamsnap_SNV_explore <- renderUI({
      
      ### get the right donor name using mapping rds file
      
      donor = as.vector( mapping_IDs$sample[which(as.vector(mapping_IDs$fixed_sample_IDs) == input$donor)] )
      
      ## find the value of the selected cell
      
      chr_location_value_snv = input$table_SNVs_cell_clicked$value
      
      ## get the chromosome number from the cell to the left of selected cell
      
      chr_number_snv = loadsnvtable()[input$table_SNVs_cell_clicked$row,which(colnames(loadsnvtable())=="Chr")]
      
      ## concatenate the call to file to pull the sequencing image
      
      sequencing_image_snv <- paste0("/bamsnap_SNV/",donor,"/",chr_number_snv,"_",chr_location_value_snv,".png")
      
      print(sequencing_image_snv)
      
      
      tags$img(src= sequencing_image_snv, alt = "")
      
    })
    
    
    ## renders bamsnap image for snv datatable when input show_bamsnap_image_snv is enacted 
    
    observeEvent(input$show_bamsnap_image_snv,{
      insertUI(
        selector = "div#snv_div",
        where = c("afterBegin"),
        uiOutput("bamsnap_SNV_explore")
      )
      
    })
    
    ## clear image for snv data table below biocircos
    
    observeEvent(input$clear_bamsnap_image_snv, {
      
      removeUI(
        selector = "div#bamsnap_SNV_explore"
        
      )
      
    })
    

    ## reactive function to load indels corresponding with the donor selected for the circos plot (loads below snv table)

    loadindeltable <- reactive({
                donor = as.vector( mapping_IDs$sample[which(as.vector(mapping_IDs$fixed_sample_IDs) == input$donor)] )
                indels_patho_file = paste0("./indels_patho/",donor,".rds")
                patho = tryCatch( readRDS(indels_patho_file) ,error=function(e) NULL)
                indels_nopatho_file = paste0("./indels_nopatho/",donor,".rds")
                nopatho = tryCatch( readRDS(indels_nopatho_file) ,error=function(e) NULL)
          	   nopatho = rbind(patho,nopatho)
          	   rownames(nopatho) = 1:nrow(nopatho)
                if (!is.null(nopatho)){
                  idxnow = which(nopatho$Chr %in% input$chr_selection_circos)
                  nopatho = nopatho[idxnow,]
                }
                rownames(nopatho) = 1:nrow(nopatho)
                return(nopatho)
      
    })
    
    ## loads bamsnap raw sequencing read below the INDEL data table in "EXPLORE" tab
    
    output$bamsnap_INDEL_explore <- renderUI({
      
              ### get the right donor name using mapping rds file 
              
              donor = as.vector( mapping_IDs$sample[which(as.vector(mapping_IDs$fixed_sample_IDs) == input$donor)] )
              
              ## assign variable to selected cell
              
              chr_location_cell_indel = input$table_INDELs_cell_clicked
              
              ## find the value of the selected cell
              
              chr_location_value_indel = chr_location_cell_indel$value
              
              ## get the chromosome number from the cell to the left of selected cell
              
              chr_number_indel = loadindeltable()[input$table_INDELs_cell_clicked$row,which(colnames(loadindeltable())=="Chr")]
              
              ## concatenate the call to file to pull the sequencing image
              
              sequencing_image_indel <- paste0("/bamsnap_INDEL/",donor,"/",chr_number_indel,"_",chr_location_value_indel,".png")
              
              print(sequencing_image_indel)
              
              tags$img(src= sequencing_image_indel, alt = "")
      
    })
    
    
    ## renders bamsnap image for indel datatable when input show_bamsnap_image_indel is enacted 
    
    observeEvent(input$show_bamsnap_image_indel,{
      insertUI(
        selector = "div#indel_div",
        where = c("afterBegin"),
        uiOutput("bamsnap_INDEL_explore")
      )
      
    })
    
    ## clear image for indel data table below biocircos
    
    observeEvent(input$clear_bamsnap_image_indel, {
      
      removeUI(
        selector = "div#bamsnap_INDEL_explore"
      
      )
      
    })
    
    
    
    ## loads CN file with data corresponding to the donor selected in the circos plot above (loads below indel data)
    
    loadcntable <- reactive({
              
              donor = as.vector( mapping_IDs$sample[which(as.vector(mapping_IDs$fixed_sample_IDs) == input$donor)] )
              cn_file = paste0("./CN_for_all_genes/CN_for_all_genes_",donor,".bed")
              
              cnv = read.table(cn_file,header = FALSE, sep="\t",stringsAsFactors=FALSE, quote="")
          	  colnames(cnv)[6:7] = c("Total CN", "Minor CN")
          	  rownames(cnv) = 1:nrow(cnv)
          	  return(cnv)
      
              # nopatho = tryCatch( readRDS(nopatho_file) ,error=function(e) NULL)
              # if (!is.null(nopatho)){
              #   idxnow = which(nopatho$Chr %in% input$chr_selection_circos)
              #   nopatho = nopatho[idxnow,]
              # }
              # rownames(nopatho) = 1:nrow(nopatho)
              # return(nopatho)
      
    })



    ## MUTATION TAB ------------------------------------------------------------------------
 
    

    
    #reactive function that takes in the users input of which samples they want to view and appends those files 
    # together so that all the files corresponding to the donors they choose are rendered 
    
    mutation_data_table <- reactive({   #XXXX ew need to load multiple donors
                #print(input$donor_choice_mutation) 
                #input_donor_choice_mutation = as.vector( mapping_IDs$sample[which(as.vector(mapping_IDs$fixed_sample_IDs) == input$donor_choice_mutation)] )
             
          
              if(input$type=="Select Donors"){
                
              dons = c(); mm =c()
          
          	   
          	  for (j in 1:length(input$donor_choice_mutation)){
          		  input_donor_choice_mutation = as.vector(mapping_IDs$sample[which(as.vector(mapping_IDs$fixed_sample_IDs) == input$donor_choice_mutation[j])])
          		  dons = c(dons,input_donor_choice_mutation)
                input_file_mutation <- paste0("./consensus_",input$var,"_ind/consensus_",input$var,"_",input_donor_choice_mutation,".rds")
                #print(input$var)
                #print(input_file_mutation)
                #print(input_donor_choice_mutation)
                t = readRDS(input_file_mutation)
                #print(t)
          	  mm = rbind(mm, readRDS(input_file_mutation))
          	  print(head(mm))
          	  }
              
            
          	  rownames(mm) = 1:nrow(mm)
          	  return(mm)
          	  
          	  
          	  
          	  ##commentted out the below chunk of code for the time being until it works with displaying the data
          	  
               
          #       #input_file_mutation <- paste0("./consensus_",input$var,"_ind/consensus_SNV_all_samples.rds")
          # 	  #print(input_file_mutation)
          # 	  #print(mm)
          #       #mm = readRDS(input_file_mutation)
          # 	  #mm = mm[which(mm$sample %in% dons),] #input$donor_choice_mutation),]
          # 	  rownames(mm) = 1:nrow(mm)
          # 	  if(input$var == "SNV"){
          # 	  mm$normal_DP = round(mm$normal_DP)
          # 	  mm$tumour_DP = round(mm$tumour_DP)
          # 	  mm$normal_VAF = round(mm$normal_VAF,digits=2)
          # 	  mm$tumour_VAF = round(mm$tumour_VAF,digits=2)
          # 	  }else{ #indels
          # 	  mm$tumour_DP = round(mm$tumour_DP)
          #     mm$tumour_VAF = round(mm$tumour_VAF,digits=2)
          # 	  }
          # 	  return(mm)
          	  
             }    else {
          
                     input_file_mutation_all <- paste0("./consensus_",input$var,"_ind/consensus_",input$var,"_all_samples.rds")
                     
                     print(input_file_mutation_all)
          
                     nn = readRDS(input_file_mutation_all)
                     
                     rownames(nn) = 1:nrow(nn)
                    
                     return(nn)
          
                   }
	      })
    
    
    ## provides a picker input with the list of chromosomes to choose from 
    
    output$mutation_data_chr_picker <- renderUI({
              pickerInput( 'chr_selection_mutation_table', 'Please choose at least one chromosome to view the corresponding data:', 
                  choices = as.character(paste0("chr",c(1:22,"X"))),
                  #selected=get_with2(d,input)[[2]],
                  selected=as.character(paste0("chr",c(1:22,"X"))),
                  options = list( `actions-box` = TRUE, size = 5, 
                                  `selected-text-format` = "count > 3" ), multiple = TRUE )
            })
    
    ## when action button to clear donor selections is pressed, updates the picker input to its originial display 
    
    observeEvent(input$clear_donor_selections, {
      updatePickerInput(
        session, 
        "donor_choice_mutation", 
        selected = "BCH_001_S4FU683F_S7EH61A2"
      )
    })
    
         
   #output of data_table for mutation data (snv/indels) 
   #data table where all chromosomes are shown 
  
          
    output$selected_mutation_all_chr <- DT::renderDataTable({
      
      ##change df from reactive because shiny doesn't allow changes to col names for reactive
      m_table_1 <- mutation_data_table()
      
      ## reorder and rename columns
      colnames(m_table_1)[which(names(m_table_1) == "sample")] <- "Sample"
      colnames(m_table_1)[which(names(m_table_1) == "avsnp147")] <- "Avsnp147"
      colnames(m_table_1)[which(names(m_table_1) == "Sift_pred")] <- "Sift_Pred"
      colnames(m_table_1)[which(names(m_table_1) == "Polyphen2_HDIV_pred")] <- "Polyphen2_HDIV_Pred"
      colnames(m_table_1)[which(names(m_table_1) == "LRT_pred")] <- "LRT_Pred"
      colnames(m_table_1)[which(names(m_table_1) == "MutationTaster_pred")] <- "Mutation_Taster_Pred"
      colnames(m_table_1)[which(names(m_table_1) == "MutationAssessor_pred")] <- "Mutation_Assessor_Pred"
      colnames(m_table_1)[which(names(m_table_1) == "FATHMM_pred")] <- "FATHMM_Pred"
      colnames(m_table_1)[which(names(m_table_1) == "PROVEAN_pred")] <- "PROVEAN_Pred"
      colnames(m_table_1)[which(names(m_table_1) == "CADD_raw")] <- "CADD_Raw"
      colnames(m_table_1)[which(names(m_table_1) == "DANN_score")] <- "DANN_Score"
      colnames(m_table_1)[which(names(m_table_1) == "fathmm-MKL_coding_pred")] <- "Fathmm-MKL_Coding_Pred"
      colnames(m_table_1)[which(names(m_table_1) == "MetaSVM_pred")] <- "Meta_SVM_Pred"
      colnames(m_table_1)[which(names(m_table_1) == "Chr_CN_segment")] <- "Chr_CN_Segment"
      colnames(m_table_1)[which(names(m_table_1) == "Start_CN_segment")] <- "Start_CN_Segment"
      colnames(m_table_1)[which(names(m_table_1) == "End_CN_segment")] <- "End_CN_Segment"
      colnames(m_table_1)[which(names(m_table_1) == "Total_CN_normal")] <- "Total_CN_Normal"
      colnames(m_table_1)[which(names(m_table_1) == "Minor_CN_normal")] <- "Minor_CN_Normal"
      colnames(m_table_1)[which(names(m_table_1) == "Total_CN_tumour")] <- "Total_CN_Tumour"
      colnames(m_table_1)[which(names(m_table_1) == "Minor_CN_tumour")] <- "Minor_CN_Tumour"
      colnames(m_table_1)[which(names(m_table_1) == "gender")] <- "Gender"
      colnames(m_table_1)[which(names(m_table_1) == "callers")] <- "Callers"
      colnames(m_table_1)[which(names(m_table_1) == "NumCallers")] <- "Num_Callers"
      colnames(m_table_1)[which(names(m_table_1) == "dista")] <- "Distance"
      colnames(m_table_1)[which(names(m_table_1) == "label")] <- "Label"
      colnames(m_table_1)[which(names(m_table_1) == "tumour_VAF")] <- "Tumour_VAF"
      colnames(m_table_1)[which(names(m_table_1) == "tumour_DP")] <- "Tumour_DP"
      colnames(m_table_1)[which(names(m_table_1) == "normal_VAF")] <- "Normal_VAF"
      colnames(m_table_1)[which(names(m_table_1) == "normal_DP")] <- "Normal_DP"
      colnames(m_table_1)[which(names(m_table_1) == "confidence")] <- "Confidence"


      # move Sample column to the front 
      setcolorder(m_table_1, c("Sample",colnames(m_table_1)[!(colnames(m_table_1) %in% c("Sample"))]))
      
      
              
                      DT::datatable(m_table_1, filter = list(   
                        position = 'top', clear = FALSE), 
                        
                        selection=list(mode="single", target="cell")
                      )
      
    })
    
    
    ## output of data table for mutation data (snv/indels) where 
    ## the user is sorting based on chromosome 
    
    output$selected_mutation_chr_sort <- DT::renderDataTable({
      
      
            ## reorder and rename columns 
      
            ##change df from reactive because shiny doesn't allow changes to col names for reactive
            m_table_1 <- mutation_data_table()
            
            ## reorder and rename columns
            colnames(m_table_1)[which(names(m_table_1) == "sample")] <- "Sample"
            colnames(m_table_1)[which(names(m_table_1) == "avsnp147")] <- "Avsnp147"
            colnames(m_table_1)[which(names(m_table_1) == "Sift_pred")] <- "Sift_Pred"
            colnames(m_table_1)[which(names(m_table_1) == "Polyphen2_HDIV_pred")] <- "Polyphen2_HDIV_Pred"
            colnames(m_table_1)[which(names(m_table_1) == "LRT_pred")] <- "LRT_Pred"
            colnames(m_table_1)[which(names(m_table_1) == "MutationTaster_pred")] <- "Mutation_Taster_Pred"
            colnames(m_table_1)[which(names(m_table_1) == "MutationAssessor_pred")] <- "Mutation_Assessor_Pred"
            colnames(m_table_1)[which(names(m_table_1) == "FATHMM_pred")] <- "FATHMM_Pred"
            colnames(m_table_1)[which(names(m_table_1) == "PROVEAN_pred")] <- "PROVEAN_Pred"
            colnames(m_table_1)[which(names(m_table_1) == "CADD_raw")] <- "CADD_Raw"
            colnames(m_table_1)[which(names(m_table_1) == "DANN_score")] <- "DANN_Score"
            colnames(m_table_1)[which(names(m_table_1) == "fathmm-MKL_coding_pred")] <- "Fathmm-MKL_Coding_Pred"
            colnames(m_table_1)[which(names(m_table_1) == "MetaSVM_pred")] <- "Meta_SVM_Pred"
            colnames(m_table_1)[which(names(m_table_1) == "Chr_CN_segment")] <- "Chr_CN_Segment"
            colnames(m_table_1)[which(names(m_table_1) == "Start_CN_segment")] <- "Start_CN_Segment"
            colnames(m_table_1)[which(names(m_table_1) == "End_CN_segment")] <- "End_CN_Segment"
            colnames(m_table_1)[which(names(m_table_1) == "Total_CN_normal")] <- "Total_CN_Normal"
            colnames(m_table_1)[which(names(m_table_1) == "Minor_CN_normal")] <- "Minor_CN_Normal"
            colnames(m_table_1)[which(names(m_table_1) == "Total_CN_tumour")] <- "Total_CN_Tumour"
            colnames(m_table_1)[which(names(m_table_1) == "Minor_CN_tumour")] <- "Minor_CN_Tumour"
            colnames(m_table_1)[which(names(m_table_1) == "gender")] <- "Gender"
            colnames(m_table_1)[which(names(m_table_1) == "callers")] <- "Callers"
            colnames(m_table_1)[which(names(m_table_1) == "NumCallers")] <- "Num_Callers"
            colnames(m_table_1)[which(names(m_table_1) == "dista")] <- "Distance"
            colnames(m_table_1)[which(names(m_table_1) == "label")] <- "Label"
            colnames(m_table_1)[which(names(m_table_1) == "tumour_VAF")] <- "Tumour_VAF"
            colnames(m_table_1)[which(names(m_table_1) == "tumour_DP")] <- "Tumour_DP"
            colnames(m_table_1)[which(names(m_table_1) == "normal_VAF")] <- "Normal_VAF"
            colnames(m_table_1)[which(names(m_table_1) == "normal_DP")] <- "Normal_DP"
            colnames(m_table_1)[which(names(m_table_1) == "confidence")] <- "Confidence"
      
      
      
      
            # move Sample column to the front 
            setcolorder(m_table_1, c("Sample",colnames(m_table_1)[!(colnames(m_table_1) %in% c("Sample"))]))
    
            
                      ## get all the chromosomes selected by the user 
                      
                      chr_choices <- input$chr_selection_mutation_table
                      
                      
                      ##change to data.table because need to show only rows with specific chromosome values 
                      
                      x <- data.table(m_table_1)
                      
                      ##set key to the column where chromosome values are 
                      
                      setkey(x,Chr) 
                      
                      
                      ## sort by which rows have the chr values selected 
                      
                      x2 <- x[c(chr_choices)]
                      
                      ## change data.table back to df to be rendered
                      
                      class(as.data.frame(x2))
                      
                      ## renders the modified DT
                      
                      DT::datatable(x2, filter = list(   
                        position = 'top', clear = FALSE), 
                        
                        selection=list(mode="single", target="cell")
                      )
      
    })
    
    
    
    ## renders a png of sequencing data corresponding with the specific mutation
    ## the user will select a chromosome start position on the data table which will generate the image below 
    ## specifically for the datatable where all chrs are being selected
    

    output$bamsnap_image_selected_all <- renderUI({
      
      
              ## assign variable to selected cell 
              
              chr_location_cell = input$selected_mutation_all_chr_cell_clicked
              
              
              ## find the value of the selected cell 
              
              chr_location_value = chr_location_cell$value
              
              ## get the chromosome number from the cell to the left of selected cell 
              
              chr_number = mutation_data_table()[input$selected_mutation_all_chr_cell_clicked$row,which(colnames(mutation_data_table())=="Chr")]
              
              
              ## images use old sample id naming 
              
              old_donor_name = as.vector(mapping_IDs$sample[which(as.vector(mapping_IDs$fixed_sample_IDs) == input$donor_choice_mutation)])
              
              ##which(colnames(mutation_data_table())=="sample")
              ##match("sample",names(mutation_data_table()))
              ## ^^ options for retrieving col number given column name sample 
              
              donor_id_from_column = mutation_data_table()[input$selected_mutation_all_chr_cell_clicked$row,which(colnames(mutation_data_table())=="sample")]
              
              
              bamsnap_folder = paste0("bamsnap_",input$var)
              
              
              
              ## concatenate the call to file to pull the sequencing image 
              
              sequencing_image <- paste0("/",bamsnap_folder,"/",donor_id_from_column,"/",chr_number,"_",chr_location_value,".png")
              
              
              ## renders the sequencing image, and makes sure there is not a 'broken image' icon before the user selects an image
              
              tags$img(src = sequencing_image, alt = "")
      
      
    })
    
    
    ## renders bamsnap image for datatable with all chrs when input show_bamsnap_image_all is enacted 
    
    observeEvent(input$show_bamsnap_image_all,{
      insertUI(
        selector = "div#all_div",
        where = c("afterBegin"),
        uiOutput("bamsnap_image_selected_all")
      )
      
    })
    
    ## clear image for mutation data table with all chromosomes 
    
    observeEvent(input$clear_bamsnap_image_all, {
      
      removeUI(
        selector = "div#bamsnap_image_selected_all"
      )
      
    })
    
    
    
    ## renders a png of sequencing data corresponding with the specific mutation
    ## the user will select a chromosome start position on the data table which will generate the image below 
    ## specifically for the datatable where certain chrs are being selected
    ## added "_1 to all variable names to avoid conflict with other bamsnap image loading function
    
    
    
    output$bamsnap_image_selected_chrs <- renderUI({
      
      
              ## assign variable to selected cell
              
              chr_location_cell_1 = input$selected_mutation_chr_sort_cell_clicked
              
              ## find the value of the selected cell
              
              chr_location_value_1 = chr_location_cell_1$value
              
              ## get the chromosome number from the cell to the left of selected cell
              
              chr_number_1 = mutation_data_table()[input$selected_mutation_chr_sort_cell_clicked$row,which(colnames(mutation_data_table())=="Chr")]
              
              
              ## images use old sample id naming
              
              old_donor_name_1 = as.vector(mapping_IDs$sample[which(as.vector(mapping_IDs$fixed_sample_IDs) == input$donor_choice_mutation)])
              
              ##which(colnames(mutation_data_table())=="sample")
              ##match("sample",names(mutation_data_table()))
              ## ^^ options for retrieving col number given column name sample
              
              donor_id_from_column_1 = mutation_data_table()[input$selected_mutation_chr_sort_cell_clicked$row, which(colnames(mutation_data_table())=="sample")]
              
              ##concatenates the correct bamsnap folder (either indels or snvs)
              
              bamsnap_folder_1 = paste0("bamsnap_",input$var)
              
              
              ## concatenate the call to file to pull the sequencing image
              
              sequencing_image_1 <- paste0("/",bamsnap_folder_1,"/",donor_id_from_column_1,"/",chr_number_1,"_",chr_location_value_1,".png")
              
              ## renders the sequencing image, and makes sure there is not a 'broken image' icon before the user selects an image
              
              tags$img(src= sequencing_image_1, alt = "")
      
      
    })
    
    ## renders bamsnap image for datatable selecting by chr when input show_bamsnap_image_chr is enacted 
    
    observeEvent(input$show_bamsnap_image_chr,{
          insertUI(
            selector = "div#all_div",
            where = c("afterBegin"),
            uiOutput("bamsnap_image_selected_chrs")
          )
      
    })
  
    ## clear image for mutation data table sorting by chromosome 
    
    observeEvent(input$clear_bamsnap_image_chr, {
      
          removeUI(
            selector = "div#bamsnap_image_selected_chrs_div"
        )

    })

    

    
    ## All functionality for the cBioPortal tab is done on the ui side ---------------
      
    }
    
    
    
    
    
    
    
    
    
    
    
    
