library(ggplot2)
library(DT)
library(BioCircos)
library(shiny)
library(shinyWidgets)
library(RColorBrewer)
library(data.table)
source("biocircos_plots.R")
#colours_ICGC= read.table("ICGC_colours_2.txt",sep="\t",header=F,stringsAsFactors = F,comment.char="")
#d$colour_ICGC = colours_ICGC$V2[match(d$histo,colours_ICGC$V1)]

source("load_data.R")

function(input, output, session) {

 

  #    observe({
#        output$cancer_type <- renderUI({
#            selectizeInput(multiple=T,'donor', 'Donor ID', choices_cancer)
#        })
#    })
    
    
    # defaultColors = colours_ICGC$V2
    # 
    # series <- structure(
    #     lapply(defaultColors, function(color) { list(color=color) }),
    #     names = colours_ICGC$V1
    # )
    # 
  
  
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
    
    
     output$chr_selection_circos  <- renderUI({
         pickerInput( 'chr_selection_circos', 'Chromosome selection for circos plot',
                      choices = as.character(paste0("chr",c(1:22,"X"))),
                      #selected=get_with2(d,input)[[2]],
                      selected=as.character(paste0("chr",c(1:22,"X"))),
                      options = list( `actions-box` = TRUE, size = 5,
                                      style = "btn-info",
                                      `selected-text-format` = "count > 3" ), multiple = TRUE )
         
     })
    #
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


	# Table for SNVs
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
        DT::datatable(loadsnvtable(), style = 'bootstrap',filter = list(position = "top", clear = FALSE), options=list(pageLength = 10, lengthMenu = c( 10, 25, 100), scrollX = T ))
    })
    output$table_CN <- DT::renderDataTable({
      DT::datatable(loadcntable(), style = 'bootstrap',filter = list(position = "top", clear = FALSE), options=list(pageLength = 10,lengthMenu = c( 10, 25, 100), scrollX = T ),
                    colnames = c('Chr', 'Start', 'End', 'Orientation', 'Gene','Total CN','Minor CN'))
    })
    output$table_INDELs <- DT::renderDataTable({
      DT::datatable(loadindeltable(), style = 'bootstrap',filter = list(position = "top", clear = FALSE), options=list(pageLength = 10,lengthMenu = c( 10, 25, 100), scrollX = T ))
    })
    
	}
	)

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



    #-------------------------------------------------
    # Table with donor information (e.g. age, etc..)
    output$table_donor_info <- DT::renderDataTable({
        DT::datatable(selectedData_donor_info(),style = 'bootstrap',options = list(sDom  = '<"top">lrt<"bottom">ip') )
    })



    output$find_case <- DT::renderDataTable({
        DT::datatable(selectedData_find_case(), style = 'bootstrap',filter = list(position = "top") )
    })
    
    
    
    #import file for the sample data table with clinical and genomic data 
    
    sample_data_table <- read.delim('combined_sample_table_genomic_clinical_data.txt')
    
    
    ## sets up which columns in sample_data_table to keep 
    
    #columns_to_keep <- c("Subject.ID","gender","NF.Status.x")
    
    #sample_data_table <- sample_data_table[columns_to_keep]
    
    #describes the output of the function "sampletable", which renders a data table using 
    #the file "firsttabledata"
    
    output$sampletable <- DT::renderDataTable({ DT::datatable(sample_data_table, filter = list(
      position = 'top', clear = FALSE),  options = list(ordering=F)) 
    })
    
    
    #reactive function that takes in the users input of which sample they want and concatenates to call the correct
    #file to be read 
    
    
    mutation_data_table <- reactive({   #XXXX ew need to load multiple donors
      #print(input$donor_choice_mutation) 
      #input_donor_choice_mutation = as.vector( mapping_IDs$sample[which(as.vector(mapping_IDs$fixed_sample_IDs) == input$donor_choice_mutation)] )


    if(input$type=="Select Donors"){
      
    dons = c(); mm =c()

#	 for (i in 1:length(input$var)) {
	   
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
	   
#	 }
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
    
   
     #output of data_table for mutation data (snv/indels)
    
    

    
    output$selected_mutation_no_chr_sort <- DT::renderDataTable({
      
      DT::datatable(mutation_data_table(), filter = list(   
        position = 'top', clear = FALSE), 
        
        selection=list(mode="single", target="cell")
      )
      
    })
    
    
    output$selected_mutation_chr_sort <- DT::renderDataTable({
      
      
      
      ## get all the chromosomes selected by the user 
      
      chr_choices <- input$chr_selection_mutation_table
      
      
      ##change to data.table because need to show only rows with specific chromosome values 
      
      x <- data.table(mutation_data_table())
      
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
      
      chr_location_cell = input$selected_mutation_no_chr_sort_cell_clicked
      
      
      ## find the value of the selected cell 
      
      chr_location_value = chr_location_cell$value
      
      ## get the chromosome number from the cell to the left of selected cell 
      
      chr_number = mutation_data_table()[input$selected_mutation_no_chr_sort_cell_clicked$row,which(colnames(mutation_data_table())=="Chr")]
      
      
      ## images use old sample id naming 
      
      old_donor_name = as.vector(mapping_IDs$sample[which(as.vector(mapping_IDs$fixed_sample_IDs) == input$donor_choice_mutation)])
      
      ##which(colnames(mutation_data_table())=="sample")
      ##match("sample",names(mutation_data_table()))
      ## ^^ options for retrieving col number given column name sample 
      
      donor_id_from_column = mutation_data_table()[input$selected_mutation_no_chr_sort_cell_clicked$row,which(colnames(mutation_data_table())=="sample")]
      
      
      bamsnap_folder = paste0("bamsnap_",input$var)
      
      
      
      ## concatenate the call to file to pull the sequencing image 
      
      sequencing_image <- paste0("/",bamsnap_folder,"/",donor_id_from_column,"/",chr_number,"_",chr_location_value,".png")
      
      print(sequencing_image)
      
      
      tags$img(src= sequencing_image)
      
      
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
      print(donor_id_from_column_1)
      
      
      bamsnap_folder_1 = paste0("bamsnap_",input$var)
      
      
      
      ## concatenate the call to file to pull the sequencing image
      
      sequencing_image_1 <- paste0("/",bamsnap_folder_1,"/",donor_id_from_column_1,"/",chr_number_1,"_",chr_location_value_1,".png")
      
      
      print(sequencing_image_1)
      tags$img(src= sequencing_image_1)
      
      
    })
    
    
    
    }
    
    
    
    
    
    
    
    
    
    
    
    
