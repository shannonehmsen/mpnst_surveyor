library(ggplot2)
library(DT)
library(BioCircos)
library(shiny)
library(shinyWidgets)
library(RColorBrewer)
source("biocircos_plots.R")
#colours_ICGC= read.table("ICGC_colours_2.txt",sep="\t",header=F,stringsAsFactors = F,comment.char="")
#d$colour_ICGC = colours_ICGC$V2[match(d$histo,colours_ICGC$V1)]


source("load_data.R")

function(input, output, session) {
#    
#    #choices_cancer = sort(unique(d$histo)) 
#    
#    
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
    
    
    selectedData <- reactive({
        d[which(d$donor_unique_id  == input$donor & d$Chr == input$chromosome),]
    })
    
    
    selectedData_donor_info <- reactive({
        out = unique(clinical[which(clinical$donor_unique_id  == input$donor),])
        out = t(unique(out))
        colnames(out)="Value"
        return(out)
    })
    
  
    
    get_donor_list = function(d,input){
        
        d_unique = sort(unique(d$donor_unique_id))
        return(d_unique)
    }
    
    
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



    
    # output$mytable2 <- DT::renderDataTable({
    #     DT::datatable(selectedData_all_chrs(), style = 'bootstrap') %>% formatStyle(
    #         #c(as.character(1:22,"X")),
    #         input$cols_to_show,
    #         #get_chrs_with(d,input),
    #         backgroundColor = styleEqual(c("High confidence","Linked to high confidence", "Linked to low confidence","Low confidence","No"), 
    #                                      c('#eded2a','#eded2a','#a1d99b','#a1d99b','#ece2f0'))
    #         #c('#feb24c', '#feb24c','#a1d99b','#a1d99b','#ece2f0'))
    # #     )
    # # })
    # 
    #-------------------------------------------------
    # Table with donor information (e.g. age, etc..)
    output$table_donor_info <- DT::renderDataTable({
        DT::datatable(selectedData_donor_info(),style = 'bootstrap',options = list(sDom  = '<"top">lrt<"bottom">ip') )
    })



    output$find_case <- DT::renderDataTable({
        DT::datatable(selectedData_find_case(), style = 'bootstrap',filter = list(position = "top") )
    })

    
    
    
    ### MPNST SERVER INTEGRATION 
    
    
    #functions referencing donor choice made in biocircos plots
    
    
    
    
    #import file of sample data
    sample_data_table <- read.delim('combined_sample_table_genomic_clinical_data.txt')
    
    #describes the output of the function "sampletable", which renders a data table using 
    #the file "firsttabledata"
    
    output$sampletable <- DT::renderDataTable({ DT::datatable(sample_data_table, filter = list(
      position = 'top', clear = FALSE),  options = list(ordering=F)) 
    })
    
    
    #reactive function that takes in the users input of which sample they want and concatenates to call the correct
    #file to be read 
    
    
    mutation_data_table <- reactive({   #XXXX ew need to load multiple donors
      print(input$donor_choice_mutation) 
      #input_donor_choice_mutation = as.vector( mapping_IDs$sample[which(as.vector(mapping_IDs$fixed_sample_IDs) == input$donor_choice_mutation)] )

	  dons = c(); mm =c()

	  for (j in 1:length(input$donor_choice_mutation)){
		  input_donor_choice_mutation = as.vector(mapping_IDs$sample[which(as.vector(mapping_IDs$fixed_sample_IDs) == input$donor_choice_mutation[j])])
		  dons = c(dons,input_donor_choice_mutation)
      input_file_mutation <- paste0("./consensus_",input$var,"_ind/consensus_",input$var,"_",input_donor_choice_mutation,".rds")
      print(input$var)
      print(input_file_mutation)
      print(input_donor_choice_mutation)
      t = readRDS(input_file_mutation)
      print(t)
	  mm = rbind(mm, readRDS(input_file_mutation))
	  print(head(mm))
	  }
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
	      })
    
    #output of data_table for mutation data (snv/indels)
    
    output$selected_mutation <- DT::renderDataTable({ DT::datatable(mutation_data_table(), filter = list(   
      position = 'top', clear = FALSE))
    })
    
    
    
    ###CN Tables: 
    
    #Lets the user choose between which sample to display the CN data for, will correspond with the donor that is specified  
    
    # CN_data_table <- reactive({
    #   
    #   input_file_CN <- paste0("./CN_for_all_genes/CN_for_all_genes_",input$donor,".bed")
    #   read.table(input_file_CN,header = FALSE, sep="\t",stringsAsFactors=FALSE, quote="")
    # })
    # 
    # 
    # output$selected_var2 <- DT::renderDataTable({ DT::datatable(CN_data_table(), filter = list(
    #   position = 'top', clear = FALSE))
    # })
    # 

    
    ### reload biocircos tab opion 
    
    ## if clicked, this action button reloads the tab containing the biocircos plots 
    
    # observeEvent(input$link_to_reload_biocircos, {
    #     plot_biocirc(input)#$chrs_selection_circos) XXXXXX
    #   
    # 
    # })


    
    }
    
    
    
    
    
    
    
    
    
    
    
    
