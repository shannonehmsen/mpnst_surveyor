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
    
    # 
    # yearData <- reactive({
    #     print(d$donor_age_at_diagnosis)
    #     idx = which(d$purity >= input$purity & d$ploidy >= input$ploidy & d$histo %in% input$cancer_type & 
    #                     d$Chr %in% input$chrom)
    #     all_cases=d[idx,]
    #     df = unique(d[idx, c("donor_unique_id ","purity","ploidy","histo","SVs in sample","donor_age_at_diagnosis")])
    #     
    #     ## second data frame 
    #     idx2 = which(d$purity >= input$purity & d$ploidy >= input$ploidy & d$histo %in% input$cancer_type &
    #                      d$Chr %in% input$chrom & 
    #                      d[,"Nb. oscillating CN"] >= input$oscil2 & d$ratio >= input$fraction_SVs_chromo)
    #     
    #     d$donor_chr = paste(d$donor_unique_id , "; chromosome ",d$Chr,sep="")
    #     df2 = unique(d[idx2, c("donor_chr","ratio","Nb. oscillating CN","histo","SVs in sample","Chr","donor_age_at_diagnosis")])
    #     names(df2) = c("donor_chr","Fraction of SVs in the tumor involved in chromothripsis and mapped to this chr","Nb. oscillating CN between 2 states","cancer type","SVs in sample","Chr","donor_age_at_diagnosis")
    #     names(df)[4] = names(df2)[4] = "Cancer type"
    #     return(list(df,df2,all_cases))
    # })
    # 
    # 
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
    # 
    # 
    # 
    # 
    # selectedData_all_chrs <- reactive({
    #     #d$zcomment = d$comment	
    #     out = d[which(d$donor_unique_id  == input$donor & d$Chr %in% input$cols_to_show),]
    #     
    #     col_names=c(
    #         "chromo_label" ,                                                     
    #         "type_chromo",   
    #         "Chr"    ,                                  
    #         "Start"  ,                                  
    #         "End" ,                                     
    #         "Intrchr. SVs" ,                            
    #         "Total SVs (intrachr. + transl.)",          
    #         "SVs in sample"  ,                          
    #         "Nb. DEL"  ,                                
    #         "Nb. DUP"  ,                                
    #         "Nb. h2hINV"  ,                             
    #         "Nb. t2tINV" ,                              
    #         "Nb. TRA",
    #         "Nb. CN segments"   ,        
    #         "Nb. oscillating CN"    ,                   
    #         "Nb. oscillaring CN 3 states"  ,           
    #         "CN segments chr."  ,                       
    #         "Nb. oscillating chr."   ,                  
    #         "Nb. oscillating chr 3 states",   
    #         "P fragment joints" ,                       
    #         "P chr breakpoint enrichment" ,                            
    #         "P exponential dist. cluster" ,                                         
    #         "clustered APOBEC mutations (<2.5 Kb)"  ,                           
    #         "Other interacting chromothripsis regions", 
    #         "FDR fragment joints intrachr. SVs and TRA",
    #         "FDR chr breakpoint enrichment",            
    #         "FDR exponential dist."    ,                
    #         "FDR exponential dist. cluster"  ,          
    #         "FDR fragment joints intrachr. SVs"   ,                                                    
    #         "TP53_mutations",                           
    #         "MDM2_CN",
    #         "comment"
    #     )
    #     
    #     # select table for only the chrs with chromothripsis
    #     ordered = c("1" , "2" , "3" , "4" , "5" , "6" , "7" , "8",  "9"  ,"10", "11" ,"12" ,"13", "14" ,"15",
    #                 "16", "17" ,"18" ,"19" ,"20", "21", "22","X")
    #     ordered = ordered[ordered %in% input$cols_to_show]
    #     out = out[match(ordered,out$Chr),]
    #     
    #     out = out[,match(col_names,names(out))]
    #     names(out)[4] = "Start coordinate for SV cluster in chr"
    #     names(out)[5] = "End coordinate for SV cluster in chr"
    #     names(out)[6] = "Nb. intrachromosomal SVs"
    #     names(out)[7] = "Total Nb. SVs (intrachr. + interchr.)"
    #     names(out)[8] = "Nb. SVs in sample"
    #     names(out)[which(names(out) == "Nb. CN segments")] = "Nb. CN segments in SV cluster"
    #     names(out)[which(names(out) == "Nb. oscillating chr.")] = "Nb. oscillating CN across 2 states in chr."
    #     names(out)[which(names(out) == "CN segments chr.")] = "Nb. CN segments in chr."
    #     names(out)[which(names(out) == "Nb. oscillating CN")] = "Nb. oscillating CN across 2 states"
    #     names(out)[which(names(out) == "Nb. oscillaring CN 3 states")] = "Nb. oscillating CN across 3 states"
    #     names(out)[which(names(out) == "Nb. oscillating chr 3 states")] = "Nb. oscillating CN across 3 states in chr."
    #     names(out)[which(names(out) == "Nb. oscillating chr.")] = "Nb. oscillating CN across 2 states in chr."          
    #     
    #     
    #     
    #     names(out)[1:2] = c("Chromothripsis in chromosome?","Chromothripsis category")
    #     chs = out$Chr
    #     out$Chr = NULL
    #     out = data.frame(t(out))
    #     colnames(out) = as.vector(chs)
    #     # chr selection
    #     idx=which(colnames(out) %in% input$cols_to_show)
    #     out = out#[,idx] ##which(colnames(out) %in% input$cols_to_show)]
    #     #return(out)} else{
    #     return(out)
    # })
    # 
    # 
    
    
    
    # get_canc_types = function(d,input){
    #     choices = sort(unique(d$histo[which(d$type_chromo %in% input$chromo_type)]))
    #     #if (length(choices) == 0){ # we assume ther is always a valid choice (i.e. we have tumors for all categories of chromotype)
    #     #}
    #     return(choices)
    # }
    
    # 
    # output$cancer_type2 <- renderUI({
    #     pickerInput('cancer_type2', 'Cancer type',
    #                 selected="Bladder-TCC", #"Biliary-AdenoCA",
    #                 choices=get_canc_types(d,input), #sort(unique(d$histo[which(d$type_chromo %in% input$chromo_type)])),
    #                 options = list( `actions-box` = TRUE, `size`= 5, 
    #                                 style = "btn-info",
    #                                 `selected-text-format` = "count > 3" ), 
    #                 multiple = TRUE )
    # })
    # 
    # 
    
    
    
    
    # get_with = function(d,input){
    #     d$histo=as.vector(d$histo) #XX
    #     out=c()
    #     typ =c("After polyploidization","Before polyploidization","Canonical without polyploidization","With other complex events")
    #     if ( sum(input$chromo_type %in% typ)>0 ){
    #         typ_now = typ[which(typ %in% input$chromo_type)]
    #         #print(nrow(d))
    #         #print(which(d$histo %in% input$cancer_type2 & d$type_chromo %in% typ_now))
    #         out = sort(unique(d$donor_unique_id [which(d$histo %in% input$cancer_type2 & d$type_chromo %in% typ_now)]))
    #     }
    #     if ("No chromothripsis" %in% input$chromo_type){
    #         now = d[which(d$histo %in% input$cancer_type2 & d$type_chromo %in% c("No chromothripsis") ),]
    #         now_sum = ddply(now,.(donor_unique_id ),summarise,tot=sum(type_chromo=="No chromothripsis",na.rm=T))
    #         now_sum = unique(as.vector(now_sum$donor_unique_id [now_sum$tot==23]))
    #         #now_sum = ddply(now,.(donor_unique_id ),summarise,tot=sum(type_chromo!="No chromothripsis"))
    #         #now_sum = unique(now_sum$donor_unique_id [now_sum$tot==0])
    #         out =c(out, now_sum)
    #     }
    #     #XXX create a global variable with the types of chromo at the moment y make the donor choice dependent on it with observe event.
    #     #input3 <<- list(cancer_type=input$cancer_type2,typ = input$chromo_type, donor=input$donor,
    #     #					donor=input$donor, chr_selection_circos=out,range_dist=input$range_dist, show_patho_indels = input$show_patho_indels, show_nopatho_indels =input$show_nopatho_indels, show_patho = input$show_patho, show_nopatho= input$show_nopatho, show_chromo_track=input$show_chromo_track, show_genes=input$show_genes)
    #     return(out)
    # }
    
    
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
    
    
    ###### get the chrs with chromothripsis in the selected tumor
    
    # get_chrs_with = function(d,input){
    #     out=c()
    #     types_now = d[which(d$donor_unique_id  == input$donor),] #& d$type_chromo %in% input$chromo_type & d$histo %in% input$cancer_type2),]
    #     types_now_chromo_type = types_now$type_chromo
    # 
    #     typp =c("After polyploidization","Before polyploidization","Canonical without polyploidization","With other complex events")
    #     if ( sum(types_now_chromo_type %in% typp)>0 ){
    #         out= types_now$Chr[ which(types_now$type_chromo %in% typp)]
    #     } else{ out=c("22")}
    #     #update_circos <<- TRUE
    #     input2 <<- list(#cancer_type=input$cancer_type2,
    #         #typ = input$chromo_type,
    #         donor=input$donor,
    #         chr_selection_circos=out,range_dist=input$range_dist, show_patho_indels = input$show_patho_indels, show_nopatho_indels =input$show_nopatho_indels, show_patho = input$show_patho, show_nopatho= input$show_nopatho, show_chromo_track=input$show_chromo_track, show_genes=input$show_genes)
    # 
    #     return(out)
    # }

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
        DT::datatable(loadsnvtable(), style = 'bootstrap',filter = list(position = "top"), options=list(pageLength = 10,lengthMenu = c( 10, 25, 100), scrollX = T ))
    })
    output$table_CN <- DT::renderDataTable({
      DT::datatable(loadcntable(), style = 'bootstrap',filter = list(position = "top"), options=list(pageLength = 10,lengthMenu = c( 10, 25, 100), scrollX = T ),
                    colnames = c('Chr', 'Start', 'End', 'Orientation', 'Gene','Total CN','Minor CN'))
    })
    output$table_INDELs <- DT::renderDataTable({
      DT::datatable(loadindeltable(), style = 'bootstrap',filter = list(position = "top"), options=list(pageLength = 10,lengthMenu = c( 10, 25, 100), scrollX = T ))
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




# selectedData_find_case <- reactive({
#     out = d# [which(d$donor_unique_id  == input$donor),]
#     out$tp53_bin="Yes"; out$tp53_bin[is.na(out$TP53_mutations)]="No"; out$tp53_bin=factor(out$tp53_bin)
#     out2=ddply(out,.(donor_unique_id ),summarise,chrs_affected=sum(chromo_label!="No"))
#     out$chrs_affected = out2$chrs_affected[match(out$donor_unique_id ,out2$donor_unique_id )]
# 
#     col_names=c(#"chromo",
#         "histo",
#         "chromo_label" ,
#         "type_chromo",
#         "donor_unique_id " ,
#         "icgc_donor_id",
#         "Chr"    ,
#         #"Start"  ,
#         #"End" ,
#         #"Intrchr. SVs" ,
#         #"Total SVs (intrachr. + transl.)",
#         "breakpoints",
#         "chrs_affected",
#         "SVs in sample"  ,
#         #"Nb. DEL"  ,
#         #"Nb. DUP"  ,
#         #"Nb. h2hINV"  ,
#         #"Nb. t2tINV" ,
#         #"Nb. TRA",
#         "Nb. CN segments"   ,
#         "Nb. oscillating CN"    ,
#         #"Nb. oscillaring CN 3 states"  ,
#         "CN segments chr."  ,
#         #"Nb. oscillating chr."   ,
#         #"Nb. oscillating chr 3 states",
#         #"P fragment joints" ,
#         #"P chr breakpoint enrichment" ,
#         #"P exponential dist. cluster" ,
#         #"clustered APOBEC mutations (<2.5 Kb)"  ,
#         #"Other interacting chromothripsis regions",
#         #"FDR fragment joints intrachr. SVs and TRA",
#         #"FDR chr breakpoint enrichment",
#         #"FDR exponential dist."    ,
#         #"FDR exponential dist. cluster"  ,
#         #"FDR fragment joints intrachr. SVs"   ,
#         "TP53_mutations",
#         "tp53_bin",
#         "MDM2_CN"
#     )
# 
#         # select table for only the chrs with chromothripsis
#         #ordered = c("1" , "2" , "3" , "4" , "5" , "6" , "7" , "8",  "9"  ,"10", "11" ,"12" ,"13", "14" ,"15",
#         #				"16", "17" ,"18" ,"19" ,"20", "21", "22","X")
#         #out = out[match(ordered,out$Chr),]
#         #out = out[order(out$Chr,decreasing=F),]
#         out$chrnum = as.vector(out$Chr); out$chrnum[out$chrnum=="X"]="23"; out$chrnum=as.numeric(out$chrnum)
#         out = with(out, out[order(histo,donor_unique_id ,chrnum),])
#         print(head(out[,c("histo","Chr","chrnum","donor_unique_id ")]))
#         out$chrnum=NULL
#         out$TP53_mutations = factor(out$TP53_mutations)
# 
#         out = out[,match(col_names,names(out))]
#         names(out)[which(names(out) == "breakpoints")] = "Nb. brekpoints in chromothripsis region"
#         names(out)[which(names(out) == "histo")] = "Cancer type"
#         names(out)[which(names(out) == "chrs_affected")] = "Nb. chrs with chromothripsis"
#         names(out)[which(names(out) == "icgc_donor_id")] = "ICGC donor ID"
#         names(out)[which(names(out) == "tp53_bin")] = "TP53 mutated?"
#         names(out)[which(names(out) == "TP53_mutations")] = "TP53 mut."
#         names(out)[which(names(out) == "type_chromo")] = "Chromothipsis category"
#         names(out)[which(names(out) == "CN segments chr.")] = "Nb. CN segments in chr."
#         out[,1] = factor(out[,1])
#         out[,2] = factor(out[,2])
#         out[,3] = factor(out[,3])
#         out[,4] = factor(out[,4])
#         out[,5] = factor(out[,5])
#         out[,6] = factor(out[,6])
# 
# 
# 
#         names(out)[1:2] = c("Cancer type","Chromothripsis in chromosome?") #Confidence of the chromothripsis call")
#         rownames(out)=NULL
#         return(out)
#     })
    
    
    
    ### MPNST SERVER INTEGRATION 
    
    
    #functions referencing donor choice made in biocircos plots
    
    
    
    
    #import file of sample data
    sample_data_table <- read.delim('combined_sample_table_genomic_clinical_data.txt')
    
    #describes the output of the function "sampletable", which renders a data table using 
    #the file "firsttabledata"
    
    output$sampletable <- DT::renderDataTable({ DT::datatable(sample_data_table, filter = list(
      position = 'top', clear = FALSE)) 
    })
    
    
    #reactive function that takes in the users input of which sample they want and concatenates to call the correct
    #file to be read 
    
    
    mutation_data_table <- reactive({   #XXXX ew need to load multiple donors
      print(input$donor_choice_mutation) 
      #input_donor_choice_mutation = as.vector( mapping_IDs$sample[which(as.vector(mapping_IDs$fixed_sample_IDs) == input$donor_choice_mutation)] )

	  dons = c(); mm =c()

	  for (j in 1:length(input$donor_choice_mutation)){
		  input_donor_choice_mutation = as.vector( mapping_IDs$sample[which(as.vector(mapping_IDs$fixed_sample_IDs) == input$donor_choice_mutation[j])] )
		  dons = c(dons,input_donor_choice_mutation)
      input_file_mutation <- paste0("./consensus_",input$var,"_ind/consensus_",input$var,"_",input_donor_choice_mutation,".rds")
	  mm = rbind(mm, readRDS(input_file_mutation))
	  }
      
      #input_file_mutation <- paste0("./consensus_",input$var,"_ind/consensus_SNV_all_samples.rds")
	  #print(input_file_mutation)
      #mm = readRDS(input_file_mutation)
	  #mm = mm[which(mm$sample %in% dons),] #input$donor_choice_mutation),]
	  rownames(mm) = 1:nrow(mm)
	  if(input$var == "SNV"){
	  mm$normal_DP = round(mm$normal_DP)
	  mm$tumour_DP = round(mm$tumour_DP)
	  mm$normal_VAF = round(mm$normal_VAF,digits=2)
	  mm$tumour_VAF = round(mm$tumour_VAF,digits=2)
	  }else{ #indels
		  mm$tumour_DP = round(mm$tumour_DP)
mm$tumour_VAF = round(mm$tumour_VAF,digits=2)
	  }
	  return(mm)
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



    
    }
    
    
    
    
    
    
    
    
    
    
    
    
