

file_list <- list.files()


a <- 


nn =c()


for(file in file_list){
  
  nn = rbind(mm, readRDS(file))
  
}



for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- readRDS(file)
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-readRDS(file)
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}
