# mpnst_surveyor

 Comprehensive genomic analysis from the sequencing of MPNST tumors. Research conducted through the GeM Consortium of the NF Research Initiative. 
 
 Details:
 
 Made using R Shiny, styled with HTML and CSS.
 
 Deployed on AWS EC2 instance, instructions below ---
 
 
 # Prerequisites: 
 
 docker 
 
 Set Up: 
 
 #clone repository from github 
 
 git clone https://github.com/colinmiller20/mpnst_surveyor.git
 
 #enter working directory of the repo 
 
 cd mpnst_surveyor 
 
 #create docker image from within mpnst_surveyor directory 
 
 docker build -t mpnst_app . 
 
 #after container 'mpnst_app' is built, add in the necessary data files as volumes 
 
 - Data volumes to have on instance (available on EBI Cluster) 
 
 - bamsnap_INDEL.tar.gz
 - bamsnap_SNV.tar.gz
 - consensus_SNV_all_samples.rds 
 
#unpack tar files 

 tar -xvf bamsnap_INDEL.tar.gz
 
 tar -xvf bamsnap_SNV.tar.gz
 
 tar -xvf consensus_SNV_all_samples.rds
 
 leave these files in home directory /home/ubuntu or adjust next statement 
 
 ------------------------------------
 
 #docker command to add volumes and run
 
sudo docker container run -d -p 8000:8000 \
-v /home/ubuntu/mpnst_surveyor/mpnst_app/:/srv/shinyapps/mpnst_app/ \
-v /home/ubuntu/bamsnap_SNV/:/srv/shinyapps/mpnst_app/www/SNV_reads/ \ 
-v /home/ubuntu/bamsnap_INDEL/:/srv/shinyapps/mpnst_app/www/INDEL_reads/ \
-v /home/ubuntu/consensus_SNV_all_samples.rds:/srv/shinyapps/mpnst_app/consensus_SNV_ind/ \ 
-v /home/ubuntu/log/shiny-server/:/var/log/shiny-server/ \
mpnst_app




 
 
 

 
 
 
