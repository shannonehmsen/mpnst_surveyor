# mpnst_surveyor

 Comprehensive genomic analysis from the sequencing of MPNST tumors. Research conducted through the GeM Consortium of the NF Research Initiative. 
 
 Details:
 
 Made using R Shiny, styled with HTML and CSS.
 
 Deployed on AWS EC2 instance, instructions below ---
 
 
 # Prerequisites: 
 
 docker 
 
 # Set Up: 
 
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
 
# Unpack tar files 

 tar -xvf bamsnap_INDEL.tar.gz
 
 tar -xvf bamsnap_SNV.tar.gz
 
 tar -xvf consensus_SNV_all_samples.rds
 
 leave these directories in home directory /home/ubuntu or adjust next statement for dir location 
 
 ------------------------------------
 
 # Run with Docker
 
 #docker command to add volumes and run on port 8000: 
 (may have to copy each line individually) 
 
sudo docker run -it –rm -p 8000:8000 \\ <br> 
-v /home/ubuntu/bamsnap_SNV:/srv/shinyapps/mpnst_app/www/SNV_reads/bamsnap_SNV \\ <br> 
-v /home/ubuntu/bamsnap_INDEL:/srv/shinyapps/mpnst_app/www/INDEL_reads/bamsnap_INDEL \\ <br> 
mpnst_app

#(OR) docker command to deploy to port 80

sudo docker run -it –rm -p 80:8000 \\
-v /home/ubuntu/bamsnap_SNV:/srv/shinyapps/mpnst_app/www/SNV_reads/bamsnap_SNV \\
-v /home/ubuntu/bamsnap_INDEL:/srv/shinyapps/mpnst_app/www/INDEL_reads/bamsnap_INDEL \\
mpnst_app





 
 
 

 
 
 



 
 
 

 
 
 
