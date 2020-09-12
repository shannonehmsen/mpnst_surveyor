# mpnst_surveyor

The MPNST Surveyor is a database for displaying genomic analysis from the sequencing of MPNST tumors. Please visit the <a href="https://www.nfresearch-childrens.org/">NF Research Initiative</a> website to learn more. 

 The website can currently be reached at <a href="http://18.206.76.181:8000/">http://18.206.76.181:8000/</a> 
 
 Details:
 
 Made using R Shiny, styled with HTML and CSS.
 
 Deployed on AWS r4 EC2 instance, instructions below ---
 
 
 # Prerequisites: 
 
- [docker](https://docs.docker.com/engine/installation/)

 # Set Up: 
 
 #clone repository from github 
 
 - git clone https://github.com/parklab/mpnst_surveyor.git
 
 #enter working directory of the repo 
 
 - cd mpnst_surveyor 
 
 #create docker image from within mpnst_surveyor directory 
 
 - docker build -t mpnst_app . 
 
 #after container 'mpnst_app' is built, add in the necessary data files as volumes. 
 Because of the size of these directo
 
 Data volumes to have on instance (available on EBI Cluster) 
 
 - bamsnap_INDEL.tar.gz
 - bamsnap_SNV.tar.gz
 - consensus_SNV_all_samples.rds 
 
# Unpack tar files 

 - tar -xvf bamsnap_INDEL.tar.gz
 
 - tar -xvf bamsnap_SNV.tar.gz
  
 leave all these files/directories in the home directory /home/ubuntu or adjust next statement for dir location 
 
 
 
 ------------------------------------
 
 # Run with Docker
 
 #docker command to add volumes and run on port 8000: 
 (may have to copy each line individually) 
 
sudo docker run -it –rm -p 8000:8000 \\ <br> 
-v /home/ubuntu/bamsnap_SNV:/srv/shinyapps/mpnst_app/www/SNV_reads/bamsnap_SNV \\ <br> 
-v /home/ubuntu/bamsnap_INDEL:/srv/shinyapps/mpnst_app/www/INDEL_reads/bamsnap_INDEL \\ <br> 
mpnst_app

#(OR) docker command to deploy to port 80

sudo docker run -it –rm -p 80:8000 \\ <br> 
-v /home/ubuntu/bamsnap_SNV:/srv/shinyapps/mpnst_app/www/SNV_reads/bamsnap_SNV \\ <br> 
-v /home/ubuntu/bamsnap_INDEL:/srv/shinyapps/mpnst_app/www/INDEL_reads/bamsnap_INDEL \\ <br> 
mpnst_app





 
 
 

 
 
 



 
 
 

 
 
 
