FROM rocker/shiny-verse:latest

# install necessary libraries 

RUN apt-get update && apt-get install -y \
    sudo \
    nano \
    libssl-dev \
    libssh2-1-dev 
  

# install necessary R packages 


RUN R -e "install.packages('plyr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinythemes', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('DT', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggplot2', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('RColorBrewer', repos='http://cran.rstudio.com/')"

RUN R -e "install.packages(c('devtools'), dependencies=TRUE)"
RUN R -e "devtools::install_github('andrewsali/shinycssloaders')"
RUN R -e "devtools::install_github('dreamRs/shinyWidgets')"

# copy mpnst_app to the image

COPY mpnst_app/ /srv/shiny-server/mpnst_app

#download BioCircos package (modified for hg38 genome build)

RUN R CMD INSTALL /srv/shiny-server/mpnst_app/dist/BioCircos.tar.gz

RUN  echo 'install.packages(c("shiny", "shinyWidgets", "shinydashboard"),\
           repos="http://cran.us.r-project.org", \
           dependencies=TRUE)' \
           > /tmp/packages.R && Rscript /tmp/packages.R


# select port
EXPOSE 3838

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# run app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/mpnst_app', 3838, host='0.0.0.0')"]