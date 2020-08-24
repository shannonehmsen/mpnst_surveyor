FROM rocker/shiny-verse:latest

# install necessary libraries

RUN sudo apt-get update && apt-get install -y \
    nano \
    libssl-dev \
    libxml2-dev


# install necessary R packages
RUN  echo 'install.packages(c("DT", "shinyjs", "ggplot2", "plyr", "RColorBrewer", "shinythemes", "data.table", "htmlwidgets"),\
           repos="http://cran.us.r-project.org", \
           dependencies=TRUE)' \
           > /tmp/packages.R && Rscript /tmp/packages.R


RUN R -e "install.packages(c('devtools'), dependencies=TRUE)"
RUN R -e "devtools::install_github('andrewsali/shinycssloaders')"
RUN R -e "devtools::install_github('dreamRs/shinyWidgets')"

# copy mpnst_app to the image

COPY mpnst_app/ /srv/shinyapps/mpnst_app

#download BioCircos package (modified for hg38 genome build)

RUN R CMD INSTALL /srv/shinyapps/mpnst_app/dist/BioCircos.tar.gz

RUN  echo 'install.packages(c("shiny", "shinyWidgets", "shinydashboard"),\
           repos="http://cran.us.r-project.org", \
           dependencies=TRUE)' \
           > /tmp/packages.R && Rscript /tmp/packages.R


# select port
EXPOSE 8000


# run app
 CMD ["R", "-e", "shiny::runApp('/srv/shinyapps/mpnst_app', 8000, host='0.0.0.0')"]

