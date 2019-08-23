FROM rocker/shiny
RUN R -e "install.packages(c('shiny', 'DT','shinythemes','openxlsx','dplyr','scales','ggplot2','tidyr'), repos='https://cran.rstudio.com/')"
COPY /app/ /srv/shiny-server/
