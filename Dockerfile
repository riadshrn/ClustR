FROM rocker/shiny:4.4.1

# 1) Install system libs required by R packages
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    build-essential \
    && apt-get clean

# 2) Install R packages used by your project
RUN R -e "install.packages(c('shiny','shinydashboard','DT','ggplot2','dplyr','tidyr','R6','cluster','factoextra','matrixStats','shinyWidgets','plotly','data.table','ggrepel','rgl','remotes'))"

# 3) Copy your package source into the container
COPY . /opt/ClustR

# 4) Install your package
RUN R -e "remotes::install_local('/opt/ClustR', dependencies = TRUE)"

# 5) Expose Shiny port
EXPOSE 3838

# 6) Run the Shiny app inside the package
CMD ["R", "-e", "shiny::runApp('/opt/ClustR/inst/shiny-app', host='0.0.0.0', port=3838)"]
