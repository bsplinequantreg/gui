# Dockerfile
FROM rocker/shiny:4.6.0

# Installer les dépendances système
RUN apt-get update && apt-get install -y \
    curl \
    build-essential \
    libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    git \
    libwebpmux3 \
    && rm -rf /var/lib/apt/lists/*

# Rust pour CLARABEL
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

# Configurer le repository CRAN
#RUN R -e "options(repos = c(CRAN = 'https://cloud.r-project.org/'))"

# Installer les packages
RUN R -e "install.packages(c('remotes', 'shiny', 'shinythemes', 'shinyjs', 'DT', 'plotly', 'colourpicker', 'png',  'devtools'))"


RUN R -e "install.packages(c('osqp', 'ECOSolveR', 'scs', 'clarabel'),repos = 'https://cloud.r-project.org/')"

#RUN R -e "install.packages('CVXR', repos = 'https://cloud.r-project.org/')"
RUN R -e "remotes::install_version('CVXR', version = '1.9.1', repos = 'https://cloud.r-project.org/', update='never')"

# Installer BsplineQuantReg depuis CRAN
#RUN R -e "remotes::install_version('BsplineQuantReg', repos = 'https://cloud.r-project.org/', version='0.2.2', upgrade='never')"

#depuis Github
RUN R -e "remotes::install_github('alexandreabbes/BsplineQuantReg', ref = 'with_quartic', upgrade='never')"

RUN R -e " packageVersion('BsplineQuantReg') "

# Installer BsplineQuantRegGui (pour les dépendances)
RUN R -e "remotes::install_github('alexandreabbes/BsplineQuantRegGui', ref = 'main')"

# Copier le code source dans le conteneur
COPY . /app

EXPOSE 3838

# Lancer Shiny directement depuis le dossier copié
CMD ["R", "-e", "shiny::runApp('/app/inst/shiny', host='0.0.0.0', port=3838)"]