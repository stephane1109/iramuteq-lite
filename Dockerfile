# Rôle du fichier: Dockerfile porte une partie du pipeline d'analyse IRaMuTeQ-like.
FROM rocker/r2u:22.04

ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8
ENV DEBIAN_FRONTEND=noninteractive
ENV R_LIBS_USER=/usr/local/lib/R/site-library
ENV R_LIBS_SITE=/usr/local/lib/R/site-library

# Python pour spaCy
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      ca-certificates \
      python3 \
      python3-pip \
      r-cran-bspm \
    && rm -rf /var/lib/apt/lists/*

# Paquets R (installation explicite via install.packages pour compatibilité CI)
RUN R --vanilla -q -e "Sys.setenv(R_BSPM_DISABLE='true', BSPM_DISABLE='true'); options(repos=c(CRAN='https://cloud.r-project.org')); install.packages(c('shiny','quanteda','wordcloud','RColorBrewer','igraph','dplyr','htmltools','remotes','irlba'))"

# FactoMineR depuis GitHub (sans tirer les Suggests)
RUN R -q -e "options(repos=c(CRAN='https://cloud.r-project.org')); if (!requireNamespace('remotes', quietly=TRUE)) install.packages('remotes'); remotes::install_github('husson/FactoMineR', dependencies=NA, upgrade='never')"

# Utilisateur non-root compatible Hugging Face
RUN set -eux; \
    if ! id -u user >/dev/null 2>&1; then \
      if getent passwd 1000 >/dev/null 2>&1; then \
        useradd -m -u 1001 user; \
      else \
        useradd -m -u 1000 user; \
      fi; \
    fi

ENV HOME=/home/user
WORKDIR /home/user/app

COPY . /home/user/app

RUN chown -R "$(id -u user):$(id -g user)" /home/user/app

USER user
EXPOSE 7860

# Lancement direct minimal de l'app Shiny
CMD ["R", "--vanilla", "-q", "-e", "port <- as.integer(Sys.getenv('PORT', '7860')); shiny::runApp('/home/user/app', host='0.0.0.0', port=port)"]
