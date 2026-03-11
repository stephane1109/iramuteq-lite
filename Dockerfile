# Rôle du fichier : Dockerfile porte une partie du pipeline d'analyse IRaMuTeQ-like.
FROM rocker/r2u:22.04

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    DEBIAN_FRONTEND=noninteractive \
    HOME=/home/user \
    PATH=/home/user/.local/bin:$PATH

ENV CHD_STATS_CORES=2

# Création de l'utilisateur non-root compatible Hugging Face
RUN set -eux; \
    if ! id -u user >/dev/null 2>&1; then \
      if getent passwd 1000 >/dev/null 2>&1; then \
        useradd -m -u 1001 user; \
      else \
        useradd -m -u 1000 user; \
      fi; \
    fi

WORKDIR /home/user/app

# Python pour spaCy
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      ca-certificates \
      python3 \
      python3-pip \
    && rm -rf /var/lib/apt/lists/*

# Paquets R (r2u installe en binaires via apt/bspm, donc très rapide)
RUN install.r shiny quanteda wordcloud RColorBrewer igraph dplyr htmltools remotes irlba markdown factoextra

# FactoMineR depuis GitHub (sans tirer les Suggests)
RUN R -q -e "options(repos=c(CRAN='https://cloud.r-project.org')); remotes::install_github('husson/FactoMineR', dependencies=NA, upgrade='never')"

# Copie de l'application
COPY --chown=user:user . /home/user/app

USER user
EXPOSE 7860

# Lancement direct de l'application Shiny
CMD ["R", "--vanilla", "-q", "-e", "port <- as.integer(Sys.getenv('PORT', '7860')); shiny::runApp('/home/user/app', host='0.0.0.0', port=port)"]
