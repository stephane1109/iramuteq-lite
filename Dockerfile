# Rôle du fichier: Dockerfile porte une partie du pipeline d'analyse Rainette.
# Ce script centralise une responsabilité métier/technique utilisée par l'application.

FROM rocker/r2u:22.04

ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8
ENV DEBIAN_FRONTEND=noninteractive

# Python pour spaCy
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      ca-certificates \
      python3 \
      python3-pip \
    && rm -rf /var/lib/apt/lists/*

# Paquets R (r2u installe en binaires via apt/bspm, donc très rapide)
RUN install.r shiny rainette quanteda wordcloud RColorBrewer igraph dplyr htmltools remotes irlba

# FactoMineR depuis GitHub (sans tirer les Suggests)
RUN R -q -e "options(repos=c(CRAN='https://cloud.r-project.org')); remotes::install_github('husson/FactoMineR', dependencies=NA, upgrade='never')"

# spaCy 3.7.2 + lookups + modèles FR/EN/ES/IT/DE/PT/CA (md, wheels)
RUN python3 -m pip install --no-cache-dir --upgrade pip setuptools wheel && \
    python3 -m pip install --no-cache-dir spacy==3.7.2 spacy-lookups-data && \
    python3 -m pip install --no-cache-dir \
      https://github.com/explosion/spacy-models/releases/download/fr_core_news_md-3.7.0/fr_core_news_md-3.7.0-py3-none-any.whl \
      https://github.com/explosion/spacy-models/releases/download/en_core_web_md-3.7.1/en_core_web_md-3.7.1-py3-none-any.whl \
      https://github.com/explosion/spacy-models/releases/download/es_core_news_md-3.7.0/es_core_news_md-3.7.0-py3-none-any.whl \
      https://github.com/explosion/spacy-models/releases/download/it_core_news_md-3.7.0/it_core_news_md-3.7.0-py3-none-any.whl \
      https://github.com/explosion/spacy-models/releases/download/de_core_news_md-3.7.0/de_core_news_md-3.7.0-py3-none-any.whl \
      https://github.com/explosion/spacy-models/releases/download/pt_core_news_md-3.7.0/pt_core_news_md-3.7.0-py3-none-any.whl \
      https://github.com/explosion/spacy-models/releases/download/ca_core_news_md-3.7.0/ca_core_news_md-3.7.0-py3-none-any.whl
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

RUN chown -R user:user /home/user/app

USER user
EXPOSE 7860
CMD ["Rscript", "/home/user/app/rainette/start.R"]
