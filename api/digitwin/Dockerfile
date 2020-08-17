FROM rocker/r-base
MAINTAINER Fedor Baart <fedor.baart@deltares.nl>

# install packages for building R packages
RUN apt-get update --allow-releaseinfo-change && apt-get install -y \
    file \
    git-core \
    libcurl4-gnutls-dev \
    libxml2-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*


## RUN R -e 'install.packages(c("devtools"))'
## RUN R -e 'devtools::install_github("trestletech/plumber")'
RUN install2.r plumber

# prepare the environment, download vmstools
COPY src /root/src
WORKDIR /root/src
RUN Rscript prep.R

HEALTHCHECK CMD curl -f http://localhost:8000 || exit 1
EXPOSE 8000

ENTRYPOINT ["R", "-e", "pr <- plumber::plumb(\"plumber.R\"); pr$run(host='0.0.0.0', port=8000)"]
CMD ["plumber.R"]
