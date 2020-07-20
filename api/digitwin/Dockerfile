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


RUN mkdir -p /root/data/tki
COPY wmr /root/data/tki/wmr
# prepare the environment, download vmstools
COPY src /root/src
WORKDIR /root/src
RUN Rscript prep.R

EXPOSE 8000
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb(commandArgs()[4]); pr$run(host='0.0.0.0', port=8000)"]
CMD ["/usr/local/lib/R/site-library/plumber/examples/04-mean-sum/plumber.R"]
