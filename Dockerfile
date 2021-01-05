FROM rocker/r-ver:3.6.3
LABEL maintainer="ahmed"

RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
  && apt-get install -y zlib1g-dev
RUN ["install2.r", "caret", "data.table", "ggplot2", "lattice", "lubridate", "pbapply", "Rcpp", "RSNNS", "signal"]

#Copy the folder withthe files
COPY myFiles myFiles

WORKDIR /myFiles/

#open interactive mode
CMD ["/bin/bash"]

