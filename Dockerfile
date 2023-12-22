FROM inwt/r-batch:4.2.1

ADD . .

RUN apt-get update -y
RUN apt-get install -y libgdal-dev
RUN apt-get install -y libudunits2-dev
RUN Rscript -e "install.packages(c('unix', 'R.utils'))"

RUN installPackage
