FROM rocker/r-ver:4.1.0

WORKDIR /project

# renv
COPY Code/CaseStudy2/renv.lock renv.lock
RUN mkdir -p renv
COPY Code/CaseStudy2/.Rprofile .Rprofile
COPY Code/CaseStudy2/renv/activate.R renv/activate.R
COPY Code/CaseStudy2/renv/settings.dcf renv/settings.dcf

RUN R -e "renv::restore()"

# DIMEX source code
COPY Code/CaseStudy2 .

CMD Rscript cli.R --help
