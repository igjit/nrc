FROM rocker/tidyverse:3.6.2

RUN install2.r --error \
    zeallot \
 && rm -rf /tmp/downloaded_packages/
