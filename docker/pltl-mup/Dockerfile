FROM ubuntu:17.10
MAINTAINER Alessandro M. Rizzi <alessandromaria.rizzi@polimi.it>

RUN apt-get update && apt-get -y install ocaml
RUN apt-get update && apt-get -y install build-essential
RUN apt-get update && apt-get -y install libbdd-dev

RUN useradd -ms /bin/bash dev
ADD pltl-mup.tar.gz /home/dev/
RUN chown -R dev:dev /home/dev
USER dev
RUN cd /home/dev/pltl-mup && make
USER root
RUN mv /home/dev/pltl-mup/pltlmup /usr/bin
USER dev

WORKDIR /home/dev
CMD /usr/bin/pltlmup

