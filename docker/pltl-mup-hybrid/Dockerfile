FROM ubuntu:17.10
MAINTAINER Alessandro M. Rizzi <alessandromaria.rizzi@polimi.it>

RUN apt-get update && apt-get -y install ocaml
RUN apt-get update && apt-get -y install build-essential
RUN apt-get update && apt-get -y install libbdd-dev
RUN apt-get update && apt-get -y install python3
RUN apt-get update && apt-get -y install gengetopt
RUN apt-get update && apt-get -y install libboost-all-dev
RUN apt-get update && apt-get -y install flex bison
RUN apt-get update && apt-get -y install time

RUN useradd -ms /bin/bash dev
ADD pltl-mup.tar.gz /home/dev/
ADD trp++uc-v2.1-20150628.tar.bz2 /home/dev/
ADD run.sh /home/dev/
RUN chown -R dev:dev /home/dev
USER dev
RUN cd /home/dev/pltl-mup && make
RUN cd /home/dev/trp++uc-v2.1-20150628 && make
RUN ln -s /home/dev/trp++uc-v2.1-20150628/bin/trp++uc /home/dev/trp
RUN sed -i s/simple// /home/dev/hybrid.py
USER root
RUN cp /home/dev/pltl-mup/pltlmup /usr/bin
USER dev

WORKDIR /home/dev
CMD /home/dev/run.sh

