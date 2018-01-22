FROM alpine
MAINTAINER Alessandro M. Rizzi <alessandromaria.rizzi@polimi.it>

RUN apk add --no-cache  libstdc++
RUN apk add --no-cache --virtual .build-deps \
    shadow \
    gcc \
    g++ \
    make \
    ocaml

RUN useradd -ms /bin/bash dev
ADD buddy-2.4.tar.gz /home/dev/
ADD pltl-mup.tar.gz /home/dev/
RUN chown -R dev:dev /home/dev
USER dev
RUN cd /home/dev/buddy-2.4 && ./configure && make
USER root
RUN cd /home/dev/buddy-2.4 && make install
USER dev
RUN cd /home/dev/pltl-mup && make
USER root
RUN mv /home/dev/pltl-mup/pltlmup /usr/bin
RUN rm -rf /home/dev/* 
RUN apk del .build-deps
USER dev
WORKDIR /home/dev
CMD /usr/bin/pltlmup

