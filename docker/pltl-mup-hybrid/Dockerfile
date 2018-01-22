FROM alpine
MAINTAINER Alessandro M. Rizzi <alessandromaria.rizzi@polimi.it>

RUN apk add --no-cache  libstdc++ \
    python3
RUN apk add --no-cache --virtual .build-deps \
    shadow \
    gcc \
    g++ \
    make \
    ocaml \
    bash \
    gengetopt \
    boost-dev \
    bison \
    flex

RUN useradd -ms /bin/bash dev
ADD buddy-2.4.tar.gz /home/dev/
ADD pltl-mup.tar.gz /home/dev/
ADD trp++uc-v2.1-20150628.tar.bz2 /home/dev/
ADD run.sh /home/dev/
RUN chown -R dev:dev /home/dev
USER dev
RUN cd /home/dev/buddy-2.4 && ./configure && make
USER root
RUN cd /home/dev/buddy-2.4 && make install
USER dev
RUN cd /home/dev/pltl-mup && make
RUN cd /home/dev/trp++uc-v2.1-20150628 && make
RUN mv /home/dev/trp++uc-v2.1-20150628/bin/trp++uc /home/dev/trp
RUN sed -i s/simple// /home/dev/hybrid.py
USER root
RUN cd /home/dev && rm -rf basic-tests buddy-2.4 test-suite trp++uc-v2.1-20150628
RUN apk del .build-deps
USER dev
WORKDIR /home/dev
CMD /home/dev/run.sh

