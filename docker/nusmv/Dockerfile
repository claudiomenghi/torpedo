FROM alpine
MAINTAINER Alessandro M. Rizzi <alessandromaria.rizzi@polimi.it>

RUN useradd -ms /bin/bash dev
ADD NuSMV-2.6.0-linux64.tar.gz /home/dev/
ADD run.sh /home/dev/
RUN chown -R dev:dev /home/dev
USER dev

WORKDIR /home/dev
CMD /home/dev/run.sh



