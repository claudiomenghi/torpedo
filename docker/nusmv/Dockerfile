FROM busybox
MAINTAINER Alessandro M. Rizzi <alessandromaria.rizzi@polimi.it>

RUN adduser -D -u 1000 dev
ADD NuSMV-2.6.0-linux64.tar.gz /home/dev/
ADD run.sh /home/dev/
RUN chown -R dev:dev /home/dev
USER dev
RUN cd /home/dev/NuSMV-2.6.0-Linux && rm -rf include lib share

WORKDIR /home/dev
CMD /home/dev/run.sh



