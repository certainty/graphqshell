FROM haskell:8.8.4
LABEL maintainer="david.krentzlin@gmail.com" 

RUN mkdir /mnt/build
WORKDIR /mnt/build
COPY ./graphqshell /mnt/build
RUN stack install --flag graphqshell:demo-server

COPY ./docker/entrypoint.sh /mnt/build
RUN mkdir ~/.graphqshell
RUN cp /mnt/build/config/* ~/.graphqshell/
