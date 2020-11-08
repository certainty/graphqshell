FROM haskell:8.8.4
LABEL maintainer="david.krentzlin@gmail.com" 

RUN mkdir /mnt/build
WORKDIR /mnt/build
COPY ./graphqshell /mnt/build
RUN stack install  

COPY ./docker/entrypoint.sh /mnt/build
