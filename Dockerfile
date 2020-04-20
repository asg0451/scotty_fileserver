FROM fpco/stack-build:lts-5.5 as build

# all that's needed to build deps
RUN mkdir -p /app
COPY *.cabal /app
COPY stack.yaml /app
COPY Setup.hs /app

WORKDIR /app

## for private repo stuff
ARG SSH_PRIVATE_KEY
ARG SSH_KHS
RUN mkdir -p /root/.ssh
RUN echo "${SSH_PRIVATE_KEY}" >> /root/.ssh/id_rsa
RUN chmod 600 /root/.ssh/id_rsa
RUN echo "Host github.com\n\tStrictHostKeyChecking no\n" >> ~/.ssh/config

RUN curl -L https://github.com/sass/dart-sass/releases/download/1.26.3/dart-sass-1.26.3-linux-x64.tar.gz > /tmp/sass.tar.gz
RUN tar xzvf /tmp/sass.tar.gz -C /tmp/

RUN stack build --install-ghc --dependencies-only

# now actually build the app. hopefully all ^ will be cached (unless deps change)

COPY . /app

RUN stack build

FROM ubuntu as run

RUN mkdir -p /root/sass
COPY --from=build /tmp/dart-sass /root/sass/
RUN cp /root/sass/sass /root/sass/scss

ENV PATH /root/sass:$PATH

COPY --from=build /app /app

WORKDIR /app
EXPOSE 4242
CMD ["/app/run_one.sh"]
