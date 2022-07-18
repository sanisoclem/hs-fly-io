from haskell:latest as build

WORKDIR /src
COPY ./package.yaml /src
COPY ./stack.yaml /src
COPY ./stack.yaml.lock /src

RUN stack setup
RUN stack build --only-dependencies

COPY . /src

RUN stack build
RUN mkdir dist && cp "$(stack path --local-install-root)/bin/hs-fly-io" ./dist/hs-fly-io

FROM haskell:latest

WORKDIR /app
COPY --from=0 /src/dist /app

EXPOSE 3000

CMD ["./hs-fly-io"]