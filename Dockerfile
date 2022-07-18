from haskell:latest as build

WORKDIR /src
COPY . /src

RUN stack setup
RUN stack build --only-dependencies
RUN stack build
RUN mkdir dist && cp "$(stack path --local-install-root)/bin/hs-fly-io" ./dist/hs-fly-io

FROM haskell:latest

WORKDIR /app
COPY --from=build /src/dist /app

EXPOSE 3000

CMD ["./hs-fly-io"]