FROM haskell:8 as buildenv

RUN cabal new-update
WORKDIR /app
COPY *.cabal ./
RUN cabal build --dependencies-only all
CMD [ "bash" ]

FROM buildenv as build
COPY . .
RUN cabal new-install


FROM ubuntu as deploy
WORKDIR /app
COPY --from=build /root/.cabal/bin/* /app/
COPY ./static ./static

CMD [ "./latticeVis" ]