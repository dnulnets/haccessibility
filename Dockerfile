FROM neptune:32000/paccbuild:14 as portal
WORKDIR /
RUN mkdir -p /haccessibility
COPY ./portal /haccessibility/portal
WORKDIR /haccessibility/portal
RUN	rm -fr .cahce \
	&& rm -fr node_modules \
	&& rm -fr output \
	&& rm -fr .spago
RUN	npm install \
	&& ./generate.sh \
	&& cat ./src/Version.purs \
	&& spago --no-color build\
	&& spago bundle-app \
	&& parcel build --public-url /iothub/ index.html
	
FROM neptune:32000/haccbuild:16.31 as build
WORKDIR /
RUN mkdir -p /opt/build/backend
COPY ./backend /opt/build/backend
WORKDIR /opt/build/backend
RUN 	   rm -fR static \
	&& mkdir static
COPY --from=portal /haccessibility/portal/dist/* ./static/
RUN        stack clean \
	&& stack build --system-ghc \
	&& stack install

FROM ubuntu:focal
RUN mkdir -p /opt/accessibility
WORKDIR /opt/accessibility
RUN        apt-get update \
	&& apt-get install -y libpq-dev \
	&& mkdir static
COPY --from=build /root/.local/bin .
COPY --from=build /opt/build/backend/static/* ./static/
COPY --from=build /opt/build/backend/hadmin/*.json ./
CMD ["/opt/accessibility/accessibility-server"]

