# The image a version tag pushes to ghcr.io/kronor-io/pgstyle
# (.github/workflows/check.yml): the static binary of the release tarballs
# (scripts/build-static) and nothing else, for linux/amd64 and linux/arm64.
#
# The build context holds each tarball unpacked under its Docker architecture,
# amd64/ and arm64/. pgstyle runs in /work; mount the files there and run it as
# yourself, so the files it rewrites stay yours:
#
#   docker run --rm -v "$PWD:/work" -u "$(id -u):$(id -g)" ghcr.io/kronor-io/pgstyle migrations/*.sql
FROM scratch
ARG TARGETARCH
COPY ${TARGETARCH}/bin/pgstyle /usr/local/bin/pgstyle
WORKDIR /work
ENTRYPOINT ["/usr/local/bin/pgstyle"]
