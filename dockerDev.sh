docker build --target buildenv -t latticevis:buildenv .
docker run -p 8000:8000 --rm -it -v $(pwd)/:/app latticevis:buildenv
