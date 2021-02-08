docker build --target buildenv -t web-analysis:buildenv .
docker run -p 8000:8000 --rm -it -v $(pwd)/:/app web-analysis:buildenv
