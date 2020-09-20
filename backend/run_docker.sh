docker run -d --rm --network host --env-file server.env -v $HACCHOME/deployment:/mnt/cert haccsvc:latest
