#!/bin/bash
export HAPI_KEY=$HACCHOME/deployment/tls.key
export HAPI_CERTIFICATE=$HACCHOME/deployment/tls.pem
export HAPI_DATABASE=postgresql://heatserver:heatserver@172.17.0.2:5432/heat
export HAPI_JWT_SESSION_LENGTH=3600
export HAPI_JWT_SECRET=tomas
export HAPI_PASSWORD_COST=10
until stack exec accessibility-server; do
    echo "Server crashed with exit code $?.  Respawning.." >&2
    sleep 10
done

