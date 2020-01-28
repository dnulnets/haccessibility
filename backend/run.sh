#!/bin/bash
export HAPI_KEY=/home/tomas/haskell/haccessability/deployment/tls.key
export HAPI_CERTIFICATE=/home/tomas/haskell/haccessability/deployment/tls.pem
export HAPI_DATABASE=postgresql://heatserver:heatserver@172.17.0.2:5432/heat
export HAPI_JWT_SESSION_LENGTH=3600
export HAPI_PASSWORD_COST=10
stack exec accessibility-server

