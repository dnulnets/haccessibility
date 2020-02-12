#!/bin/bash
export HAPI_TEST_KEY=/home/tomas/haskell/haccessability/deployment/tls.key
export HAPI_TEST_CERTIFICATE=/home/tomas/haskell/haccessability/deployment/tls.pem
export HAPI_TEST_DATABASE=postgresql://heatserver:heatserver@172.17.0.3:5432/test
export HAPI_TEST_JWT_SESSION_LENGTH=3600
export HAPI_TEST_PASSWORD_COST=10
stack test
