#!/bin/bash
export HAPI_TEST_DATABASE=postgresql://heatserver:heatserver@172.17.0.3:5432/test
export HAPI_TEST_JWT_SESSION_LENGTH=3600
export HAPI_TEST_PASSWORD_COST=10
stack test

