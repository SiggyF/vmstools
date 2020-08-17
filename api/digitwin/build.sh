#!/bin/bash
docker build -t shrimp .
docker tag shrimp openearth/shrimp:latest
gcloud builds submit --tag gcr.io/hydro-engine/shrimp --timeout=30m
