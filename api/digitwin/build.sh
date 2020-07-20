#!/bin/bash
docker build -t shrimp .
docker tag shrimp openearth/shrimp:latest
