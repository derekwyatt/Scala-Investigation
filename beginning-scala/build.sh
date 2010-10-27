#!/bin/sh

mkdir -p out
scalac -d out -deprecation $*
