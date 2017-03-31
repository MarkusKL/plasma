#!/bin/bash

stack build &&
stack install &&
~/.local/bin/plasma
