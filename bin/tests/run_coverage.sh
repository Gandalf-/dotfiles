#!/bin/bash

coverage erase
coverage run -a --source ../../lib/python test_apocrypha.py
coverage run -a --source ../../lib/python test_apocrypha_server.py

coverage html
