#!/bin/bash

coverage erase
coverage run --branch -a --source . test/test_core.py
coverage run --branch -a --source . test/test_server.py

coverage html
