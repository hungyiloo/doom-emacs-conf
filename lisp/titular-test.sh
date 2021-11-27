#!/usr/bin/env bash

emacs -batch -L lisp -l lisp/titular-test.el -f ert-run-tests-batch-and-exit
