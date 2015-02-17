#!/usr/bin/sh

dropdb --if-exists eval
createdb eval
psql eval -f `pwd`/eval_schema.sql
