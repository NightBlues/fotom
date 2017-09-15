#!/bin/bash

echo "CREATE TABLE IF NOT EXISTS photo (id INTEGER PRIMARY KEY, name TEXT, date TEXT)" | sqlite3 db.sqlite 
