#!/bin/bash
set -ev
sudo curl https://packages.microsoft.com/config/ubuntu/16.04/prod.list > /etc/apt/sources.list.d/mssql-release.list
sudo apt-get update
sudo ACCEPT_EULA=Y apt-get install msodbcsql17
sudo apt-get install unixodbc-dev
