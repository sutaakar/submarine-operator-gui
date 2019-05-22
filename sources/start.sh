#!/bin/bash

sed -i 's@<openshift-url-placeholder>@'$ROUTE_URL'@g' /usr/local/apache2/htdocs/index.html

TOKEN=$(cat /var/run/secrets/kubernetes.io/serviceaccount/token)
sed -i 's@<authentication-token-placeholder>@'$TOKEN'@g' /usr/local/apache2/htdocs/index.html

sed -i 's@Listen 80@Listen 8080@g' /usr/local/apache2/conf/httpd.conf

httpd-foreground
