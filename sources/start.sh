#!/bin/bash

sed -i 's@<openshift-url-placeholder>@'$ROUTE_URL'@g' /usr/local/apache2/htdocs/index.html
httpd-foreground
