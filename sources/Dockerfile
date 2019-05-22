FROM httpd
RUN chgrp -Rf root /usr/local/apache2 && chmod -Rf g+w /usr/local/apache2
ADD index.html /usr/local/apache2/htdocs/index.html
ADD main.js /usr/local/apache2/htdocs/main.js
ADD start.sh /usr/share/start.sh
CMD ["/usr/share/start.sh"]

