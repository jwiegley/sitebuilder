HAKYLL = $(shell pwd)/result/bin/johnwiegley

all: build
	echo JohnWiegley.com is built

build:
	nix-build '<nixpkgs>' -A haskell801Packages.johnwiegley

site:
	$(HAKYLL) rebuild

watch:
	$(HAKYLL) watch

deploy: site
	@echo Copying files...
	rsync --checksum -av --delete _site/ jw:/srv/johnwiegley/

	@echo Setting ownership...
	ssh jw chown -R nginx:nginx /srv/johnwiegley

	@echo Setting permissions...
	ssh jw chmod -R ugo+rX /srv/johnwiegley

	@echo Setting contexts...
	ssh jw chcon -R -u system_u -t httpd_sys_content_t /srv/johnwiegley

	@echo Restarting nginx...
	ssh jw service nginx restart
