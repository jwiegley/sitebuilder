HAKYLL = $(shell pwd)/result/bin/johnwiegley

all: build
	echo JohnWiegley.com is built

build:
	nix-build '<nixpkgs>' --fallback -A pkgs.haskell7103Packages.johnwiegley

site:
	$(HAKYLL) rebuild

deploy: site
	@echo Copying files...
	rsync --checksum -av --delete _site/ jw2:/srv/johnwiegley/

	@echo Setting ownership...
	ssh jw2 chown -R nginx:nginx /srv/johnwiegley

	@echo Setting permissions...
	ssh jw2 chmod -R ugo+rX /srv/johnwiegley

	@echo Setting contexts...
	ssh jw2 chcon -R -u system_u -t httpd_sys_content_t /srv/johnwiegley

	@echo Restarting nginx...
	ssh jw2 service nginx restart
