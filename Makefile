-include .env
export

react:
	elm reactor

basic:
	cp -f Config.elm src/Config.elm

pro:
	cp -f Config.pro.elm src/Config.elm

watch: basic
	watch -n 5 elm make src/Main.elm --debug --output=sniper.js

watch-pro: pro
	watch -n 5 elm make src/Main.elm --debug --output=sniper.js

build: basic
	elm make src/Main.elm --optimize --output=sniper.js

build-pro: pro
	elm make src/Main.elm --optimize --output=sniper.js

deploy: build
	scp index.html sniper.js $$DST

deploy-pro: build-pro
	scp index.html sniper.js $$DST_PRO
