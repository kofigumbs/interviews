# Only using PHONY targets, so this is just a glorified script runner
# with the benefit of being ubiquitously installed.
.PHONY: dev server client

# START DEV SERVERS
#
#   First rule in Makefiles are also the default.
dev:
	@# In Ruby-land it's generally faster to use a check command before attempting
	@# to install. Bundler is generally installed in production environments
	@# (i.e. Netlify and Heroku), and rerun is a dev convenience.
	gem which bundler rerun || gem install bundler rerun
	bundle check || bundle install

	@# Running make explicitly here is purely ergonomic. Developers can start
	@# the app with 5 keystrokes, and we take care inside the file to start each
	@# server in its own child process (or "job" in make lingo).
	make -j2 client server

15SECONDLY_API_PORT = 3001
client:
	env 15SECONDLY_API="http://localhost:${15SECONDLY_API_PORT}" \
		bundle exec jekyll serve --source client --open-url
server:
	env DATABASE_URL="postgres://localhost/15secondly" \
		rerun --dir server -- \
		bundle exec rackup server/config.ru --port ${15SECONDLY_API_PORT}
