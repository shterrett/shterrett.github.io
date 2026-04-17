nginx_conf := justfile_directory() / "nginx.conf"

# Build the site generator binary
build:
    stack build

# Build the static site, then reload nginx if it's already running
site:
    stack run site -- build
    just _reload-if-running

# Start nginx (creates logs/ if needed)
serve:
    mkdir -p logs
    nginx -c {{nginx_conf}}

# Reload nginx (fails clearly if nginx is not running)
reload:
    nginx -c {{nginx_conf}} -s reload

# Stop nginx
stop:
    nginx -c {{nginx_conf}} -s stop

# Internal: reload nginx only if the pid file exists and the process is alive
_reload-if-running:
    #!/usr/bin/env sh
    if [ -f logs/nginx.pid ] && kill -0 "$(cat logs/nginx.pid)" 2>/dev/null; then
        nginx -c {{nginx_conf}} -s reload
    fi
