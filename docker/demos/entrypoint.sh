#!/bin/bash
# Boots the embedded Postgres, prepares the kanban DB, starts the Phoenix
# releases (kanban on :4001, burrito on :4002, pong on :4003) and nginx
# routing /kanban, /burrito and /pong on $PORT (default 4000). The DB lives inside the
# container (no volume): staging demo data is disposable and the idempotent
# seeds repopulate it on every (re)deploy.
set -e

PG_BIN=/usr/lib/postgresql/16/bin
export PGDATA=/var/lib/postgresql/data
NGINX_PORT="${PORT:-4000}"

mkdir -p "$PGDATA"
chown -R postgres:postgres "$PGDATA"

if [ ! -s "$PGDATA/PG_VERSION" ]; then
  # Plain initdb defaults pg_hba to trust auth; fine here — Postgres listens
  # on loopback only, inside the container.
  su -s /bin/sh postgres -c "$PG_BIN/initdb -D $PGDATA -E UTF8"
fi

su -s /bin/sh postgres -c "$PG_BIN/pg_ctl -D $PGDATA -w start -o '-c listen_addresses=127.0.0.1'"
su -s /bin/sh postgres -c "$PG_BIN/psql -tAc \"SELECT 1 FROM pg_database WHERE datname='kanban'\" | grep -q 1 || $PG_BIN/createdb kanban"

export DATABASE_URL="${DATABASE_URL:-ecto://postgres@127.0.0.1/kanban}"
export HOME=/app

# setpriv (not su) so the apps keep SECRET_KEY_BASE/PHX_HOST from the
# container environment. Each app gets its own PORT and URL_PATH.
run_as_nobody() {
  setpriv --reuid=nobody --regid=nogroup --clear-groups "$@"
}

run_as_nobody env PORT=4001 URL_PATH=/kanban /app/kanban/bin/migrate
run_as_nobody env PORT=4001 URL_PATH=/kanban /app/kanban/bin/seed

run_as_nobody env PORT=4001 URL_PATH=/kanban /app/kanban/bin/server &
run_as_nobody env PORT=4002 URL_PATH=/burrito /app/burrito/bin/burrito start &
run_as_nobody env PORT=4003 URL_PATH=/pong /app/pong/bin/local_lv_pong start &

sed "s/__PORT__/$NGINX_PORT/" /etc/nginx/nginx.conf.template > /etc/nginx/nginx.conf
nginx -g 'daemon off;' &

# Exit (and let the restart policy recover) as soon as any process dies.
wait -n
