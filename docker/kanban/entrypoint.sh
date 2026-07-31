#!/bin/sh
# Boots the embedded Postgres, prepares the kanban DB, then starts the app.
# The DB lives inside the container (no volume): staging demo data is
# disposable and the idempotent seeds repopulate it on every (re)deploy.
set -e

PG_BIN=/usr/lib/postgresql/16/bin
export PGDATA=/var/lib/postgresql/data

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

# setpriv (not su) so the app keeps SECRET_KEY_BASE/PHX_HOST/PORT from the
# container environment.
setpriv --reuid=nobody --regid=nogroup --clear-groups /app/bin/local_lv_kanban eval 'LocalLvKanban.Release.migrate()'
setpriv --reuid=nobody --regid=nogroup --clear-groups /app/bin/local_lv_kanban eval 'LocalLvKanban.Release.seed()'
exec setpriv --reuid=nobody --regid=nogroup --clear-groups /app/bin/local_lv_kanban start
