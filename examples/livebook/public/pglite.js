// Postgres in the page: PGlite, Postgres compiled to WebAssembly, serves the
// notebooks' connections to the host "pglite", which their VMs open with
// gen_tcp's {tcp_module, popcorn_tcp} option: see Popcorn's `tcp` option.
//
// The tab has one database, in memory, which starts with the first
// connection. PGlite has one session, which the connections share: a
// connection has it from its first message until the session is idle again,
// out of a transaction, as Postgres reports. Their other session state is
// shared too, like named prepared statements, so connections should use
// unnamed ones, with Postgrex's `prepare: :unnamed`.

const PGLITE = "https://cdn.jsdelivr.net/npm/@electric-sql/pglite@0.5.16/dist/index.js";

const SSL_REQUEST = 80877103;
const GSSENC_REQUEST = 80877104;
const CANCEL_REQUEST = 80877102;
const TERMINATE = 0x58; // "X"
const SYNC = 0x53; // "S"
// ReadyForQuery, "Z", with status "I": out of a transaction
const READY_AND_IDLE = new Uint8Array([0x5a, 0, 0, 0, 5, 0x49]);
// Logs each message's wait and run times, when set in the console
const debug = () => globalThis.PGLITE_DEBUG === true;

let database = null;
// The messages waiting for the session, in order, and the connection that has
// the session, if any
const queue = [];
let owner = null;
let running = false;
let connections = 0;

// For debugging, in the console: who has the session, and who waits
globalThis.pgliteState = () => ({
  owner: owner?.id ?? null,
  queue: queue.map(({ connection, message }) => `${connection.id}:${String.fromCharCode(message[0])}`),
});

export async function connectPglite({ host }, peer) {
  if (host !== "pglite") throw new Error(`no server at ${host}, only at "pglite"`);
  database ??= import(PGLITE).then(({ PGlite }) => PGlite.create());
  const db = await database;

  const connection = { id: ++connections, peer, buffer: new Uint8Array(0), started: false, closed: false };
  return {
    send(data) {
      if (connection.closed) return;
      connection.buffer = concat(connection.buffer, data);
      for (const message of messages(connection)) {
        // A Sync alone, from a connection that waits for nothing, is a ping,
        // as DBConnection's of idle connections: it's answered at once, rather
        // than after another connection's transaction
        const waiting = owner === connection || queue.some((item) => item.connection === connection);
        if (!waiting && isSync(message)) connection.peer.deliver(READY_AND_IDLE);
        else queue.push({ connection, message, queued: performance.now() });
      }
      run(db);
    },
    close() {
      connection.closed = true;
      for (let i = queue.length - 1; i >= 0; i--) {
        if (queue[i].connection === connection) queue.splice(i, 1);
      }
      if (owner === connection) release(db);
    },
  };
}

// The whole messages of the connection's data. The first one has no type: the
// startup message, or a request that comes before it.
function* messages(connection) {
  while (true) {
    const { buffer } = connection;
    const view = new DataView(buffer.buffer, buffer.byteOffset, buffer.byteLength);
    let length;
    if (!connection.started) {
      if (buffer.length < 8) return;
      length = view.getInt32(0);
      const code = view.getInt32(4);
      if (buffer.length < length) return;
      connection.buffer = buffer.subarray(length);
      // No encryption: the connection doesn't leave the page
      if (code === SSL_REQUEST || code === GSSENC_REQUEST) {
        connection.peer.deliver(new TextEncoder().encode("N"));
        continue;
      }
      // PGlite has no backend to cancel
      if (code === CANCEL_REQUEST) continue;
      connection.started = true;
    } else {
      if (buffer.length < 5) return;
      length = 1 + view.getInt32(1);
      if (buffer.length < length) return;
      connection.buffer = buffer.subarray(length);
    }
    yield buffer.slice(0, length);
  }
}

// Runs the queued messages: the session owner's, while there is one
async function run(db) {
  if (running) return;
  running = true;
  try {
    while (true) {
      const index = owner === null ? 0 : queue.findIndex((item) => item.connection === owner);
      if (index < 0 || index >= queue.length) return;
      const [{ connection, message, queued }] = queue.splice(index, 1);
      owner = connection;
      const started = performance.now();

      // Terminate would end PGlite's only session: the connection just leaves
      if (message[0] === TERMINATE) {
        await release(db);
        continue;
      }

      let tail = new Uint8Array(0);
      try {
        await db.runExclusive(() =>
          db.execProtocolRawStream(message, {
            onRawData: (data) => {
              tail = concat(tail, data).slice(-6);
              if (!connection.closed) connection.peer.deliver(data);
            },
          }),
        );
      } catch (error) {
        console.warn("[pglite] The connection failed:", error);
        connection.peer.close();
        connection.closed = true;
        await release(db);
        continue;
      }
      if (debug()) {
        const type = connection.started ? String.fromCharCode(message[0]) : "startup";
        const waited = Math.round(started - queued);
        const ran = Math.round(performance.now() - started);
        console.debug(`[pglite] ${type} waited ${waited} ms, ran ${ran} ms`);
      }
      // The session is free once Postgres is ready for a query out of a
      // transaction
      if (isReadyAndIdle(tail)) owner = null;
    }
  } finally {
    running = false;
  }
}

// Frees the session of a connection that leaves, rolling back what it left
async function release(db) {
  owner = null;
  if (db.isInTransaction()) await db.exec("ROLLBACK");
  run(db);
}

function isReadyAndIdle(tail) {
  return tail.length === 6 && tail.every((byte, i) => byte === READY_AND_IDLE[i]);
}

function isSync(message) {
  return message.length === 5 && message[0] === SYNC;
}

function concat(a, b) {
  if (a.length === 0) return b;
  const result = new Uint8Array(a.length + b.length);
  result.set(a);
  result.set(b, a.length);
  return result;
}
