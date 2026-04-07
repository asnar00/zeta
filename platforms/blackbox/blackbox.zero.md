# blackbox
*flight recorder for fault diagnosis*

## specification

A flight recorder that continuously captures app state and actions into a circular buffer. When a user reports a fault, the buffer is frozen, annotated with a comment, and uploaded to the server for replay-based diagnosis.

Each participant (client or server) runs its own blackbox. The buffer holds a rolling 60-second window of activity, divided into 10-second *moments*. Each moment opens with a full state snapshot (keyframe), followed by a sequence of timed actions, giving the structure:

    state -> actions -> state -> actions -> ... -> state -> actions

On fault report, the local buffer is frozen and queued for upload. If the device is offline, the frozen buffer is persisted locally and uploaded when connectivity returns. For multi-device scenarios (chat, ride-sharing), the server collects buffers from all relevant participants and bundles them into a single fault package.

Every action that crosses a component boundary (RPC, WebSocket, event handler) is tagged with a correlation ID, so actions on different devices can be stitched together into a causal timeline after the fact.

The blackbox also captures a build fingerprint at session start: the feature tree, composed source hash, platform versions, and context defaults. This allows the server to reconstruct the exact code that was running, enabling deterministic replay.

For replay fidelity, the return values of non-deterministic operations (random, time, external API responses) are recorded as part of each action entry. During replay, these recorded values are substituted in place of live execution, ensuring identical behaviour.

### platform surface

The blackbox needs only three thin OS primitives:

1. **monotonic clock** — milliseconds since recording started
2. **periodic tick** — call a function every N milliseconds
3. **local key-value store** — persist and retrieve strings by key (survives restart)

Everything else (buffer management, moment rotation, freezing, correlation, upload) is pure logic built on these three primitives plus the existing `remote` and `runtime` platforms.

## interface

Milliseconds since recording started:

    on input (number ms) = elapsed time ()

Call a function repeatedly at an interval. Returns a timer ID:

    on (string timer) = every (number ms) do (string callback)

Cancel a repeating timer:

    on cancel timer (string timer)

Persist a string locally by key (survives app restart):

    on store locally (string key, string value)

Retrieve a persisted string (empty string if not found):

    on (string value) = retrieve locally (string key)

List all keys matching a prefix:

    on (string result) = stored keys (string prefix)

Remove a persisted key:

    on remove locally (string key)

Freeze the buffer and attach a user comment. Returns a fault ID. On the client, persists locally for offline upload. On the server, receives and stores client fault reports:

    on (string fault) = report fault (string comment)

Retrieve a stored fault report by ID:

    on (string result) = get fault (string fault-id)

Upload any pending fault reports that were saved while offline:

    on upload pending faults ()

Get the build fingerprint (hash, git commit, feature list) as JSON:

    on (string fp) = build fingerprint ()

Freeze the current buffer for multi-device collection. Called by the server on other clients when a fault is reported. Returns the frozen moments as JSON:

    on (string buffer) = freeze buffer (string fault-id)

## tests

Elapsed time returns a non-negative number:

    elapsed time () => 0

Report fault returns an 8-character ID:

    length of (report fault ("test")) => 8
