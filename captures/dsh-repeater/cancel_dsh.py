#!/usr/bin/env python3
"""Cancel a real dsh ACP tool turn and retain wire/session evidence."""

import argparse
import json
import os
from pathlib import Path
import select
import signal
import subprocess
import time


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--patch", required=True, type=Path)
    parser.add_argument("--out", required=True, type=Path)
    args = parser.parse_args()
    out = args.out.resolve()
    workspace = out / "workspace"
    home = out / "home"
    workspace.mkdir(parents=True, exist_ok=False)
    home.mkdir()
    env = os.environ.copy()
    env["DSH_HOME"] = str(home)
    process = subprocess.Popen(
        ["dsh", "acp", "--patch", str(args.patch.resolve())],
        cwd=workspace,
        env=env,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=1,
        start_new_session=True,
    )
    wire = out / "wire.jsonl"
    wire_handle = wire.open("w")
    received = []

    def send(value):
        wire_handle.write(json.dumps({"dir": "send", "value": value}) + "\n")
        wire_handle.flush()
        process.stdin.write(json.dumps(value, separators=(",", ":")) + "\n")
        process.stdin.flush()

    def read_one(timeout=120):
        ready, _, _ = select.select([process.stdout], [], [], timeout)
        if not ready:
            raise TimeoutError("dsh ACP read")
        line = process.stdout.readline()
        if not line:
            raise RuntimeError(f"dsh ACP exited {process.poll()}")
        value = json.loads(line)
        received.append(value)
        wire_handle.write(json.dumps({"dir": "recv", "value": value}) + "\n")
        wire_handle.flush()
        return value

    def response(request_id):
        while True:
            value = read_one()
            if value.get("id") == request_id and "method" not in value:
                return value

    send({"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": 1, "clientCapabilities": {"fs": {"readTextFile": True, "writeTextFile": True}, "terminal": False}}})
    initialized = response(1)
    if "error" in initialized:
        raise RuntimeError(initialized)
    send({"jsonrpc": "2.0", "id": 2, "method": "session/new", "params": {"cwd": str(workspace), "mcpServers": []}})
    created = response(2)
    sid = created["result"]["sessionId"]
    prompt = "Run this exact shell command: touch cancel-started; sleep 30. Do not reply before the command finishes."
    send({"jsonrpc": "2.0", "id": 3, "method": "session/prompt", "params": {"sessionId": sid, "prompt": [{"type": "text", "text": prompt}]}})
    deadline = time.monotonic() + 180
    while not (workspace / "cancel-started").exists():
        if time.monotonic() > deadline:
            raise TimeoutError("real dsh tool did not start")
        read_one(max(0.1, deadline - time.monotonic()))
    send({"jsonrpc": "2.0", "method": "session/cancel", "params": {"sessionId": sid}})
    settled = response(3)
    stop = settled.get("result", {}).get("stopReason")
    process.stdin.close()
    try:
        process.wait(timeout=15)
    except subprocess.TimeoutExpired:
        os.killpg(process.pid, signal.SIGTERM)
        process.wait(timeout=5)
    wire_handle.close()
    stderr = process.stderr.read()
    (out / "stderr.txt").write_text(stderr)
    logs = list(home.glob(f"sessions/*/{sid}/session.v4.jsonl.zstd"))
    if len(logs) != 1:
        raise RuntimeError(f"expected one session log, got {logs}")
    decoded = subprocess.run(["zstd", "-dc", str(logs[0])], check=True, text=True, stdout=subprocess.PIPE).stdout
    (out / "session.jsonl").write_text(decoded)
    events = [json.loads(line) for line in decoded.splitlines() if line]
    result = {
        "session_id": sid,
        "stop_reason": stop,
        "process_exit": process.returncode,
        "tool_started": (workspace / "cancel-started").exists(),
        "turn_ends": [event["data"] for event in events if event.get("type") == "turn/end"],
        "wire": str(wire),
        "session_log": str(out / "session.jsonl"),
        "frames": len(received),
    }
    (out / "analysis.json").write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(result, indent=2))
    if stop != "cancelled":
        raise SystemExit(1)


if __name__ == "__main__":
    main()
