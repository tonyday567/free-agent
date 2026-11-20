#!/usr/bin/env python3
"""Real dsh ACP crash-tail recovery with separate bus ambiguity settlement."""

import argparse
import json
import os
from pathlib import Path
import signal
import subprocess
import time


def call(command, **kwargs):
    return subprocess.run(command, check=True, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, **kwargs)


def wait_for(description, predicate, timeout=120):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if predicate():
            return
        time.sleep(0.1)
    raise TimeoutError(description)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--binary", required=True)
    parser.add_argument("--patch", required=True)
    parser.add_argument("--out", required=True, type=Path)
    args = parser.parse_args()
    binary = str(Path(args.binary).resolve())
    patch = str(Path(args.patch).resolve())
    out = args.out.resolve()
    root = out / "root"
    workspace = out / "workspace"
    home = out / "home"
    for directory in [root, workspace, home]:
        directory.mkdir(parents=True, exist_ok=False)
    (root / "log.jsonl").touch()
    prompt = out / "prompt.md"
    prompt.write_text("Run the exact shell command the post supplies, then reply with only its stdout.\n")
    command = [
        binary,
        "dsh-acp",
        "--root",
        str(root),
        "--name",
        "seat",
        "--prompt",
        str(prompt),
        "--dsh-patch",
        patch,
        "--dsh-home",
        str(home),
        "--workspace",
        str(workspace),
    ]
    env = os.environ.copy()
    result = {"root": str(root), "home": str(home), "workspace": str(workspace), "phases": []}

    def start(label):
        stdout_path = out / f"{label}.out"
        stderr_path = out / f"{label}.err"
        stdout = stdout_path.open("w")
        stderr = stderr_path.open("w")
        process = subprocess.Popen(
            command,
            cwd=workspace,
            env=env,
            stdout=stdout,
            stderr=stderr,
            text=True,
            start_new_session=True,
        )
        wait_for(f"{label} cursor", lambda: "cursor:" in stdout_path.read_text(), 30)
        phase = {"label": label, "pid": process.pid}
        result["phases"].append(phase)
        return process, stdout, stderr, phase

    task = (
        "Run this exact shell command: "
        "if [ -f crash-once ]; then printf 'RECOVERED_OK\\n'; "
        "else touch crash-once; touch crash-started; sleep 30; fi. "
        "Reply with only its stdout."
    )
    posted = json.loads(call([binary, "bus", "post", "--root", str(root), "--from", "pilot", "--to", "seat", "--body", task]).stdout)
    result["task_post"] = posted["id"]
    call([binary, "bus", "cursor", "set", "--root", str(root), "seat", "0"])
    first, first_out, first_err, first_phase = start("crash")
    wait_for("crash tool effect", lambda: (workspace / "crash-started").exists(), 180)
    os.killpg(first.pid, signal.SIGKILL)
    first.wait(timeout=10)
    first_out.close()
    first_err.close()
    first_phase["exit"] = first.returncode
    first_phase["active_after_kill"] = (root / ".active-seat").exists()
    first_phase["transcript_after_kill"] = (root / ".transcripts" / "seat.jsonl").exists()

    second, second_out, second_err, second_phase = start("ambiguous-restart")
    second.wait(timeout=60)
    second_out.close()
    second_err.close()
    second_phase["exit"] = second.returncode
    posts_after_ambiguity = [json.loads(line) for line in (root / "log.jsonl").read_text().splitlines()]
    second_phase["ambiguous_notices"] = sum(
        post["from"] == "seat" and "ambiguous recovery" in post["body"] for post in posts_after_ambiguity
    )
    second_phase["replies"] = sum(post["from"] == "seat" and "pilot" in post["to"] for post in posts_after_ambiguity)

    # Explicit reconciliation of this scratch batch only. The bus marker is not
    # a dsh session marker; removing one must not remove or rewrite the other.
    (root / ".active-seat").unlink()
    result["explicit_reconciliation"] = "removed only the scratch bus active-batch marker"
    third, third_out, third_err, third_phase = start("reconciled-resume")

    def settled():
        transcript = root / ".transcripts" / "seat.jsonl"
        rows = transcript.read_text().splitlines() if transcript.exists() else []
        if not rows:
            return False
        if json.loads(rows[-1])["outcome"] != "settled":
            raise RuntimeError(f"recovery outcome was {rows[-1]}")
        cursor = int(call([binary, "bus", "cursor", "get", "--root", str(root), "seat"]).stdout)
        return cursor > posted["id"]

    wait_for("reconciled reply", settled, 180)
    halted = json.loads(call([binary, "bus", "post", "--root", str(root), "--from", "pilot", "--to", "seat", "--body", "🟢"]).stdout)
    third_phase["halt_post"] = halted["id"]
    third.wait(timeout=60)
    third_out.close()
    third_err.close()
    third_phase["exit"] = third.returncode

    posts = [json.loads(line) for line in (root / "log.jsonl").read_text().splitlines()]
    replies = [post for post in posts if post["from"] == "seat" and "pilot" in post["to"]]
    result["replies"] = replies
    result["accepted"] = len(replies) == 1 and replies[0]["body"] == "RECOVERED_OK" and replies[0]["thread"] == [posted["id"]]
    result["duplicate_posts"] = max(0, len(replies) - 1)
    result["cursor"] = int(call([binary, "bus", "cursor", "get", "--root", str(root), "seat"]).stdout)
    sid = (root / ".sessions" / "seat.sid").read_text().strip()
    result["session_id"] = sid
    logs = list(home.glob(f"sessions/*/{sid}/session.v4.jsonl.zstd"))
    if len(logs) != 1:
        raise RuntimeError(f"expected one dsh session log, got {logs}")
    decoded = call(["zstd", "-dc", str(logs[0])]).stdout
    session = out / "session.jsonl"
    session.write_text(decoded)
    events = [json.loads(line) for line in decoded.splitlines() if line]
    result["turn_ends"] = [event["data"] for event in events if event.get("type") == "turn/end"]
    result["session_events"] = len(events)
    result["session_log"] = str(session)
    (out / "analysis.json").write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(result, indent=2))
    if not result["accepted"] or result["duplicate_posts"] != 0:
        raise SystemExit(1)


if __name__ == "__main__":
    main()
