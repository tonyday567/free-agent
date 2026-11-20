#!/usr/bin/env python3
"""Measure dsh card and tool-schema prompt overhead with four fresh ACP turns."""

import argparse
import json
import os
from pathlib import Path
import subprocess


def post(binary, root, body):
    subprocess.run(
        [binary, "bus", "post", "--root", str(root), "--from", "probe", "--to", "seat", "--body", body],
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--binary", required=True)
    parser.add_argument("--out", required=True, type=Path)
    parser.add_argument("--patch", required=True, type=Path)
    parser.add_argument("--no-tools-patch", required=True, type=Path)
    parser.add_argument("--card", required=True, type=Path)
    parser.add_argument("--empty", required=True, type=Path)
    args = parser.parse_args()
    binary = str(Path(args.binary).resolve())
    base = args.out.resolve()
    base.mkdir(parents=True, exist_ok=False)
    workspace = base / "workspace"
    workspace.mkdir()
    results = []

    for label, tools, card in [
        ("full-empty", True, False),
        ("full-card", True, True),
        ("no-tools-empty", False, False),
        ("no-tools-card", False, True),
    ]:
        arm = base / label
        root = arm / "root"
        home = arm / "home"
        root.mkdir(parents=True)
        home.mkdir()
        (root / "log.jsonl").touch()
        post(binary, root, "Reply with exactly OVERHEAD_OK and nothing else.")
        post(binary, root, "🟢")
        subprocess.run(
            [binary, "bus", "cursor", "set", "--root", str(root), "seat", "0"],
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
        command = [
            binary,
            "dsh-acp",
            "--root",
            str(root),
            "--name",
            "seat",
            "--prompt",
            str((args.card if card else args.empty).resolve()),
            "--dsh-patch",
            str(args.patch.resolve()),
            "--dsh-home",
            str(home),
            "--workspace",
            str(workspace),
        ]
        if not tools:
            command += ["--dsh-model-patch", str(args.no_tools_patch.resolve())]
        completed = subprocess.run(
            command,
            cwd=workspace,
            env=os.environ.copy(),
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            timeout=120,
        )
        (arm / "run.out").write_text(completed.stdout)
        (arm / "run.err").write_text(completed.stderr)
        if completed.returncode != 0:
            raise RuntimeError(f"{label} exited {completed.returncode}: {completed.stderr}")
        sid = (root / ".sessions" / "seat.sid").read_text().strip()
        logs = list(home.glob(f"sessions/*/{sid}/session.v4.jsonl.zstd"))
        if len(logs) != 1:
            raise RuntimeError(f"{label} has {len(logs)} session logs")
        decoded = subprocess.run(
            ["zstd", "-dc", str(logs[0])], check=True, stdout=subprocess.PIPE, text=True
        ).stdout
        session = arm / "session.jsonl"
        session.write_text(decoded)
        events = [json.loads(line) for line in decoded.splitlines() if line]
        usages = [
            event["data"]["usage"]
            for event in events
            if event.get("type") == "assistant/message"
        ]
        if len(usages) != 1:
            raise RuntimeError(f"{label} has {len(usages)} model calls")
        posts = [json.loads(line) for line in (root / "log.jsonl").read_text().splitlines()]
        replies = [post for post in posts if post["from"] == "seat"]
        if len(replies) != 1 or replies[0]["body"] != "OVERHEAD_OK":
            raise RuntimeError(f"{label} reply was not accepted: {replies}")
        headers = [event for event in events if event.get("type") == "request/header"]
        tool_values = headers[0]["data"]["header"].get("tools", [])
        usage = usages[0]
        fresh = usage["inputTokens"]
        cached = usage.get("cacheReadTokens", 0)
        output = usage["outputTokens"]
        row = {
            "label": label,
            "F": fresh,
            "C": cached,
            "O": output,
            "input": fresh + cached,
            "mass": usage["totalTokens"],
            "tools": len(tool_values),
            "tool_schema_json_bytes": len(json.dumps(tool_values, separators=(",", ":")).encode()),
            "card_bytes": (args.card if card else args.empty).stat().st_size,
            "session": str(session),
        }
        results.append(row)
        print(json.dumps(row), flush=True)

    by_label = {row["label"]: row for row in results}
    summary = {
        "arms": results,
        "card_input_tokens_full": by_label["full-card"]["input"] - by_label["full-empty"]["input"],
        "card_input_tokens_no_tools": by_label["no-tools-card"]["input"] - by_label["no-tools-empty"]["input"],
        "tool_schema_input_tokens_empty_card": by_label["full-empty"]["input"] - by_label["no-tools-empty"]["input"],
        "tool_schema_input_tokens_with_card": by_label["full-card"]["input"] - by_label["no-tools-card"]["input"],
    }
    (base / "analysis.json").write_text(json.dumps(summary, indent=2) + "\n")
    print(json.dumps(summary, indent=2))


if __name__ == "__main__":
    main()
