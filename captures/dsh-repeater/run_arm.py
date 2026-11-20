#!/usr/bin/env python3
"""Run one bounded, four-post repeater arm with a mid-run process restart."""

import argparse
import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import time

TASKS = [
    "Read notes.txt and reply with only its last line.",
    "Run ./report.sh with the shell and reply with only the REPORT-TOTAL value.",
    "Append checkpoint-seven to scratch.txt and reply with only the resulting line count.",
    "Recall all three results in one line exactly as NOTES=<last-line> REPORT=<report-total> SCRATCH=<line-count>.",
]

PROMPT = """You are a bounded scratch repeater.
Work only in the current scratch workspace. Use tools when the post asks for them.
Reply with only the requested result. Do not use goals, todos, workflows, or subagents.
Do not modify any file except scratch.txt when explicitly asked.
"""


def wait_until(description, predicate, timeout, process=None):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if predicate():
            return
        if process is not None and process.poll() is not None:
            raise RuntimeError(f"seat exited {process.returncode} while waiting for {description}")
        time.sleep(0.1)
    raise TimeoutError(f"timed out waiting for {description}")


def stop_process(process, out, timeout=45):
    if process.poll() is None:
        try:
            process.wait(timeout=timeout)
        except subprocess.TimeoutExpired:
            print("SHUTDOWN_TIMEOUT", flush=True)
            os.killpg(process.pid, signal.SIGTERM)
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                os.killpg(process.pid, signal.SIGKILL)
                process.wait(timeout=5)
    out.flush()
    return process.returncode


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--binary", required=True)
    parser.add_argument("--out", required=True)
    parser.add_argument("--arm", choices=["hermes", "dsh-acp"], required=True)
    parser.add_argument("--patch", required=True)
    parser.add_argument("--home", help="Reuse a visible, already-provisioned Hermes home")
    args = parser.parse_args()

    binary = str(Path(args.binary).resolve())
    patch = str(Path(args.patch).resolve())
    out = Path(args.out).resolve()
    out.mkdir(parents=True, exist_ok=False)
    root = out / "root"
    workspace = out / "workspace"
    home = Path(args.home).resolve() if args.home else out / "home"
    sessions = root / ".sessions"
    transcripts = root / ".transcripts"
    for directory in [root, workspace, home, sessions, transcripts]:
        directory.mkdir(parents=True, exist_ok=True)
    (root / "log.jsonl").touch()
    prompt = out / "prompt.md"
    prompt.write_text(PROMPT)
    (workspace / "notes.txt").write_text("first line\nlast line ⟜ VESPER-OK-7731\n")
    (workspace / "scratch.txt").write_text("checkpoint-zero\n")
    report = workspace / "report.sh"
    report.write_text(
        "#!/bin/sh\n"
        "i=1\n"
        "while [ \"$i\" -le 400 ]; do\n"
        "  printf 'REPORT-ROW-%03d repeated-payload-for-cache-measurement\\n' \"$i\"\n"
        "  i=$((i + 1))\n"
        "done\n"
        "printf 'REPORT-TOTAL=400\\n'\n"
    )
    report.chmod(0o755)

    common = [
        binary,
        "--root",
        str(root),
        "--name",
        "seat",
        "--prompt",
        str(prompt),
        "--delivery-budget",
        "6",
    ]
    if args.arm == "hermes":
        command = common + [
            "--backend",
            "hermes",
            "--model",
            "k3.1-pro",
            "--provider",
            "kimi2-coding",
            "--reasoning",
            "high",
            "--session",
            str(sessions / "seat.sid"),
        ]
    else:
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
            "--delivery-budget",
            "6",
        ]

    result = {
        "arm": args.arm,
        "binary": binary,
        "patch": patch,
        "model": "k3.1-pro",
        "provider": "kimi2-coding" if args.arm == "hermes" else "kimi2",
        "root": str(root),
        "workspace": str(workspace),
        "home": str(home),
        "prompt_bytes": len(PROMPT.encode()),
        "report_bytes": report.stat().st_size,
        "phases": [],
    }
    env = os.environ.copy()
    if args.arm == "hermes":
        env["HERMES_HOME"] = str(home)
        (home / "config.yaml").write_text(
            "providers:\n"
            "  kimi2-coding:\n"
            "    name: Kimi Coding 2\n"
            "    api: https://api.kimi.com/coding/v1\n"
            "    key_env: KIMI2_API_KEY\n"
        )

    try:
        for phase, phase_tasks in enumerate([TASKS[:2], TASKS[2:]], start=1):
            stdout_path = out / f"phase-{phase}.out"
            stderr_path = out / f"phase-{phase}.err"
            started = time.time()
            with stdout_path.open("w") as stdout, stderr_path.open("w") as stderr:
                process = subprocess.Popen(
                    command,
                    cwd=workspace,
                    env=env,
                    stdout=stdout,
                    stderr=stderr,
                    text=True,
                    start_new_session=True,
                )
                phase_result = {"phase": phase, "pid": process.pid, "posts": []}
                result["phases"].append(phase_result)
                try:
                    wait_until(
                        "cursor",
                        lambda: "cursor:" in stdout_path.read_text(),
                        30,
                        process,
                    )
                    for offset, task in enumerate(phase_tasks):
                        index = (phase - 1) * 2 + offset
                        posted = subprocess.run(
                            [
                                binary,
                                "bus",
                                "post",
                                "--root",
                                str(root),
                                "--from",
                                "pilot",
                                "--to",
                                "seat",
                                "--body",
                                task,
                            ],
                            check=True,
                            text=True,
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE,
                        )
                        row = json.loads(posted.stdout)
                        post_id = row["id"]
                        post_started = time.monotonic()
                        print(f"PHASE {phase} POST {post_id}: {task}", flush=True)

                        def settled():
                            transcript = transcripts / "seat.jsonl"
                            rows = transcript.read_text().splitlines() if transcript.exists() else []
                            if len(rows) < index + 1:
                                return False
                            outcome = json.loads(rows[index])["outcome"]
                            if outcome != "settled":
                                raise RuntimeError(f"turn {index + 1} recorded outcome {outcome}")
                            cursor = subprocess.run(
                                [binary, "bus", "cursor", "get", "--root", str(root), "seat"],
                                check=True,
                                text=True,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE,
                            ).stdout.strip()
                            return int(cursor) >= post_id + 1

                        wait_until(f"turn {index + 1}", settled, 240, process)
                        phase_result["posts"].append(
                            {"post_id": post_id, "wait_seconds": time.monotonic() - post_started}
                        )
                    halted = subprocess.run(
                        [
                            binary,
                            "bus",
                            "post",
                            "--root",
                            str(root),
                            "--from",
                            "pilot",
                            "--to",
                            "seat",
                            "--body",
                            "🟢",
                        ],
                        check=True,
                        text=True,
                        stdout=subprocess.PIPE,
                        stderr=subprocess.PIPE,
                    )
                    phase_result["halt"] = json.loads(halted.stdout)
                    phase_result["exit"] = stop_process(process, stdout)
                    phase_result["seconds"] = time.time() - started
                except Exception:
                    phase_result["exit"] = stop_process(process, stdout, timeout=2)
                    raise
    except Exception as error:
        result["error"] = f"{type(error).__name__}: {error}"
        (out / "run.json").write_text(json.dumps(result, indent=2) + "\n")
        raise
    (out / "run.json").write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
