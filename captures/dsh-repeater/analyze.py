#!/usr/bin/env python3
"""Recompute the matched-run token, outcome, duplication, and latency measures."""

import argparse
from collections import Counter, defaultdict
import json
from pathlib import Path

EXPECTED = {
    0: "last line ⟜ VESPER-OK-7731",
    2: "400",
    5: "2",
    7: "NOTES=last line ⟜ VESPER-OK-7731 REPORT=400 SCRATCH=2",
}


def load_jsonl(path):
    return [json.loads(line) for line in path.read_text().splitlines() if line]


def hermes_usage(row):
    raw = row["raw"]
    start = raw.rfind("\n{")
    if start < 0:
        raise ValueError("Hermes transcript row has no usage report")
    report = json.loads(raw[start + 1 :])
    if not report["completed"] or report["failed"]:
        raise ValueError("Hermes usage report is not completed")
    fresh = report["input_tokens"] - report["cache_read_tokens"]
    cached = report["cache_read_tokens"]
    output = report["output_tokens"]
    if report["total_tokens"] != fresh + cached + output:
        raise ValueError("Hermes C/F/O identity failed")
    return {
        "turn": None,
        "requests": report["api_calls"],
        "F": fresh,
        "C": cached,
        "O": output,
        "mass": report["total_tokens"],
        "auxiliary_tokens": report.get("auxiliary", {}).get("total_tokens", 0),
    }


def dsh_usage(events):
    grouped = defaultdict(lambda: {"requests": 0, "F": 0, "C": 0, "O": 0, "mass": 0})
    for event in events:
        if event.get("type") != "assistant/message":
            continue
        data = event["data"]
        usage = data["usage"]
        fresh = usage["inputTokens"]
        cached = usage.get("cacheReadTokens", 0)
        output = usage["outputTokens"]
        if usage["totalTokens"] != fresh + cached + output:
            raise ValueError(f"dsh C/F/O identity failed at seq {event['seq']}")
        turn = grouped[data["turn"]]
        turn["requests"] += 1
        turn["F"] += fresh
        turn["C"] += cached
        turn["O"] += output
        turn["mass"] += usage["totalTokens"]
    return [dict(turn=number, **grouped[number]) for number in sorted(grouped)]


def sum_usage(turns):
    keys = ["requests", "F", "C", "O", "mass", "auxiliary_tokens"]
    return {key: sum(turn.get(key, 0) for turn in turns) for key in keys}


def analyze(arm):
    root = arm / "root"
    run = json.loads((arm / "run.json").read_text())
    posts = load_jsonl(root / "log.jsonl")
    transcript = load_jsonl(root / ".transcripts" / "seat.jsonl")
    replies = [post for post in posts if post["from"] == "seat" and "pilot" in post["to"]]
    accepted_by_post = {
        post_id: sum(post["body"] == expected and post["thread"] == [post_id] for post in replies)
        for post_id, expected in EXPECTED.items()
    }
    accepted = sum(count == 1 for count in accepted_by_post.values())
    duplicates = sum(max(0, count - 1) for count in Counter(
        thread for post in replies for thread in post["thread"]
    ).values())
    failed_turns = sum(row["outcome"] != "settled" for row in transcript)
    elapsed_ms = sum(row["elapsed_ms"] for row in transcript)
    scratch = (arm / "workspace" / "scratch.txt").read_text()
    if scratch != "checkpoint-zero\ncheckpoint-seven\n":
        accepted = 0

    if run["arm"] == "hermes":
        turns = [hermes_usage(row) for row in transcript]
        for number, turn in enumerate(turns, start=1):
            turn["turn"] = number
        session_events = None
    else:
        session_events = load_jsonl(arm / "session.jsonl")
        turns = dsh_usage(session_events)
    totals = sum_usage(turns)
    result = {
        "arm": run["arm"],
        "model": run["model"],
        "provider": run["provider"],
        "accepted_outcomes": accepted,
        "accepted_by_post": accepted_by_post,
        "replies": len(replies),
        "duplicate_posts": duplicates,
        "failed_turns": failed_turns,
        "transcript_rows": len(transcript),
        "process_exits": [phase["exit"] for phase in run["phases"]],
        "session_ids": sorted({row["session_id"] for row in transcript}),
        "turns": turns,
        "totals": totals,
        "tokens_per_accepted_outcome": totals["mass"] / accepted if accepted else None,
        "latency_ms_per_accepted_outcome": elapsed_ms / accepted if accepted else None,
        "elapsed_ms": elapsed_ms,
        "driver_wall_seconds": sum(phase["seconds"] for phase in run["phases"]),
        "scratch_bytes": len(scratch.encode()),
        "prompt_bytes": run["prompt_bytes"],
    }
    if session_events is not None:
        headers = [event for event in session_events if event.get("type") == "request/header"]
        tools = headers[0]["data"]["header"].get("tools", []) if headers else []
        result["dsh"] = {
            "request_headers": len(headers),
            "tool_schema_count": len(tools),
            "tool_schema_json_bytes": len(json.dumps(tools, separators=(",", ":")).encode()),
            "tool_calls": sum(event.get("type") == "tool/call" for event in session_events),
            "tool_results": sum(event.get("type") == "tool/result" for event in session_events),
            "turn_end_reasons": [
                event["data"]["reason"] for event in session_events if event.get("type") == "turn/end"
            ],
            "session_log": str(arm / "session.jsonl"),
        }
    return result


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("arms", nargs="+", type=Path)
    parser.add_argument("--out", required=True, type=Path)
    args = parser.parse_args()
    result = {"arms": [analyze(arm.resolve()) for arm in args.arms]}
    args.out.write_text(json.dumps(result, indent=2, ensure_ascii=False) + "\n")
    print(json.dumps(result, indent=2, ensure_ascii=False))


if __name__ == "__main__":
    main()
