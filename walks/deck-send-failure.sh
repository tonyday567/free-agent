#!/usr/bin/env bash
# deck-send-failure — a failed deck send keeps the draft and shows the CLI fault.

source "$(dirname "$0")/lib.sh"
source "$(dirname "$0")/deck-lib"

step "a composition addressed through a CLI that exits 3"
new_root send-failure
stub="$ROOT/failing-free-agent"
printf '%s\n' '#!/usr/bin/env bash' 'cat >/dev/null' 'printf "injected send failure\\n"' 'exit 3' >"$stub"
chmod +x "$stub"

out=$(deck_eval '(progn
  (setq free-agent-bus-executable
        (expand-file-name "failing-free-agent" (getenv "FREE_AGENT_WALK_ROOT")))
  (free-agent-bus-post)
  (insert "draft survives")
  (setq free-agent-bus-post-to (list "yin"))
  (free-agent-bus-send (point-min) (point-max))
  (let ((draft (buffer-substring-no-properties (point-min) (point-max)))
        (err (with-current-buffer "*free-agent-bus-error*"
               (buffer-substring-no-properties (point-min) (point-max)))))
    (princ (format "draft=%s\nexit=%S\noutput=%S"
                   draft
                   (and (string-match-p "bus say exited 3" err) t)
                   (and (string-match-p "injected send failure" err) t)))))')
assert_eq "the draft, exit status and CLI output all survive" \
  $'draft=draft survives\nexit=t\noutput=t' "$out"
assert_eq "the failed send appends no log bytes" "0" \
  "$(wc -c <"$ROOT/log.jsonl" | tr -d ' ')"

finish
