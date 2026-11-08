#!/usr/bin/env bash
# deck-log-navigation — reading the log: search in the read-only tail, the
# thread at point with its way back, and file references that open at their
# section.  Scratch root and batch Emacs only.

source "$(dirname "$0")/lib.sh"
source "$(dirname "$0")/deck-lib"

step "a scratch bus whose posts cite files, and the cards they cite"
new_root log-nav
mkdir -p "$ROOT/loom" "$ROOT/buff"
cat > "$ROOT/loom/mac-survey.md" <<'MD'
# mac-survey

### heron

body
MD
printf 'reply card\n' > "$ROOT/loom/mark-reply.md"
printf 'note\n' > "$ROOT/loom/runner-note.md"
printf 'dup\n' > "$ROOT/buff/dup.md"
printf 'dup\n' > "$ROOT/loom/dup.md"
ABS="$ROOT/loom/mac-survey.md"
post alice runner "root about [mac-survey §heron](mac-survey.md#heron) and $ABS + \`loom/mark-reply.md\` + bare runner-note.md + [gone](nope/missing.md#x) + [dead](loom/mark-reply.md#nope) + dup.md"
printf 'reply' | "$FA" bus post -r "$ROOT" --from alice --to runner --thread 0 >/dev/null

step "RET and TAB resolve, the tail is read-only, and search runs in it"
out=$(deck_eval '(progn
  (setq free-agent-bus-reference-roots
        (list (getenv "FREE_AGENT_WALK_ROOT")
              (concat (getenv "FREE_AGENT_WALK_ROOT") "/loom")
              (concat (getenv "FREE_AGENT_WALK_ROOT") "/buff")))
  (free-agent-bus-log)
  (goto-char (point-min))
  (let ((ro buffer-read-only)
        (ret (key-binding (kbd "RET")))
        (tab (key-binding (kbd "TAB"))))
    (condition-case err (execute-kbd-macro (kbd "C-s heron RET")) (error nil))
    (let ((hit (string-match-p "heron"
                               (buffer-substring-no-properties
                                (line-beginning-position) (line-end-position)))))
      (princ (format "readonly=%S\nret=%S\ntab=%S\nsearch-hit=%S"
                     ro ret tab (and hit t))))))')
assert_eq "search in the read-only tail; RET/TAB bound" \
  $'readonly=t\nret=free-agent-bus-thread-at-point\ntab=free-agent-bus-cycle-depth\nsearch-hit=t' "$out"

step "references resolve; unresolved and ambiguous ones stay plain"
out=$(deck_eval '(progn
  (setq free-agent-bus-reference-roots
        (list (getenv "FREE_AGENT_WALK_ROOT")
              (concat (getenv "FREE_AGENT_WALK_ROOT") "/loom")
              (concat (getenv "FREE_AGENT_WALK_ROOT") "/buff"))
        free-agent-bus-log-depth 2)
  (free-agent-bus-log)
  (free-agent-bus-log-refresh t)
  (let (recs)
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let ((f (get-text-property (point) (quote free-agent-bus-target-file))))
        (when f
          (push (format "%s|%s" (file-name-nondirectory f)
                        (or (get-text-property (point) (quote free-agent-bus-target-anchor)) "-"))
                recs)))
      (goto-char (or (next-single-property-change (point) (quote free-agent-bus-target-file))
                     (point-max))))
    (princ (format "n=%d\n%s" (length recs) (mapconcat (function identity) (nreverse recs) "\n")))))')
assert_eq "five resolved references, in order" \
  $'n=5\nmac-survey.md|heron\nmac-survey.md|-\nmark-reply.md|-\nrunner-note.md|-\nmark-reply.md|nope' "$out"

step "RET at a reference opens it; the anchor jumps; a miss says so"
out=$(deck_eval '(progn
  (setq free-agent-bus-reference-roots
        (list (getenv "FREE_AGENT_WALK_ROOT")
              (concat (getenv "FREE_AGENT_WALK_ROOT") "/loom")
              (concat (getenv "FREE_AGENT_WALK_ROOT") "/buff"))
        free-agent-bus-log-depth 2)
  (defun walk-to (anchor)
    (goto-char (point-min))
    (let ((found nil))
      (while (and (not found) (< (point) (point-max)))
        (if (equal (get-text-property (point) (quote free-agent-bus-target-anchor)) anchor)
            (setq found (point))
          (goto-char (or (next-single-property-change (point) (quote free-agent-bus-target-file))
                         (point-max)))))
      found))
  (free-agent-bus-log)
  (free-agent-bus-log-refresh t)
  (let ((log (current-buffer))
        (key nil) (buf nil) (line nil))
    (let ((p (walk-to "heron")))
      (setq key (key-binding (kbd "RET")))
      (when p (free-agent-bus-open-reference))
      (setq buf (buffer-name (current-buffer)))
      (setq line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (set-buffer log)
    (let ((p (walk-to "nope")))
      (when p (free-agent-bus-open-reference))
      (princ (format "key=%S\nanchor-buffer=%S\nanchor-line=%S\nmiss-buffer=%S\nmiss-at-top=%S"
                     key buf line (buffer-name (current-buffer)) (= (point) (point-min)))))))')
assert_eq "a reference opens; #heron jumps to its heading; #nope lands at the top" \
  $'key=free-agent-bus-open-reference\nanchor-buffer="mac-survey.md"\nanchor-line="### heron"\nmiss-buffer="mark-reply.md"\nmiss-at-top=t' "$out"

step "the thread at point opens, names its way back, and q buries it"
out=$(deck_eval '(progn
  (free-agent-bus-log)
  (goto-char (point-min))
  (search-forward "[0@" nil t)
  (let ((log (current-buffer))
        (cmd (key-binding (kbd "RET"))))
    (when (eq cmd (quote free-agent-bus-thread-at-point))
      (free-agent-bus-thread-at-point))
    (let ((hint (with-current-buffer "*free-agent-bus-thread*"
                  (save-excursion (goto-char (point-min))
                                  (search-forward "q: close" nil t))))
          (q (with-current-buffer "*free-agent-bus-thread*" (key-binding (kbd "q")))))
      (when (eq q (quote quit-window))
        (with-current-buffer "*free-agent-bus-thread*" (quit-window)))
      (princ (format "open=%S\nhint=%S\nq=%S\nburied=%S\nlog-alive=%S"
                     (eq cmd (quote free-agent-bus-thread-at-point))
                     (and hint t) q
                     (not (buffer-live-p (get-buffer "*free-agent-bus-thread*")))
                     (buffer-live-p log)))))) ')
assert_eq "thread opens, says how it closes, and q buries it without killing the log" \
  $'open=t\nhint=t\nq=quit-window\nburied=nil\nlog-alive=t' "$out"

finish
