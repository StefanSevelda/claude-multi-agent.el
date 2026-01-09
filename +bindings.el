;;; tools/claude-multi/+bindings.el -*- lexical-binding: t; -*-

;; Keybindings for claude-multi
;; This file is loaded automatically by Doom after config.el

(map! :leader
      :prefix ("c m" . "claude-multi")
      :desc "Start session"           "s" #'claude-multi/start-session
      :desc "Spawn agent"             "a" #'claude-multi/spawn-agent
      :desc "Spawn with worktree"     "w" #'claude-multi/spawn-agent-with-worktree
      :desc "Open progress"           "p" #'claude-multi/open-progress
      :desc "Dashboard"               "d" #'claude-multi/dashboard
      :desc "Focus agent"             "f" #'claude-multi/focus-agent
      :desc "Kill agent"              "k" #'claude-multi/kill-agent
      :desc "Kill all"                "K" #'claude-multi/kill-all-agents
      :desc "Cleanup status files"    "c" #'claude-multi/cleanup-status-files
      :desc "Debug status matching"   "?" #'claude-multi/debug-status-matching
      :desc "Export progress"         "e" #'claude-multi/export-progress
      :desc "List worktrees"          "l" #'claude-multi/list-worktrees
      :desc "Save session"            "S" #'claude-multi/save-session
      :desc "Restore session"         "R" #'claude-multi/restore-session
      :desc "List sessions"           "L" #'claude-multi/list-sessions
      :desc "Delete session"          "D" #'claude-multi/delete-session
      (:prefix ("r" . "review")
       :desc "Review agent changes"   "r" #'claude-multi/review-agent-changes
       :desc "Accept current diff"    "a" #'claude-multi/accept-current-diff
       :desc "Reject current diff"    "x" #'claude-multi/reject-current-diff
       :desc "Next diff file"         "n" #'claude-multi/next-diff-file))
