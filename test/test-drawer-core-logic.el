;; -*- lexical-binding: t; -*-
;;; test-drawer-core-logic.el --- Tests for drawer visibility core logic

;;; Commentary:
;; Tests for the refactored drawer visibility functions that don't depend on org-mode display

;;; Code:

(require 'buttercup)

;; Load test helper
(add-to-list 'load-path (file-name-directory load-file-name))
(require 'test-helper)

;; Load config which has the refactored functions
(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))
(load-file (expand-file-name "../config.el" (file-name-directory load-file-name)))

(describe "Drawer Visibility Core Logic"

  (before-each
    ;; Initialize session state
    (setq claude-multi--session-start-time (current-time))
    (setq claude-multi--agents nil)
    (setq claude-multi--progress-buffer (get-buffer-create "*test-drawer-logic*")))

  (after-each
    ;; Cleanup
    (when (buffer-live-p claude-multi--progress-buffer)
      (kill-buffer claude-multi--progress-buffer))
    (setq claude-multi--agents nil)
    (setq claude-multi--progress-buffer nil))

  (describe "claude-multi--find-agent-headlines"

    (it "finds no headlines in empty buffer"
      (with-current-buffer claude-multi--progress-buffer
        (erase-buffer))
      (expect (claude-multi--find-agent-headlines) :to-equal nil))

    (it "finds single agent headline"
      (with-current-buffer claude-multi--progress-buffer
        (erase-buffer)
        (insert "* Session\n")
        (insert "** agent-1 | Task 1\n")
        (insert "Some content\n"))
      (let ((headlines (claude-multi--find-agent-headlines)))
        (expect (length headlines) :to-equal 1)))

    (it "finds multiple agent headlines"
      (with-current-buffer claude-multi--progress-buffer
        (erase-buffer)
        (insert "* Session\n")
        (insert "** agent-1 | Task 1\n")
        (insert "Some content\n")
        (insert "** agent-2 | Task 2\n")
        (insert "More content\n")
        (insert "** agent-3 | Task 3\n"))
      (let ((headlines (claude-multi--find-agent-headlines)))
        (expect (length headlines) :to-equal 3)))

    (it "returns positions in correct order"
      (with-current-buffer claude-multi--progress-buffer
        (erase-buffer)
        (insert "* Session\n")
        (insert "** agent-1\n")
        (insert "** agent-2\n"))
      (let ((headlines (claude-multi--find-agent-headlines)))
        (expect (car headlines) :to-be-less-than (cadr headlines))))

    (it "handles missing progress buffer gracefully"
      (setq claude-multi--progress-buffer nil)
      (expect (claude-multi--find-agent-headlines) :to-equal nil)))

  (describe "claude-multi--apply-to-headlines"

    (it "calls action function for each headline"
      (with-current-buffer claude-multi--progress-buffer
        (erase-buffer)
        (insert "** agent-1\n")
        (insert "** agent-2\n"))
      (let ((call-count 0))
        (claude-multi--apply-to-headlines
         (lambda () (setq call-count (1+ call-count))))
        (expect call-count :to-equal 2)))

    (it "action function is called with point at headline start"
      (with-current-buffer claude-multi--progress-buffer
        (erase-buffer)
        (insert "** agent-1\n")
        (insert "content\n")
        (insert "** agent-2\n"))
      (let ((positions nil))
        (claude-multi--apply-to-headlines
         (lambda () (push (point) positions)))
        (expect (length positions) :to-equal 2)
        ;; Verify each position is at start of a line with **
        (dolist (pos positions)
          (expect (with-current-buffer claude-multi--progress-buffer
                    (goto-char pos)
                    (looking-at "\\*\\* ")) :to-be-truthy))))

    (it "handles no headlines gracefully"
      (with-current-buffer claude-multi--progress-buffer
        (erase-buffer)
        (insert "No headlines here\n"))
      (let ((call-count 0))
        (claude-multi--apply-to-headlines
         (lambda () (setq call-count (1+ call-count))))
        (expect call-count :to-equal 0))))

  (describe "Display wrapper functions"

    (it "claude-multi--org-show-subtree-at-point checks fboundp"
      ;; Should not error even if org-show-subtree doesn't exist
      (expect (claude-multi--org-show-subtree-at-point) :not :to-throw))

    (it "claude-multi--org-hide-drawer-at-point checks fboundp"
      (expect (claude-multi--org-hide-drawer-at-point) :not :to-throw))

    (it "claude-multi--org-cycle-at-point checks fboundp"
      (expect (claude-multi--org-cycle-at-point) :not :to-throw)))

  (describe "Integration with user commands"

    (it "show command applies action to all headlines"
      (with-current-buffer claude-multi--progress-buffer
        (erase-buffer)
        (insert "** agent-1\n")
        (insert "** agent-2\n"))
      ;; Mock the org function
      (spy-on 'org-show-subtree)
      (claude-multi/show-all-status-drawers)
      ;; Verify it was called for each headline
      (expect (spy-calls-count 'org-show-subtree) :to-equal 2))

    (it "hide command applies action to all headlines"
      (with-current-buffer claude-multi--progress-buffer
        (erase-buffer)
        (insert "** agent-1\n"))
      (spy-on 'org-hide-drawer-all)
      (claude-multi/hide-all-status-drawers)
      (expect 'org-hide-drawer-all :to-have-been-called))

    (it "toggle command applies action to all headlines"
      (with-current-buffer claude-multi--progress-buffer
        (erase-buffer)
        (insert "** agent-1\n"))
      (spy-on 'org-cycle)
      (claude-multi/toggle-all-status-drawers)
      (expect 'org-cycle :to-have-been-called))

    (it "commands handle missing buffer gracefully"
      (setq claude-multi--progress-buffer nil)
      (expect (claude-multi/show-all-status-drawers) :not :to-throw)
      (expect (claude-multi/hide-all-status-drawers) :not :to-throw)
      (expect (claude-multi/toggle-all-status-drawers) :not :to-throw))))

(provide 'test-drawer-core-logic)
;;; test-drawer-core-logic.el ends here
