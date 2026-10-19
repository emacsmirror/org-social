;;; org-social-ui-boost-test.el --- Tests for actions on boosted posts -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0

;;; Commentary:

;; A simple boost (INCLUDE with an empty body) shares someone else's post,
;; so its Reply, React, Boost, Thread, Profile and Share buttons must act
;; on the boosted post, while Edit keeps acting on the boost itself.
;; The network and every write are stubbed: nothing is published.
;;
;; Run it in batch mode with `ert-run-tests-batch-and-exit', with `.',
;; `ui' and `ui/buffers' in the `load-path'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-social)
(require 'org-social-ui-components)

(defconst org-social-ui-boost-test--me "https://host.org-social.org/andros/social.org")
(defconst org-social-ui-boost-test--alice "https://alice.com/social.org")
(defconst org-social-ui-boost-test--bob "https://bob.com/social.org")
(defconst org-social-ui-boost-test--boosted
  "https://alice.com/social.org#2025-05-01T10:00:00+0100")

(defun org-social-ui-boost-test--post (author-url nick &rest props)
  "Build a post by AUTHOR-URL with NICK and extra PROPS (an alist)."
  (append `((timestamp . "2025-05-01T14:00:00+0100")
            (author-url . ,author-url)
            (author-nick . ,nick))
          props))

(defun org-social-ui-boost-test--render (post)
  "Render POST with stubbed network and return the recorded calls.
The result is an alist from button label to the call its press made,
plus `interactions' with the URL whose reactions were fetched."
  (let ((calls '())
        (org-social-variables--my-profile
         `((nick . "andros") (url . ,org-social-ui-boost-test--me)))
        (org-social-live-preview-url "https://preview.example/"))
    (cl-letf (((symbol-function 'org-social-ui--fetch-post-reactions-sync)
               (lambda (url data) (push (cons 'interactions url) calls) data))
              ((symbol-function 'org-social-ui--fetch-post-sync)
               (lambda (_url)
                 `((timestamp . "2025-05-01T10:00:00+0100")
                   (author-nick . "alice")
                   (text . "Original post"))))
              ((symbol-function 'org-social-ui--post-has-real-replies-p) #'ignore)
              ((symbol-function 'org-social-ui--apply-org-mode-to-region) #'ignore)
              ((symbol-function 'org-social-file--new-post)
               (lambda (&rest args) (push (cons "↳" args) calls)))
              ((symbol-function 'org-social-ui--add-reaction)
               (lambda (&rest args) (push (cons "😊" args) calls)))
              ((symbol-function 'org-social-ui--boost-post)
               (lambda (&rest args) (push (cons "🔄" args) calls)))
              ((symbol-function 'org-social-ui-thread)
               (lambda (&rest args) (push (cons "🧵" args) calls)))
              ((symbol-function 'org-social-ui-profile)
               (lambda (&rest args) (push (cons "👤" args) calls)))
              ((symbol-function 'org-social-ui--open-live-preview)
               (lambda (&rest args) (push (cons "🔗" args) calls)))
              ((symbol-function 'org-social-file--edit-post)
               (lambda (&rest args) (push (cons "✏️" args) calls))))
      (with-temp-buffer
        (org-social-ui--post-component post)
        (goto-char (point-min))
        (while (< (point) (point-max))
          (let ((widget (widget-at (point))))
            (if (and widget (eq (widget-type widget) 'push-button))
                (progn
                  (funcall (widget-get widget :notify) widget)
                  (goto-char (widget-get widget :to)))
              (forward-char 1))))))
    calls))

(defun org-social-ui-boost-test--call (calls label)
  "Return the arguments recorded in CALLS for the button LABEL."
  (cdr (assoc label calls)))

(ert-deftest org-social-ui-boost-test-simple-boost-acts-on-boosted-post ()
  "Bob's simple boost of Alice's post: every content action targets Alice."
  (let* ((calls (org-social-ui-boost-test--render
                 (org-social-ui-boost-test--post
                  org-social-ui-boost-test--bob "bob"
                  `(include . ,org-social-ui-boost-test--boosted))))
         (alice org-social-ui-boost-test--alice)
         (id "2025-05-01T10:00:00+0100"))
    (should (equal (org-social-ui-boost-test--call calls 'interactions)
                   org-social-ui-boost-test--boosted))
    (should (equal (org-social-ui-boost-test--call calls "😊") (list alice id)))
    (should (equal (org-social-ui-boost-test--call calls "↳") (list alice id)))
    (should (equal (org-social-ui-boost-test--call calls "🔄") (list alice id)))
    (should (equal (org-social-ui-boost-test--call calls "🔗") (list alice id)))
    (should (equal (org-social-ui-boost-test--call calls "👤") (list alice)))
    (should (equal (org-social-ui-boost-test--call calls "🧵")
                   (list org-social-ui-boost-test--boosted)))
    (should-not (assoc "✏️" calls))))

(ert-deftest org-social-ui-boost-test-own-simple-boost ()
  "My simple boost: content actions target Alice, Edit targets my boost."
  (let ((calls (org-social-ui-boost-test--render
                (org-social-ui-boost-test--post
                 org-social-ui-boost-test--me "andros"
                 `(include . ,org-social-ui-boost-test--boosted)))))
    (should (equal (org-social-ui-boost-test--call calls "😊")
                   (list org-social-ui-boost-test--alice "2025-05-01T10:00:00+0100")))
    (should (equal (org-social-ui-boost-test--call calls "✏️")
                   (list "2025-05-01T14:00:00+0100")))))

(ert-deftest org-social-ui-boost-test-boost-of-my-post ()
  "Bob's simple boost of my post: no reply, reaction or boost on myself."
  (let ((calls (org-social-ui-boost-test--render
                (org-social-ui-boost-test--post
                 org-social-ui-boost-test--bob "bob"
                 `(include . ,(concat org-social-ui-boost-test--me
                                      "#2025-05-01T10:00:00+0100"))))))
    (should-not (assoc "😊" calls))
    (should-not (assoc "↳" calls))
    (should-not (assoc "🔄" calls))
    (should-not (assoc "✏️" calls))))

(ert-deftest org-social-ui-boost-test-quote-boost-acts-on-itself ()
  "Bob's quote boost carries his own text, so actions target Bob's post."
  (let* ((calls (org-social-ui-boost-test--render
                 (org-social-ui-boost-test--post
                  org-social-ui-boost-test--bob "bob"
                  `(include . ,org-social-ui-boost-test--boosted)
                  '(text . "Guys, you have to see this!"))))
         (bob (list org-social-ui-boost-test--bob "2025-05-01T14:00:00+0100")))
    (should (equal (org-social-ui-boost-test--call calls 'interactions)
                   (concat org-social-ui-boost-test--bob "#2025-05-01T14:00:00+0100")))
    (should (equal (org-social-ui-boost-test--call calls "😊") bob))
    (should (equal (org-social-ui-boost-test--call calls "↳") bob))))

(ert-deftest org-social-ui-boost-test-regular-post-unchanged ()
  "A regular post keeps acting on itself."
  (let ((calls (org-social-ui-boost-test--render
                (org-social-ui-boost-test--post
                 org-social-ui-boost-test--bob "bob" '(text . "Hello")))))
    (should (equal (org-social-ui-boost-test--call calls "😊")
                   (list org-social-ui-boost-test--bob "2025-05-01T14:00:00+0100")))
    (should (equal (org-social-ui-boost-test--call calls "👤")
                   (list org-social-ui-boost-test--bob)))))

(ert-deftest org-social-ui-boost-test-simple-boost-target ()
  "Only an INCLUDE with an empty or blank body is a simple boost."
  (should (equal (org-social-ui--simple-boost-target
                  `((include . ,org-social-ui-boost-test--boosted) (text . " \n")))
                 org-social-ui-boost-test--boosted))
  (should-not (org-social-ui--simple-boost-target
               `((include . ,org-social-ui-boost-test--boosted) (text . "Hi"))))
  (should-not (org-social-ui--simple-boost-target '((include . "") (text . ""))))
  (should-not (org-social-ui--simple-boost-target '((text . "")))))

(ert-deftest org-social-ui-boost-test-split-post-url ()
  "A post URL splits at its last #."
  (should (equal (org-social-ui--split-post-url org-social-ui-boost-test--boosted)
                 (cons org-social-ui-boost-test--alice "2025-05-01T10:00:00+0100"))))

(provide 'org-social-ui-boost-test)
;;; org-social-ui-boost-test.el ends here
