;;; org-social-vfile-test.el --- Tests for vfile synchronization -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0

;;; Commentary:

;; A feed hosted with a vfile can be edited from several clients.  These
;; tests check that Emacs never overwrites posts published elsewhere:
;; downloads bypass caches and remember the ETag, uploads are refused
;; when the feed changed on the host, and conflicts keep both versions.
;; The host is simulated by stubbing `url-retrieve' and
;; `url-retrieve-synchronously': nothing goes to the network.
;;
;; Run it in batch mode with `ert-run-tests-batch-and-exit', with `.',
;; `ui' and `ui/buffers' in the `load-path'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-social)
(require 'org-social-accounts)
(require 'org-social-file)
(require 'org-social-ui-components)
(require 'org-social-ui-discover)

(defconst org-social-vfile-test--vfile
  (concat "https://host.example/vfile?token=" (make-string 64 ?a)
          "&ts=1763972561&sig=" (make-string 64 ?b)))

(defconst org-social-vfile-test--public-url "https://host.example/test/social.org")

(defconst org-social-vfile-test--feed
  "#+TITLE: Test\n#+NICK: test\n\n* Posts\n** 2025-05-01T10:00:00+0100\n:PROPERTIES:\n:END:\n\nFirst post from Emacs\n")

(defconst org-social-vfile-test--android-post
  "\n** 2025-05-02T10:00:00+0100\n:PROPERTIES:\n:CLIENT: android\n:END:\n\nPost from Android\n")

;;; Simulated host

(defvar org-social-vfile-test--remote nil
  "Feed content stored on the simulated host.")

(defvar org-social-vfile-test--if-match-support t
  "Non-nil if the simulated host honors If-Match on upload.")

(defvar org-social-vfile-test--upload-reply nil
  "When non-nil, a function returning a raw response for an upload.
If it returns nil, the upload is handled normally.")

(defvar org-social-vfile-test--retry-delays nil
  "Delays requested before retrying a package action, most recent first.")

(defvar org-social-vfile-test--requests nil
  "Requests received by the simulated host, most recent first.
Each one is a plist (:method :url :headers :data).")

(defvar org-social-vfile-test--warnings nil
  "Warnings shown with `display-warning', most recent first.")

(defvar org-social-vfile-test--ediffs nil
  "Arguments of the ediff sessions opened, most recent first.")

(defun org-social-vfile-test--etag (content)
  "Return the ETag the host serves for CONTENT."
  (format "\"%s\"" (secure-hash 'sha256 (encode-coding-string content 'utf-8))))

(defun org-social-vfile-test--http (status &optional headers body)
  "Build a raw HTTP response with STATUS, HEADERS (an alist) and BODY."
  (concat (format "HTTP/1.1 %d X\r\n" status)
          (mapconcat (lambda (h) (format "%s: %s\r\n" (car h) (cdr h))) headers "")
          "\r\n"
          (or body "")))

(defun org-social-vfile-test--uploaded-file (data)
  "Extract the uploaded social.org from the multipart DATA."
  (when (string-match "charset=utf-8\r\n\r\n\\(\\(?:.\\|\n\\)*\\)\r\n--[^\r\n]*--\r\n\\'" data)
    (decode-coding-string (match-string 1 data) 'utf-8)))

(defun org-social-vfile-test--serve (url)
  "Answer the request to URL as the simulated host, using url.el variables."
  (let ((method (or url-request-method "GET"))
        (headers url-request-extra-headers))
    (push (list :method method :url url :headers headers :data url-request-data
                :noninteractive url-request-noninteractive)
          org-social-vfile-test--requests)
    (cond
     ((and (equal method "GET")
           (string-prefix-p org-social-vfile-test--public-url url))
      (org-social-vfile-test--http
       200 `(("ETag" . ,(org-social-vfile-test--etag org-social-vfile-test--remote)))
       org-social-vfile-test--remote))
     ((and (equal method "POST") (equal url "https://host.example/upload"))
      (let ((if-match (cdr (assoc "If-Match" headers))))
        (cond
         ((and org-social-vfile-test--upload-reply
               (funcall org-social-vfile-test--upload-reply)))
         ((and org-social-vfile-test--if-match-support
               if-match
               (not (equal if-match (org-social-vfile-test--etag
                                     org-social-vfile-test--remote))))
          (org-social-vfile-test--http 412 nil "{\"errors\": [\"Precondition Failed\"]}"))
         (t
          (setq org-social-vfile-test--remote
                (org-social-vfile-test--uploaded-file url-request-data))
          (org-social-vfile-test--http
           200 (when org-social-vfile-test--if-match-support
                 `(("ETag" . ,(org-social-vfile-test--etag org-social-vfile-test--remote))))
           "{\"type\": \"Success\"}")))))
     (t (org-social-vfile-test--http 404)))))

(defun org-social-vfile-test--response-buffer (raw)
  "Return a new unibyte buffer holding the RAW response, like url.el."
  (let ((buf (generate-new-buffer " *org-social-vfile-test-http*")))
    (with-current-buffer buf
      (set-buffer-multibyte nil)
      (insert (encode-coding-string raw 'utf-8)))
    buf))

(defun org-social-vfile-test--url-retrieve (url callback &optional cbargs _silent _inhibit)
  "Stub of `url-retrieve': answer URL and call CALLBACK with CBARGS."
  (let ((buf (org-social-vfile-test--response-buffer
              (org-social-vfile-test--serve url))))
    (with-current-buffer buf
      (apply callback nil cbargs))
    buf))

(defun org-social-vfile-test--url-retrieve-synchronously (url &rest _)
  "Stub of `url-retrieve-synchronously': answer URL."
  (org-social-vfile-test--response-buffer (org-social-vfile-test--serve url)))

(defmacro org-social-vfile-test--with-host (&rest body)
  "Run BODY with a simulated host and a temporary `user-emacs-directory'."
  (declare (indent 0) (debug t))
  `(let* ((dir (make-temp-file "org-social-vfile-test" t))
          (user-emacs-directory (file-name-as-directory dir))
          (org-social-file org-social-vfile-test--vfile)
          (org-social-my-public-url org-social-vfile-test--public-url)
          (org-social-accounts--current nil)
          (org-social-vfile-conflict-ediff t)
          (org-social-vfile-test--remote org-social-vfile-test--feed)
          (org-social-vfile-test--if-match-support t)
          (org-social-vfile-test--upload-reply nil)
          (org-social-vfile-test--requests nil)
          (org-social-vfile-test--warnings nil)
          (org-social-vfile-test--ediffs nil)
          (org-social-vfile-test--retry-delays nil)
          (org-social-file--upload-state (make-hash-table :test #'equal))
          (original-run-at-time (symbol-function 'run-at-time)))
     (cl-letf (((symbol-function 'url-retrieve) #'org-social-vfile-test--url-retrieve)
               ((symbol-function 'url-retrieve-synchronously)
                #'org-social-vfile-test--url-retrieve-synchronously)
               ((symbol-function 'display-warning)
                (lambda (type message &optional level _buffer)
                  (push (list type message level) org-social-vfile-test--warnings)))
               ((symbol-function 'run-at-time)
                (lambda (time repeat function &rest args)
                  (cond
                   ((eq function #'org-social-file--open-conflict-ediff)
                    (push args org-social-vfile-test--ediffs))
                   ;; Retry at once instead of waiting
                   ((memq function '(org-social-file--run-action
                                     org-social-file--start-action))
                    (push time org-social-vfile-test--retry-delays)
                    (apply function args))
                   (t (apply original-run-at-time time repeat function args)))))
               ((symbol-function 'org-social-validator-validate-and-display) #'ignore)
               ((symbol-function 'recenter) #'ignore))
       (unwind-protect
           (progn ,@body)
         (dolist (buf (buffer-list))
           (when (and (buffer-file-name buf)
                      (file-in-directory-p (buffer-file-name buf) dir))
             (with-current-buffer buf
               (set-buffer-modified-p nil))
             (kill-buffer buf)))
         (delete-directory dir t)))))

(defun org-social-vfile-test--local ()
  "Return the path of the vfile local copy."
  (org-social-file--get-local-file-path org-social-file))

(defun org-social-vfile-test--read (path)
  "Return the contents of PATH."
  (org-social-file--read-local path))

(defun org-social-vfile-test--requests-with (method)
  "Return the recorded requests with METHOD, oldest first."
  (reverse (seq-filter (lambda (r) (equal (plist-get r :method) method))
                       org-social-vfile-test--requests)))

(defun org-social-vfile-test--conflict-files ()
  "Return the conflict copies next to the local copy."
  (file-expand-wildcards (concat (org-social-vfile-test--local) ".conflict-*")))

(defun org-social-vfile-test--edit-locally (content)
  "Replace the local copy with CONTENT, as a save from Emacs would."
  (org-social-file--write-local (org-social-vfile-test--local) content))

;;; Downloads

(ert-deftest org-social-vfile-test-no-cache-url ()
  "The cache-busting parameter respects an existing query."
  (should (string-match-p "\\`https://h/f\\.org\\?_t=[0-9]+\\'"
                          (org-social-file--no-cache-url "https://h/f.org")))
  (should (string-match-p "\\`https://h/f\\.org\\?a=1&_t=[0-9]+\\'"
                          (org-social-file--no-cache-url "https://h/f.org?a=1"))))

(ert-deftest org-social-vfile-test-download-bypasses-cache-and-stores-etag ()
  "Downloads add `_t' and no-cache headers, and remember the ETag."
  (org-social-vfile-test--with-host
   (should (eq (org-social-file--refresh-vfile-cache) 'updated))
   (let* ((request (car (org-social-vfile-test--requests-with "GET")))
          (headers (plist-get request :headers)))
     (should (string-match-p "/test/social\\.org\\?_t=[0-9]+\\'" (plist-get request :url)))
     (should (equal (cdr (assoc "Cache-Control" headers)) "no-cache"))
     (should (equal (cdr (assoc "Pragma" headers)) "no-cache")))
   (should (equal (org-social-vfile-test--read (org-social-vfile-test--local))
                  org-social-vfile-test--feed))
   (should (equal (plist-get (org-social-file--read-base (org-social-vfile-test--local)) :etag)
                  (org-social-vfile-test--etag org-social-vfile-test--feed)))
   (should (file-exists-p (concat (org-social-vfile-test--local) ".etag")))))

(ert-deftest org-social-vfile-test-async-download-bypasses-cache ()
  "The asynchronous download also bypasses caches and returns the ETag."
  (org-social-vfile-test--with-host
   (let (result)
     (org-social-file--download-vfile org-social-my-public-url
                                      (lambda (content etag) (setq result (list content etag))))
     (should (equal result (list org-social-vfile-test--feed
                                 (org-social-vfile-test--etag org-social-vfile-test--feed))))
     (let ((request (car (org-social-vfile-test--requests-with "GET"))))
       (should (string-match-p "_t=[0-9]+" (plist-get request :url)))
       (should (assoc "Cache-Control" (plist-get request :headers)))))))

;;; Uploads

(ert-deftest org-social-vfile-test-upload-sends-if-match-and-updates-etag ()
  "With the feed unchanged on the host, upload with If-Match."
  (org-social-vfile-test--with-host
   (org-social-file--refresh-vfile-cache)
   (let ((old-etag (org-social-vfile-test--etag org-social-vfile-test--feed))
         (edited (concat org-social-vfile-test--feed "\nEdited in Emacs\n")))
     (org-social-vfile-test--edit-locally edited)
     (org-social-file--upload-vfile org-social-file (org-social-vfile-test--local))
     (let ((post (car (org-social-vfile-test--requests-with "POST"))))
       (should post)
       (should (equal (cdr (assoc "If-Match" (plist-get post :headers))) old-etag)))
     (should (equal org-social-vfile-test--remote edited))
     (should (equal (plist-get (org-social-file--read-base (org-social-vfile-test--local)) :etag)
                    (org-social-vfile-test--etag edited)))
     (should-not org-social-vfile-test--warnings)
     (should-not (plist-get (org-social-file--state (org-social-vfile-test--local)) :status)))))

(ert-deftest org-social-vfile-test-upload-works-without-host-if-match-support ()
  "A host that ignores If-Match and sends no ETag still gets the upload."
  (org-social-vfile-test--with-host
   (org-social-file--refresh-vfile-cache)
   (setq org-social-vfile-test--if-match-support nil)
   (let ((edited (concat org-social-vfile-test--feed "\nEdited\n")))
     (org-social-vfile-test--edit-locally edited)
     (org-social-file--upload-vfile org-social-file (org-social-vfile-test--local))
     (should (equal org-social-vfile-test--remote edited))
     ;; Without an ETag, the SHA-256 of the content identifies the version
     (should (equal (plist-get (org-social-file--read-base (org-social-vfile-test--local)) :sha256)
                    (org-social-file--sha256 edited)))
     ;; The next edit is not mistaken for a conflict
     (org-social-vfile-test--edit-locally (concat edited "\nAgain\n"))
     (org-social-file--upload-vfile org-social-file (org-social-vfile-test--local))
     (should (equal org-social-vfile-test--remote (concat edited "\nAgain\n")))
     (should-not (org-social-vfile-test--conflict-files)))))

(defun org-social-vfile-test--assert-conflict (local-version remote-version)
  "Check the conflict outcome for LOCAL-VERSION and REMOTE-VERSION."
  (let ((conflicts (org-social-vfile-test--conflict-files))
        (local-path (org-social-vfile-test--local)))
    (should (= (length conflicts) 1))
    (should (equal (org-social-vfile-test--read (car conflicts)) local-version))
    (should (equal (org-social-vfile-test--read local-path) remote-version))
    (should (equal org-social-vfile-test--remote remote-version))
    (should (equal (plist-get (org-social-file--read-base local-path) :etag)
                   (org-social-vfile-test--etag remote-version)))
    (should (equal (car org-social-vfile-test--ediffs) (list (car conflicts) local-path)))
    (should (eq (plist-get (org-social-file--state local-path) :status) 'conflict))
    (let ((warning (car org-social-vfile-test--warnings)))
      (should (eq (nth 0 warning) 'org-social))
      (should (eq (nth 2 warning) :error))
      (should (string-match-p "org-social-force-upload" (nth 1 warning))))))

(ert-deftest org-social-vfile-test-remote-changed-does-not-upload ()
  "A post published from another client is not overwritten."
  (org-social-vfile-test--with-host
   (org-social-file--refresh-vfile-cache)
   (let ((android (concat org-social-vfile-test--feed org-social-vfile-test--android-post))
         (edited (concat org-social-vfile-test--feed "\nEdited in Emacs\n")))
     (setq org-social-vfile-test--remote android)
     (org-social-vfile-test--edit-locally edited)
     (org-social-file--upload-vfile org-social-file (org-social-vfile-test--local))
     (should-not (org-social-vfile-test--requests-with "POST"))
     (org-social-vfile-test--assert-conflict edited android))))

(ert-deftest org-social-vfile-test-host-412-is-a-conflict ()
  "If the host answers 412, nothing is lost and both versions are kept."
  (org-social-vfile-test--with-host
   (org-social-file--refresh-vfile-cache)
   (let ((android (concat org-social-vfile-test--feed org-social-vfile-test--android-post))
         (edited (concat org-social-vfile-test--feed "\nEdited in Emacs\n")))
     ;; Another client publishes between the check and the upload
     (setq org-social-vfile-test--upload-reply
           (lambda ()
             (setq org-social-vfile-test--remote android)
             (org-social-vfile-test--http 412 nil "Precondition Failed")))
     (org-social-vfile-test--edit-locally edited)
     (org-social-file--upload-vfile org-social-file (org-social-vfile-test--local))
     (should (= (length (org-social-vfile-test--requests-with "POST")) 1))
     (org-social-vfile-test--assert-conflict edited android))))

(ert-deftest org-social-vfile-test-unknown-base-is-a-conflict ()
  "Without a remembered version, a differing host feed is not overwritten."
  (org-social-vfile-test--with-host
   (let ((android (concat org-social-vfile-test--feed org-social-vfile-test--android-post)))
     (setq org-social-vfile-test--remote android)
     (org-social-vfile-test--edit-locally org-social-vfile-test--feed)
     (org-social-file--upload-vfile org-social-file (org-social-vfile-test--local))
     (should-not (org-social-vfile-test--requests-with "POST"))
     (org-social-vfile-test--assert-conflict org-social-vfile-test--feed android))))

(ert-deftest org-social-vfile-test-401-shows-invalid-vfile-warning ()
  "A 401 is reported with `display-warning' and retried on the next save."
  (org-social-vfile-test--with-host
   (org-social-file--refresh-vfile-cache)
   (setq org-social-vfile-test--upload-reply
         (lambda () (org-social-vfile-test--http
                     401 nil "{\"errors\": [\"Invalid vfile signature\"]}")))
   (let ((edited (concat org-social-vfile-test--feed "\nEdited\n"))
         (local-path (org-social-vfile-test--local)))
     (org-social-vfile-test--edit-locally edited)
     (org-social-file--upload-vfile org-social-file local-path)
     (let ((warning (car org-social-vfile-test--warnings)))
       (should (eq (nth 0 warning) 'org-social))
       (should (eq (nth 2 warning) :error))
       (should (string-match-p "HTTP 401" (nth 1 warning)))
       (should (string-match-p "the vfile URL is invalid or incomplete, copy it again from your other client"
                               (nth 1 warning)))
       (should (string-match-p "Invalid vfile signature" (nth 1 warning))))
     ;; url.el must not prompt for a user and password on 401
     (should (plist-get (car (org-social-vfile-test--requests-with "POST")) :noninteractive))
     (should (eq (plist-get (org-social-file--state local-path) :status) 'failed))
     (should (equal org-social-vfile-test--remote org-social-vfile-test--feed))
     ;; The mode line flags the failure
     (find-file local-path)
     (org-social-mode 1)
     (should org-social-mode)
     (should (string-match-p "upload failed" (org-social-file--mode-line-lighter)))
     ;; Saving again, even without changes, retries the upload
     (setq org-social-vfile-test--upload-reply nil)
     (org-social-file--save)
     (should (equal org-social-vfile-test--remote edited))
     (should-not (plist-get (org-social-file--state local-path) :status))
     (should (equal (org-social-file--mode-line-lighter) " OrgSocial")))))

(ert-deftest org-social-vfile-test-unreachable-host-warns ()
  "A network failure is shown with `display-warning', not `message'."
  (org-social-vfile-test--with-host
   (org-social-file--refresh-vfile-cache)
   (org-social-vfile-test--edit-locally (concat org-social-vfile-test--feed "\nEdited\n"))
   (cl-letf (((symbol-function 'org-social-vfile-test--serve) (lambda (_url) "")))
     (org-social-file--upload-vfile org-social-file (org-social-vfile-test--local)))
   (should (eq (nth 2 (car org-social-vfile-test--warnings)) :error))
   (should (eq (plist-get (org-social-file--state (org-social-vfile-test--local)) :status)
               'failed))))

(ert-deftest org-social-vfile-test-force-upload ()
  "`org-social-force-upload' overwrites the host after confirmation."
  (org-social-vfile-test--with-host
   (org-social-file--refresh-vfile-cache)
   (let ((android (concat org-social-vfile-test--feed org-social-vfile-test--android-post))
         (edited (concat org-social-vfile-test--feed "\nEdited\n")))
     (setq org-social-vfile-test--remote android)
     (org-social-vfile-test--edit-locally edited)
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) nil)))
       (should-error (org-social-force-upload) :type 'user-error))
     (should (equal org-social-vfile-test--remote android))
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
       (org-social-force-upload))
     (should (equal org-social-vfile-test--remote edited))
     (should-not (assoc "If-Match" (plist-get (car (org-social-vfile-test--requests-with "POST"))
                                              :headers))))))

(ert-deftest org-social-vfile-test-401-skips-url-auth ()
  "The package requests never ask url.el for credentials."
  (with-temp-buffer
    (setq org-social-file--skip-http-auth t)
    (should (eq (org-social-file--skip-http-auth (lambda (&rest _) (error "Prompted")) nil)
                t)))
  (with-temp-buffer
    (should (eq (org-social-file--skip-http-auth (lambda (&rest _) 'asked) nil)
                'asked))))

;;; vfile URL validation

(ert-deftest org-social-vfile-test-url-validation ()
  "Valid vfile URLs pass; truncated ones are explained."
  (should-not (org-social-file--vfile-url-problem org-social-vfile-test--vfile))
  (should (org-social-file--vfile-url-problem
           (substring org-social-vfile-test--vfile 0 -10)))
  (should (org-social-file--vfile-url-problem
           (car (split-string org-social-vfile-test--vfile "&sig="))))
  (should (org-social-file--vfile-url-problem "https://host.example/test/social.org"))
  (should (org-social-file--schemeless-url-p
           (substring org-social-vfile-test--vfile (length "https://"))))
  (should (org-social-file--schemeless-url-p "host.example/test/social.org"))
  (should-not (org-social-file--schemeless-url-p "~/social.org"))
  (should-not (org-social-file--schemeless-url-p "/home/me/social.org"))
  (should-not (org-social-file--schemeless-url-p "social.org")))

(ert-deftest org-social-vfile-test-bad-vfile-url-is-a-user-error ()
  "A vfile URL without scheme or truncated signals a `user-error'."
  (org-social-vfile-test--with-host
   (let ((default-directory user-emacs-directory))
     (dolist (bad (list (substring org-social-vfile-test--vfile (length "https://"))
                        (substring org-social-vfile-test--vfile 0 -10)))
       (let ((org-social-file bad))
         (should-error (org-social-file--open) :type 'user-error)
         (should-error (org-social-file--ensure-open-and-fresh) :type 'user-error)
         (should-error (org-social-file--edit-post "2025-05-01T10:00:00+0100")
                       :type 'user-error)))
     (should-not (directory-files user-emacs-directory nil "\\`[^.]"))
     (should-not org-social-vfile-test--requests))))

(ert-deftest org-social-vfile-test-upload-with-truncated-vfile-warns ()
  "Saving with a truncated vfile URL explains it instead of failing silently."
  (org-social-vfile-test--with-host
   (org-social-file--refresh-vfile-cache)
   (let ((org-social-file (substring org-social-vfile-test--vfile 0 -10)))
     (org-social-file--upload-vfile org-social-file (org-social-vfile-test--local)))
   (should (string-match-p "invalid or incomplete" (nth 1 (car org-social-vfile-test--warnings))))
   (should-not (org-social-vfile-test--requests-with "POST"))))

;;; Read before write

(ert-deftest org-social-vfile-test-edit-post-downloads-first ()
  "Editing a post downloads the feed before touching the local copy."
  (org-social-vfile-test--with-host
   (org-social-file--refresh-vfile-cache)
   (setq org-social-vfile-test--remote
         (concat org-social-vfile-test--feed org-social-vfile-test--android-post))
   (setq org-social-vfile-test--requests nil)
   (org-social-file--edit-post "2025-05-02T10:00:00+0100")
   (should (org-social-vfile-test--requests-with "GET"))
   (should (equal (buffer-file-name) (org-social-vfile-test--local)))
   (should (looking-at-p "Post from Android"))
   (should (equal (buffer-string) org-social-vfile-test--remote))))

(ert-deftest org-social-vfile-test-poll-vote-downloads-first ()
  "Voting downloads the feed first, so the Android post survives."
  (org-social-vfile-test--with-host
   (org-social-file--refresh-vfile-cache)
   (let ((android (concat org-social-vfile-test--feed org-social-vfile-test--android-post)))
     (setq org-social-vfile-test--remote android)
     (with-temp-buffer
       (org-social-ui--create-poll-vote "https://alice.example/social.org"
                                        "2025-05-01T09:00:00+0100" "Yes"))
     (should (string-prefix-p android org-social-vfile-test--remote))
     (should (string-match-p ":POLL_OPTION: Yes" org-social-vfile-test--remote))
     ;; A single upload, even with `org-social-mode' hooks installed
     (should (= (length (org-social-vfile-test--requests-with "POST")) 1))
     (should-not (org-social-vfile-test--conflict-files)))))

(ert-deftest org-social-vfile-test-new-post-downloads-first ()
  "Creating a post keeps posts published elsewhere."
  (org-social-vfile-test--with-host
   (org-social-file--refresh-vfile-cache)
   (let ((android (concat org-social-vfile-test--feed org-social-vfile-test--android-post)))
     (setq org-social-vfile-test--remote android)
     (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "public")))
       (org-social-file--new-post))
     (insert "New post from Emacs")
     (save-buffer)
     (should (string-prefix-p android org-social-vfile-test--remote))
     (should (string-match-p "New post from Emacs" org-social-vfile-test--remote)))))

;;; Unsaved and unuploaded changes

(defun org-social-vfile-test--open-with-unsaved-edit ()
  "Open the local copy with an unsaved edit and return its buffer."
  (org-social-file--refresh-vfile-cache)
  (find-file (org-social-vfile-test--local))
  (org-social-mode 1)
  (goto-char (point-max))
  (insert "\nUnsaved draft\n")
  (current-buffer))

(ert-deftest org-social-vfile-test-unsaved-changes-abort ()
  "Reloading with unsaved changes can be aborted without losing them."
  (org-social-vfile-test--with-host
   (let ((buf (org-social-vfile-test--open-with-unsaved-edit)))
     (setq org-social-vfile-test--remote
           (concat org-social-vfile-test--feed org-social-vfile-test--android-post))
     (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) '(?a "abort"))))
       (should-error (org-social-file--ensure-open-and-fresh) :type 'user-error))
     (with-current-buffer buf
       (should (buffer-modified-p))
       (should (string-match-p "Unsaved draft" (buffer-string)))))))

(ert-deftest org-social-vfile-test-unsaved-changes-save ()
  "Choosing to save uploads the unsaved changes before reloading."
  (org-social-vfile-test--with-host
   (let ((buf (org-social-vfile-test--open-with-unsaved-edit)))
     (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) '(?s "save"))))
       (org-social-file--ensure-open-and-fresh))
     (with-current-buffer buf
       (should-not (buffer-modified-p))
       (should (string-match-p "Unsaved draft" (buffer-string))))
     (should (string-match-p "Unsaved draft" org-social-vfile-test--remote)))))

(ert-deftest org-social-vfile-test-unsaved-changes-and-remote-change ()
  "Unsaved changes plus a remote post end in a conflict, not in a loss."
  (org-social-vfile-test--with-host
   (let ((buf (org-social-vfile-test--open-with-unsaved-edit))
         (android (concat org-social-vfile-test--feed org-social-vfile-test--android-post)))
     (setq org-social-vfile-test--remote android)
     (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) '(?s "save"))))
       (org-social-file--ensure-open-and-fresh))
     (should (equal org-social-vfile-test--remote android))
     (with-current-buffer buf
       (should (equal (buffer-string) android)))
     (let ((conflicts (org-social-vfile-test--conflict-files)))
       (should (= (length conflicts) 1))
       (should (string-match-p "Unsaved draft"
                               (org-social-vfile-test--read (car conflicts))))))))

(ert-deftest org-social-vfile-test-unuploaded-changes-survive-refresh ()
  "A saved change whose upload failed is not overwritten by a download."
  (org-social-vfile-test--with-host
   (org-social-file--refresh-vfile-cache)
   (let ((edited (concat org-social-vfile-test--feed "\nSaved offline\n")))
     (org-social-vfile-test--edit-locally edited)
     (cl-letf (((symbol-function 'org-social-vfile-test--serve) (lambda (_url) "")))
       (org-social-file--upload-vfile org-social-file (org-social-vfile-test--local)))
     (should (eq (org-social-file--refresh-vfile-cache) 'local-ahead))
     (should (equal (org-social-vfile-test--read (org-social-vfile-test--local)) edited))
     ;; The pending change was uploaded once the host was back
     (should (equal org-social-vfile-test--remote edited)))))

(ert-deftest org-social-vfile-test-edit-during-download-is-kept ()
  "Typing while the feed downloads ends in a conflict, not in a loss."
  (org-social-vfile-test--with-host
   (let ((buf (org-social-vfile-test--open-with-unsaved-edit))
         (android (concat org-social-vfile-test--feed org-social-vfile-test--android-post)))
     (setq org-social-vfile-test--remote android)
     ;; The buffer was modified after `org-social-file--resolve-unsaved-changes'
     (should (eq (org-social-file--apply-remote (org-social-vfile-test--local)
                                                android
                                                (org-social-vfile-test--etag android))
                 'conflict))
     (with-current-buffer buf
       (should (equal (buffer-string) android)))
     (should (string-match-p "Unsaved draft"
                             (org-social-vfile-test--read
                              (car (org-social-vfile-test--conflict-files))))))))

;;; Package actions retry on conflicts

(defun org-social-vfile-test--412-once (new-remote)
  "Return an upload reply answering 412 once, after setting NEW-REMOTE.
Later uploads are handled normally."
  (let ((done nil))
    (lambda ()
      (unless done
        (setq done t
              org-social-vfile-test--remote new-remote)
        (org-social-vfile-test--http 412 nil "{\"errors\": [\"Precondition Failed\"]}")))))

(defun org-social-vfile-test--412-always ()
  "Return an upload reply that always answers 412."
  (lambda ()
    (org-social-vfile-test--http 412 nil "{\"errors\": [\"Precondition Failed\"]}")))

(defun org-social-vfile-test--count-ids (content id)
  "Count the posts of CONTENT whose ID is ID."
  (seq-count (lambda (x) (equal x id)) (org-social-file--post-ids content)))

(ert-deftest org-social-vfile-test-timestamp-add-second ()
  "IDs move forward one second, keeping their time zone format."
  (should (equal (org-social-file--timestamp-add-second "2025-05-01T10:00:59+01:00")
                 "2025-05-01T10:01:00+01:00"))
  (should (equal (org-social-file--timestamp-add-second "2025-05-01T23:59:59+0100")
                 "2025-05-02T00:00:00+0100"))
  (should (equal (org-social-file--timestamp-add-second "2025-05-01T10:00:00-05:30")
                 "2025-05-01T10:00:01-05:30"))
  (should (equal (org-social-file--timestamp-add-second "2025-05-01T10:00:00Z")
                 "2025-05-01T10:00:01Z"))
  (should (equal (org-social-file--unique-timestamp
                  "2025-05-01T10:00:00+01:00"
                  '("2025-05-01T10:00:00+01:00" "2025-05-01T10:00:01+01:00"))
                 "2025-05-01T10:00:02+01:00")))

(ert-deftest org-social-vfile-test-reaction-retries-after-412 ()
  "A reaction whose upload gets 412 is added again on top of the new feed."
  (org-social-vfile-test--with-host
   (org-social-file--refresh-vfile-cache)
   (let ((android (concat org-social-vfile-test--feed org-social-vfile-test--android-post)))
     (org-social-file--new-reaction "https://alice.example/social.org"
                                    "2025-05-01T09:00:00+0100" "👍")
     (setq org-social-vfile-test--upload-reply (org-social-vfile-test--412-once android))
     (save-buffer)
     (should (= (length (org-social-vfile-test--requests-with "POST")) 2))
     (should (string-match-p "Post from Android" org-social-vfile-test--remote))
     (should (string-match-p ":MOOD: 👍" org-social-vfile-test--remote))
     (should (string-match-p "First post from Emacs" org-social-vfile-test--remote))
     ;; The second upload used the ETag of the new feed
     (should (equal (cdr (assoc "If-Match" (plist-get (car (last (org-social-vfile-test--requests-with "POST")))
                                                      :headers)))
                    (org-social-vfile-test--etag android)))
     (should (equal org-social-vfile-test--retry-delays '(0.3)))
     (should-not org-social-vfile-test--ediffs)
     (should-not org-social-vfile-test--warnings)
     (should-not (org-social-vfile-test--conflict-files))
     ;; The buffer shows what was uploaded
     (should (equal (buffer-string) org-social-vfile-test--remote))
     (should-not (buffer-modified-p)))))

(ert-deftest org-social-vfile-test-vote-retries-after-412 ()
  "A vote whose upload gets 412 is applied again on the new feed."
  (org-social-vfile-test--with-host
   (org-social-file--refresh-vfile-cache)
   (let ((android (concat org-social-vfile-test--feed org-social-vfile-test--android-post))
         (done nil))
     (setq org-social-vfile-test--upload-reply (org-social-vfile-test--412-once android))
     (org-social-file--apply-action
      (lambda (content)
        (org-social-file--append-new-post
         content (lambda (id) (format "** %s\n:PROPERTIES:\n:POLL_OPTION: Yes\n:END:\n" id))))
      (lambda (content) (setq done content)))
     (should (= (length (org-social-vfile-test--requests-with "POST")) 2))
     (should (equal org-social-vfile-test--retry-delays '(0.3)))
     (should (string-prefix-p (string-trim-right android) org-social-vfile-test--remote))
     (should (string-match-p ":POLL_OPTION: Yes" org-social-vfile-test--remote))
     (should (equal done org-social-vfile-test--remote))
     (should (equal (org-social-vfile-test--read (org-social-vfile-test--local))
                    org-social-vfile-test--remote))
     (should-not org-social-vfile-test--ediffs))))

(ert-deftest org-social-vfile-test-new-post-same-second-gets-new-id ()
  "A post published elsewhere in the same second does not share our ID."
  (org-social-vfile-test--with-host
   (org-social-file--refresh-vfile-cache)
   (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "public")))
     (org-social-file--new-post))
   (insert "My post from Emacs")
   (let* ((id (car (plist-get (org-social-file--state (org-social-vfile-test--local))
                              :inserted-ids)))
          (other (format "\n** %s\n:PROPERTIES:\n:CLIENT: android\n:END:\n\nSame second on Android\n"
                         id)))
     (should id)
     ;; Another client publishes with the same ID before we save
     (setq org-social-vfile-test--remote (concat org-social-vfile-test--feed other))
     (save-buffer)
     (let ((remote org-social-vfile-test--remote))
       (should (string-match-p "Same second on Android" remote))
       (should (string-match-p "My post from Emacs" remote))
       (should (= (org-social-vfile-test--count-ids remote id) 1))
       (should (= (org-social-vfile-test--count-ids
                   remote (org-social-file--timestamp-add-second id))
                  1))
       (should (= (length (org-social-file--post-ids remote))
                  (length (delete-dups (org-social-file--post-ids remote))))))
     (should-not org-social-vfile-test--ediffs)
     (should-not (org-social-vfile-test--conflict-files)))))

(ert-deftest org-social-vfile-test-three-412-is-a-conflict ()
  "After three refused uploads, the action ends in the conflict flow."
  (org-social-vfile-test--with-host
   (org-social-file--refresh-vfile-cache)
   (org-social-file--new-reaction "https://alice.example/social.org"
                                  "2025-05-01T09:00:00+0100" "👍")
   (setq org-social-vfile-test--upload-reply (org-social-vfile-test--412-always))
   (save-buffer)
   (should (= (length (org-social-vfile-test--requests-with "POST")) 3))
   (should (equal org-social-vfile-test--retry-delays '(1.0 0.3)))
   (should (equal org-social-vfile-test--remote org-social-vfile-test--feed))
   (let ((conflicts (org-social-vfile-test--conflict-files)))
     (should (= (length conflicts) 1))
     (should (string-match-p ":MOOD: 👍" (org-social-vfile-test--read (car conflicts)))))
   (should (equal (org-social-vfile-test--read (org-social-vfile-test--local))
                  org-social-vfile-test--feed))
   (should org-social-vfile-test--ediffs)
   (should (eq (nth 2 (car org-social-vfile-test--warnings)) :error))))

(ert-deftest org-social-vfile-test-vote-three-412-is-a-conflict ()
  "A vote refused three times keeps the attempted version as conflict copy."
  (org-social-vfile-test--with-host
   (org-social-file--refresh-vfile-cache)
   (setq org-social-vfile-test--upload-reply (org-social-vfile-test--412-always))
   (org-social-ui--create-poll-vote "https://alice.example/social.org"
                                    "2025-05-01T09:00:00+0100" "Yes")
   (should (= (length (org-social-vfile-test--requests-with "POST")) 3))
   (should (equal org-social-vfile-test--retry-delays '(1.0 0.3)))
   (should (string-match-p ":POLL_OPTION: Yes"
                           (org-social-vfile-test--read
                            (car (org-social-vfile-test--conflict-files)))))
   (should org-social-vfile-test--ediffs)))

(ert-deftest org-social-vfile-test-manual-save-412-opens-ediff ()
  "A file edited by hand is not retried: a 412 goes straight to ediff."
  (org-social-vfile-test--with-host
   (org-social-file--refresh-vfile-cache)
   (find-file (org-social-vfile-test--local))
   (org-social-mode 1)
   (goto-char (point-min))
   (search-forward "First post from Emacs")
   (replace-match "First post, edited by hand")
   (let ((android (concat org-social-vfile-test--feed org-social-vfile-test--android-post)))
     (setq org-social-vfile-test--upload-reply (org-social-vfile-test--412-once android))
     (save-buffer)
     (should (= (length (org-social-vfile-test--requests-with "POST")) 1))
     (should-not org-social-vfile-test--retry-delays)
     (org-social-vfile-test--assert-conflict
      (replace-regexp-in-string "First post from Emacs" "First post, edited by hand"
                                org-social-vfile-test--feed)
      android))))

(ert-deftest org-social-vfile-test-new-post-plus-manual-edit-opens-ediff ()
  "A new post plus a manual edit elsewhere is a conflict, not a retry."
  (org-social-vfile-test--with-host
   (org-social-file--refresh-vfile-cache)
   (org-social-file--new-reaction "https://alice.example/social.org"
                                  "2025-05-01T09:00:00+0100" "👍")
   (goto-char (point-min))
   (search-forward "First post from Emacs")
   (replace-match "First post, edited by hand")
   (setq org-social-vfile-test--remote
         (concat org-social-vfile-test--feed org-social-vfile-test--android-post))
   (save-buffer)
   (should-not (org-social-vfile-test--requests-with "POST"))
   (should (= (length (org-social-vfile-test--conflict-files)) 1))))

(ert-deftest org-social-vfile-test-action-with-unsaved-buffer-is-refused ()
  "A package action never mixes with unsaved manual edits."
  (org-social-vfile-test--with-host
   (let ((buf (org-social-vfile-test--open-with-unsaved-edit)))
     (setq org-social-vfile-test--requests nil)
     (with-temp-buffer
       (should-error (org-social-ui--create-poll-vote "https://alice.example/social.org"
                                                      "2025-05-01T09:00:00+0100" "Yes")
                     :type 'user-error))
     (should-not org-social-vfile-test--requests)
     (should (equal org-social-vfile-test--remote org-social-vfile-test--feed))
     (with-current-buffer buf
       (should (buffer-modified-p))
       (should (string-match-p "Unsaved draft" (buffer-string)))
       (should-not (string-match-p "POLL_OPTION" (buffer-string)))))))

(ert-deftest org-social-vfile-test-follow-and-unfollow ()
  "Following and unfollowing change only the FOLLOW line of the latest feed."
  (org-social-vfile-test--with-host
   (org-social-file--refresh-vfile-cache)
   (let ((android (concat org-social-vfile-test--feed org-social-vfile-test--android-post)))
     (setq org-social-vfile-test--remote android)
     (cl-letf (((symbol-function 'org-social-ui-discover) #'ignore))
       (org-social-ui--follow-user "https://bob.example/social.org" "bob")
       (should (string-match-p "^#\\+FOLLOW: bob https://bob.example/social.org\n\\(?:.\\|\n\\)*\\* Posts"
                               org-social-vfile-test--remote))
       (should (string-match-p "Post from Android" org-social-vfile-test--remote))
       (org-social-ui--unfollow-user "https://bob.example/social.org" "bob"))
     (should (equal org-social-vfile-test--remote android)))))

(provide 'org-social-vfile-test)
;;; org-social-vfile-test.el ends here
