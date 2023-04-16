;;; tabnine.el --- An unofficial TabNine package for Emacs ;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2023 Tommy Xiang, John Gong, Aaron Ji
;;
;; Author: Tommy Xiang <tommyx058@gmail.com>
;;         John Gong <gjtzone@hotmail.com>
;;         Aaron Ji <shuxiao9058@gmail.com>
;; Keywords: convenience
;; Version: 0.0.1
;; URL: https://github.com/shuxiao9058/tabnine/
;; Package-Requires: ((emacs "25") (cl-lib "0.5") (dash "2.16.0") (s "1.12.0") (editorconfig "0.9.1"))
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;; Commentary:
;;
;; Description:
;;
;; An overlay verison of `tabnine` Emacs package.
;;
;; Installation:
;;
;; 1. Enable `tabnine-mode` in `prog-mode`.
;; (add-to-list 'prog-mode-hook #'tabnine-mode)
;; 2. Run M-x tabnine-install-binary to install the TabNine binary for your system.
;;
;; Usage:
;;
;; See M-x customize-group RET tabnine RET for customizations.
;;
;;

;;; Code:

;;
;; Dependencies
;;

(require 'cl-lib)
(require 'dash)
(require 'json)
(require 's)
(require 'url)

(require 'editorconfig)

;;
;; Constants
;;

(defconst tabnine--process-name "tabnine")
(defconst tabnine--buffer-name "*tabnine-log*")
(defconst tabnine--hooks-alist nil)
(defconst tabnine--api-version "4.4.223"
  "TabNine api version.")

(defconst tabnine-version "0.0.1"
  "TabNine version.")

;; tmp file put in tabnine-binaries-folder directory
(defconst tabnine--version-tempfile "version")

(defconst tabnine--indentation-alist
  (append '((latex-mode tex-indent-basic)
            (nxml-mode nxml-child-indent)
            (python-mode python-indent py-indent-offset python-indent-offset)
            (python-ts-mode python-indent py-indent-offset python-indent-offset)
            (web-mode web-mode-markup-indent-offset web-mode-html-offset))
          editorconfig-indentation-alist)
  "Alist of `major-mode' to indentation map with optional fallbacks.")

;;
;; Macros
;;

(eval-and-compile
  (defun tabnine--transform-pattern (pattern)
    "Transform PATTERN to (&plist PATTERN) recursively."
    (cons '&plist
          (mapcar (lambda (p)
                    (if (listp p)
                        (tabnine--transform-pattern p)
                      p))
                  pattern))))

(defmacro tabnine--dbind (pattern source &rest body)
  "Destructure SOURCE against plist PATTERN and eval BODY."
  (declare (indent 2))
  `(-let ((,(tabnine--transform-pattern pattern) ,source))
     ,@body))

;;
;; Customization
;;

(defgroup tabnine nil
  "Options for tabnine."
  :link '(url-link :tag "Github" "https://github.com/shuxiao9058/tabnine")
  :group 'company
  :prefix "tabnine-")

(defcustom tabnine-idle-delay 0.1
  "Time in seconds to wait before starting completion.  Complete immediately if set to 0."
  :type 'float
  :group 'tabnine)

(defcustom tabnine-max-num-results 5
  "Maximum number of results to show."
  :group 'tabnine
  :type 'integer)

(defcustom tabnine-context-char-limit-before 100000
  "The number of chars before point to send for completion."
  :group 'tabnine
  :type 'integer)

(defcustom tabnine-context-char-limit-after 100000
  "The number of chars after point to send for completion."
  :group 'tabnine
  :type 'integer)

(defcustom tabnine-minimum-prefix-length 3
  "The minimum prefix length for idle completion."
  :group 'tabnine
  :type 'integer)


(defcustom tabnine-clear-overlay-ignore-commands nil
  "List of commands that should not clear the overlay when called."
  :group 'tabnine
  :type '(repeat function))

(defcustom tabnine-max-consecutive-restart-count 10
  "Maximum number of times TabNine can consecutively restart.
This may be due to errors in or automatic server updates.
Any successful completion will reset the consecutive count."
  :group 'tabnine
  :type 'integer)

(defcustom tabnine-wait 1.0
  "Number of seconds to wait for TabNine to respond."
  :group 'tabnine
  :type 'float)

(defcustom tabnine-binaries-folder "~/.TabNine"
  "Path to TabNine binaries folder.
`tabnine-install-binary' will use this directory."
  :group 'tabnine
  :type 'string)

(defcustom tabnine-install-static-binary (file-exists-p "/etc/nixos/hardware-configuration.nix")
  "Whether to install the musl-linked static binary instead of the standard glibc-linked dynamic binary.
Only useful on GNU/Linux.  Automatically set if NixOS is detected."
  :group 'tabnine
  :type 'boolean)

(defcustom tabnine-log-file-path nil
  "If non-nil, next TabNine restart will write debug log to this path."
  :group 'tabnine
  :type 'string)

(defcustom tabnine-auto-balance t
  "Whether TabNine should insert balanced parentheses upon completion."
  :group 'tabnine
  :type 'boolean)

;;
;; Faces
;;

;;
;; Variables
;;


(defvar tabnine-executable-args nil
  "Arguments passed to TabNine.")

(defvar tabnine--process nil
  "TabNine server process.")

(defvar tabnine--consecutive-restart-count 0
  "Number of times TabNine server has restarted abnormally.
Resets every time successful completion is returned.")

(defvar tabnine--response nil
  "Temporarily stored TabNine server responses.")

(defvar tabnine--disabled nil
  "Variable to temporarily disable tabnine and pass control to next backend.")

(defvar tabnine--calling-continue nil
  "Flag for when `company-continue' is being called.")

(defvar tabnine--response-chunks nil
  "The string to store response chunks from TabNine server.")


(defvar-local tabnine--overlay nil
  "Overlay for TabNine completion.")

(defvar-local tabnine--completion-cache nil)
(defvar-local tabnine--completion-idx 0)


(defvar-local tabnine--line-bias 1
  "Line bias for TabNine completion.")


(defvar tabnine--post-command-timer nil)
(defvar-local tabnine--buffer-changed nil
  "Non nil if buffer has changed since last time `tabnine-complete' has been invoked.")

(defun tabnine--buffer-changed-p ()
  tabnine--buffer-changed)


(defmacro tabnine--json-serialize (params)
  (if (progn
        (require 'json)
        (fboundp 'json-serialize))
      `(json-serialize ,params
                       :null-object nil
                       :false-object :json-false)
    `(let ((json-false :json-false))
       (json-encode ,params))))


(defmacro tabnine--read-json (str)
  "Read json string STR.  and return the decoded object."
  (if (progn
        (require 'json)
        (fboundp 'json-parse-string))
      `(json-parse-string ,str
                          :array-type 'array
                          :object-type 'plist
                          :null-object nil
                          :false-object :json-false)
    `(let ((json-array-type 'vector)
           (json-object-type 'plist)
           (json-false nil))
       (json-read-from-string ,str))))

;;
;; Agent process
;;

(defsubst tabnine--process-alivep ()
  "Non-nil if the `tabnine--process' is alive."
  (and tabnine--process
       (process-live-p tabnine--process)))

(defmacro tabnine--send-request (request)
  "Send a REQUEST to the TabNine process.
  REQUEST needs to be JSON-serializable object."
  `(progn
     (unless (tabnine--process-alivep)
       (tabnine-start-process))

     (when tabnine--process
       ;; TODO make sure utf-8 encoding works
       (let ((encoded (concat
                       (tabnine--json-serialize request)
                       "\n")))
	 (setq tabnine--response nil)
	 (tabnine--log-to-buffer "Write to TabNine process" encoded)
	 (process-send-string tabnine--process encoded)
	 (accept-process-output tabnine--process tabnine-wait)))))

(defun tabnine--make-request (method)
  "Create request body for method METHOD and parameters PARAMS."
  (cond
   ((eq method 'autocomplete)
    (let* ((buffer-min 1)
           (buffer-max (1+ (buffer-size)))
           (before-point
            (max (point-min) (- (point) tabnine-context-char-limit-before)))
           (after-point
            (min (point-max) (+ (point) tabnine-context-char-limit-after)))
	   (offset (max 0 (1- (point))))
	   (line (- (line-number-at-pos) 1))
	   (character (max 0 (- (point) (line-beginning-position))))
	   (indentation_size (tabnine--infer-indentation-offset)))

      (list
       :version tabnine--api-version
       :request
       (list :Autocomplete
             (list
              :before (buffer-substring-no-properties before-point (point))
              :after (buffer-substring-no-properties (point) after-point)
              :filename (or (buffer-file-name) nil)
              :region_includes_beginning (if (= before-point buffer-min)
                                             t json-false)
              :region_includes_end (if (= after-point buffer-max)
				       t json-false)
              :max_num_results tabnine-max-num-results
              :offset offset
              :line line
              :character character
              :indentation_size indentation_size
	      )))))
   ((eq method 'prefetch)
    (list
     :version tabnine--api-version
     :request
     (list :Prefetch
           (list
            :filename (or (buffer-file-name) nil)))))
   ((eq method 'getidentifierregex)
    (list
     :version tabnine--api-version
     :request
     (list :GetIdentifierRegex
           (list
            :filename (or (buffer-file-name) nil)
            ))))
   ;; Query Capabilities
   ((eq method 'capabilities)
    (list
     :version tabnine--api-version
     :request
     (list :Features '(:__ t))))
   ((eq method 'state)
    (list
     :version tabnine--api-version
     :request
     (list :State '(:__ t))))
   ((eq method 'configuration)
    (list
     :version tabnine--api-version
     :request
     (list :Configuration '(:__ t))))
   ;; SaveSnippet
   ;; NotifyWorkspaceChanged
   ;; setState
   ;; StartupActions
   ;; HoverAction
   ))


(defun tabnine--error-no-binaries ()
  "Signal error for when TabNine binary is not found."
  (error "No TabNine binaries found.  Run M-x tabnine-install-binary to download binaries"))

(defun tabnine--get-target ()
  "Return TabNine's system configuration.  Used for finding the correct binary."
  (let* ((system-architecture (car (s-split "-" system-configuration)))
         (tabnine-architecture
          (cond
           ((or (string= system-architecture "aarch64")
                (and (eq system-type 'darwin)
                     (string= system-architecture "x86_64")
                     ;; Detect AArch64 running x86_64 Emacs
                     (string= (shell-command-to-string "arch -arm64 uname -m") "arm64\n")))
            "aarch64")
           ((or (string= system-architecture "arm")
                (and (eq system-type 'darwin)
                     (string= system-architecture "x86_64")
                     ;; Detect AArch64 running x86_64 Emacs
                     (string= (shell-command-to-string "arch -arm64 uname -m") "arm64\n")))
            "aarch64")
           ((string= system-architecture "x86_64")
            "x86_64")
           ((string-match system-architecture "i.86")
            "i686")
           (t
            (error "Unknown or unsupported architecture %s" system-architecture))))

         (os
          (cond
           ((or (eq system-type 'ms-dos)
                (eq system-type 'windows-nt)
                (eq system-type 'cygwin))
            "pc-windows-gnu")
           ((or (eq system-type 'darwin))
            "apple-darwin")
           (tabnine-install-static-binary
            "unknown-linux-musl")
           (t
            "unknown-linux-gnu"))))

    (concat tabnine-architecture "-" os)))

(defun tabnine--get-exe ()
  "Return TabNine's binary file name.  Used for finding the correct binary."
  (if (or (eq system-type 'ms-dos)
          (eq system-type 'windows-nt)
          (eq system-type 'cygwin))
      "TabNine.exe"
    "TabNine"
    ))

(defun tabnine--executable-path ()
  "Find and return the path of the latest TabNine binary for the current system."
  (let ((parent tabnine-binaries-folder))
    (if (file-directory-p parent)
        (let* ((children (->> (directory-files parent)
			      (--remove (member it '("." "..")))
			      (--filter (file-directory-p
                                         (expand-file-name
                                          it
                                          (file-name-as-directory
                                           parent))))
			      (--filter (ignore-errors (version-to-list it)))
			      (-non-nil)))
               (sorted (nreverse (sort children #'version<)))
               (target (tabnine--get-target))
               (filename (tabnine--get-exe)))
          (cl-loop
           for ver in sorted
           for fullpath = (expand-file-name (format "%s/%s/%s"
                                                    ver target filename)
                                            parent)
           if (and (file-exists-p fullpath)
                   (file-regular-p fullpath))
           return fullpath
           finally do (tabnine--error-no-binaries)))
      (tabnine--error-no-binaries))))

(defun tabnine-start-process ()
  "Start TabNine process."
  (tabnine-kill-process)
  (let ((process-connection-type nil))
    (setq tabnine--process
          ;; TODO make process with env (proxy support)
          (make-process
           :name tabnine--process-name
           :buffer tabnine--buffer-name
	   :command (append
                     (cons (tabnine--executable-path)
			   (list "--client" "emacs"))
		     tabnine-executable-args
		     (when tabnine-log-file-path
		       (list
			"--log-file-path"
			(expand-file-name
			 (format "%s-%d" tabnine-log-file-path (emacs-pid)))))
		     (list "--client-metadata"
			   (format "clientVersion=%s" emacs-version)
			   (format "pluginVersion=%s" tabnine-version)
			   (format "ide-restart-counter=%d" tabnine--consecutive-restart-count)))
	   :coding 'utf-8-emacs-unix
	   :connection-type 'pipe
	   :filter #'tabnine--process-filter
	   :sentinel #'tabnine--process-sentinel
	   :noquery t
	   :stderr (get-buffer-create
                    (format "*%s stderr*" tabnine--process-name))
	   )))
  ;; hook setup
  (message "TabNine server started.")
  (tabnine-capabilities)
  (dolist (hook tabnine--hooks-alist)
    (add-hook (car hook) (cdr hook))))

(defun tabnine-kill-process ()
  "Kill TabNine process."
  (interactive)
  (when tabnine--process
    (let ((process tabnine--process))
      (setq tabnine--process nil) ; this happens first so sentinel don't catch the kill
      (delete-process process)))
  ;; hook remove
  (dolist (hook tabnine--hooks-alist)
    (remove-hook (car hook) (cdr hook))))



;;
;; TabNine all operations
;;

(defun tabnine-autocomplete ()
  "Query TabNine server for auto-complete."
  (let ((request (tabnine--make-request 'autocomplete)))
    (tabnine--send-request request)))

(defun tabnine-prefetch ()
  "Prefetch buffer."
  (let ((request (tabnine--make-request 'prefetch)))
    (tabnine--send-request request)))

(defun tabnine-capabilities ()
  "Query TabNine server Capabilities."
  (let ((request (tabnine--make-request 'capabilities)))
    (tabnine--send-request request)))

(defun tabnine-state ()
  "Query TabNine Service State."
  (let ((request (tabnine--make-request 'state)))
    (tabnine--send-request request)))

(defun tabnine-getidentifierregex ()
  "Get identifier regex from TabNine server."
  (let ((request (tabnine--make-request 'getidentifierregex)))
    (tabnine--send-request request)))

(defun tabnine-configuration ()
  "Get identifier regex from TabNine server."
  (interactive)
  (let ((request (tabnine--make-request 'configuration)))
    (tabnine--send-request request)))


;;
;; Major mode definition
;;

;;
;; Auto completion
;;

(defun tabnine--prefix-candidate-p (candidate prefix)
  "Return t if CANDIDATE string begins with PREFIX."
  (let ((insertion-text (cdr (assq 'insertion_text candidate))))
    (s-starts-with? prefix insertion-text t)))

(defun tabnine--log-to-buffer(prefix text)
  "Log to TabNine debug buffer, PREFIX is log prefix and TEXT is the log body."
  (when tabnine--buffer-name
    (with-current-buffer (get-buffer-create tabnine--buffer-name)
      (goto-char (point-max))
      (insert (format "====== %s START %s ====== \n%s ====== END ======\n" (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)) prefix text)))))


(defun tabnine--infer-indentation-offset ()
  "Infer indentation offset."
  (or (let ((mode major-mode))
        (while (and (not (assq mode tabnine--indentation-alist))
                    (setq mode (get mode 'derived-mode-parent))))
        (when mode
          (cl-some (lambda (s)
                     (when (boundp s)
		       (symbol-value s)))
                   (alist-get mode tabnine--indentation-alist))))
      tab-width))


(defun tabnine--process-sentinel (process event)
  "Sentinel for TabNine server process.
PROCESS is the process under watch, EVENT is the event occurred."
  (when (and tabnine--process
             (memq (process-status process) '(exit signal)))

    (message "TabNine process %s received event %s."
             (prin1-to-string process)
             (prin1-to-string event))

    (if (>= tabnine--consecutive-restart-count
            tabnine-max-consecutive-restart-count)
        (progn
          (message "TabNine process restart limit reached.")
          (setq tabnine--process nil))

      (message "Restarting TabNine process.")
      (tabnine-start-process)
      (setq tabnine--consecutive-restart-count
            (1+ tabnine--consecutive-restart-count)))))

(defun tabnine--valid-response(response)
  "Check if the RESPONSE is valid complition result."
  (when response
    (let* ((results (plist-get response :results))
           (first-completion (if (seq-empty-p results) nil (seq-elt results 0))))
      (and first-completion (s-present? (plist-get first-completion :new_prefix)))
      )))

(defun tabnine--invalid-completion(completion)
  "Check if the COMPLETION is valid, some completions is stupid."
  (when completion
    (let ((new_prefix (plist-get completion :new_prefix))
	  (old_suffix (plist-get completion :old_suffix)))
      (s-equals? new_prefix old_suffix))))

(defun tabnine--filter-completions(completions)
  "Filter duplicates and bad COMPLETIONS result."
  (when completions
    (when-let ((completions (cl-remove-duplicates completions
						  :key (lambda (x) (plist-get x :new_prefix))
						  :test #'s-equals-p))
	       (completions (cl-remove-if #'tabnine--invalid-completion completions)))
      completions)))


(defun tabnine--process-filter (process output)
  "Filter for TabNine server process.
PROCESS is the process under watch, OUTPUT is the output received."
  (tabnine--log-to-buffer "Read from TabNine process" output)
  (push output tabnine--response-chunks)

  (let* ((response
	  (mapconcat #'identity
		     (nreverse tabnine--response-chunks) nil))
         (ss (s-split "\n" (s-trim response)))
         str
	 result)

    (when (and ss (> (length ss) 0))
      (setq tabnine--response-chunks nil)
      (setq str (car ss))
      (when (s-present? str)
        (setq result (tabnine--read-json str))
        (setq ss (cdr ss))
	(when (and result (tabnine--valid-response result))
          (setq tabnine--response result)

	  (let* ((old_prefix (plist-get result :old_prefix))
		 (completions (plist-get result :results))
		 (completions (tabnine--filter-completions completions))
		 (completion (if (seq-empty-p completions) nil (seq-elt completions 0))))

	    (when (and result (> (length completions) 0) )
	      (setq tabnine--completion-cache result)
	      (setq tabnine--completion-idx 0))

	    (when completion
	      (let ((new_prefix (plist-get completion :new_prefix)))
		(when (s-present? new_prefix)
		  (if (s-starts-with? old_prefix  new_prefix)
		      (progn
			(setq new_prefix (s-chop-prefix old_prefix new_prefix))))
		  (tabnine--show-completion old_prefix completion))))))))))


;;
;; Interactive functions
;;

(defun tabnine-restart-server ()
  "Start/Restart TabNine server."
  (interactive)
  (tabnine-start-process))

(defun tabnine-install-binary ()
  "Install TabNine binary into `tabnine-binaries-folder'."
  (interactive)
  (let ((version-tempfile (concat
                           (file-name-as-directory
                            tabnine-binaries-folder)
                           tabnine--version-tempfile))
        (target (tabnine--get-target))
        (exe (tabnine--get-exe))
        (binaries-dir tabnine-binaries-folder))
    (message version-tempfile)
    (message "Getting current version...")
    (make-directory (file-name-directory version-tempfile) t)
    (url-copy-file "https://update.tabnine.com/bundles/version" version-tempfile t)
    (let ((version (s-trim (with-temp-buffer (insert-file-contents version-tempfile) (buffer-string)))))
      (when (= (length version) 0)
        (error "TabNine installation failed.  Please try again"))
      (message "Current version is %s" version)
      (let* ((url (concat "https://update.tabnine.com/bundles/" version "/" target "/TabNine.zip"))
             (version-directory (file-name-as-directory
                                 (concat
                                  (file-name-as-directory
                                   (concat (file-name-as-directory binaries-dir) version)))))
             (target-directory (file-name-as-directory (concat version-directory target) ))
             (bundle-path (concat version-directory (format "%s.zip" target)))
             (target-path (concat target-directory exe)))
        (message "Installing at %s. Downloading %s ..." target-path url)
        (make-directory target-directory t)
        (url-copy-file url bundle-path t)
        (condition-case ex
            (let ((default-directory target-directory))
	      (if (or (eq system-type 'ms-dos)
		      (eq system-type 'windows-nt)
		      (eq system-type 'cygwin))
                  (shell-command (format "tar -xf %s" (expand-file-name bundle-path)))
                (shell-command (format "unzip -o %s -d %s"
				       (expand-file-name bundle-path)
				       (expand-file-name target-directory)))))
          ('error
           (error "Unable to unzip automatically. Please go to [%s] and unzip the content of [%s] into [%s/]."
                  (expand-file-name version-directory)
                  (file-name-nondirectory bundle-path)
                  (file-name-sans-extension (file-name-nondirectory bundle-path)))))
        (mapc (lambda (filename)
                (set-file-modes (concat target-directory filename) (string-to-number "744" 8)))
	      (--remove (member it '("." "..")) (directory-files target-directory)))
        (delete-file bundle-path)
        (delete-file version-tempfile)
        (message "TabNine installation complete.")))))

(defun tabnine--get-completion-result ()
  "Get completions with TabNine."
  (with-timeout (0.01)
    (tabnine-autocomplete))
  tabnine--response)

(defun tabnine--get-completions-cycling (callback)
  "Get completion cycling options with CALLBACK."
  (when tabnine--completion-cache
    (funcall callback tabnine--completion-cache)))

(defun tabnine--cycle-completion (direction)
  "Cycle completion with DIRECTION."
  (lambda (result)
    (unless tabnine--completion-cache
      (setq tabnine--completion-cache result))
    (let* ((cache tabnine--completion-cache)
	   (old_prefix (plist-get cache :old_prefix))
	   (completions (plist-get cache :results))
	   (completions (tabnine--filter-completions completions)))
      (cond ((seq-empty-p completions)
	     (message "No completion is available."))
	    ((= (length completions) 1)
	     (message "Only one completion is available."))
	    (t (let ((idx (mod (+ tabnine--completion-idx direction)
			       (length completions))))
		 (setq tabnine--completion-idx idx)
		 (let ((completion (elt completions idx)))
		   (tabnine--show-completion old_prefix completion))))))))

(defsubst tabnine--overlay-visible ()
  "Return whether the `tabnine--overlay' is avaiable."
  (and (overlayp tabnine--overlay)
       (overlay-buffer tabnine--overlay)))

(defun tabnine-next-completion ()
  "Cycle to next completion."
  (interactive)
  (when (tabnine--overlay-visible)
    (tabnine--get-completions-cycling (tabnine--cycle-completion 1))))

(defun tabnine-previous-completion ()
  "Cycle to previous completion."
  (interactive)
  (when (tabnine--overlay-visible)
    (tabnine--get-completions-cycling (tabnine--cycle-completion -1))))


;;
;; UI
;;


(defun tabnine-current-completion ()
  "Get current completion."
  (and (tabnine--overlay-visible)
       (overlay-get tabnine--overlay 'completion)))

(defface tabnine-overlay-face
  '((t :inherit shadow))
  "Face for TabNine overlay.")

(defvar-local tabnine--real-posn nil
  "Posn information without overlay.
To work around posn problems with after-string property.")

(defconst tabnine-completion-map (make-sparse-keymap)
  "Keymap for TabNine completion overlay.")

(defun tabnine--get-overlay ()
  "Create or get overlay for TabNine."
  (unless (overlayp tabnine--overlay)
    (setq tabnine--overlay (make-overlay 1 1 nil nil t))
    (overlay-put tabnine--overlay 'keymap tabnine-completion-map)
    (overlay-put tabnine--overlay 'priority 100))
  tabnine--overlay)

(defun tabnine--set-overlay-text (ov completion line-text-after)
  "Set overlay OV with COMPLETION and LINE-TEXT-AFTER."
  (move-overlay ov (point) (let ((line-end-pos))
			     (if line-text-after
				 (- (line-end-position) (length line-text-after))
			       (line-end-position))))
  (let ((p-completion (propertize completion 'face 'tabnine-overlay-face)))
    (if (eolp)
        (progn
          (overlay-put ov 'after-string "") ; make sure posn is correct
          (setq tabnine--real-posn (cons (point) (posn-at-point)))
          (put-text-property 0 1 'cursor t p-completion)
          (overlay-put ov 'display "")
          (overlay-put ov 'after-string p-completion))
      (overlay-put ov 'display (substring p-completion 0 1))
      (overlay-put ov 'after-string (substring p-completion 1)))
    (overlay-put ov 'completion completion)
    (overlay-put ov 'start (point))))

(defun tabnine--display-overlay-completion (new_prefix new_suffix old_prefix old_suffix user_intent user-pos)
  "Show COMPLETION with UUID, NEW_PREFIX, NEW_SUFFIX, OLD_PREFIX and OLD_SUFFIX in overlay at LINE and COL.
For TabNine, COL is always 0.
USER-POS is the cursor position (for verification only)."
  (tabnine-clear-overlay)
  (save-restriction
    (widen)
    (save-excursion
      ;; remove common prefix
      (let* ((cur-line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
	     (common-prefix-len (length (s-shared-start new_prefix cur-line))))
        (setq new_prefix (substring new_prefix common-prefix-len))
        (forward-char common-prefix-len))

      (when (and (s-present-p new_prefix)
                 (or (= (point) user-pos) ;; up-to-date completion
		     (and (< (point) user-pos) ;; special case for removing indentation
                          (s-blank-p (s-trim (buffer-substring-no-properties (point) user-pos))))))
        (let* ((ov (tabnine--get-overlay))
	       (text new_prefix)
	       (line-text-after (buffer-substring-no-properties (point) (line-end-position))))
          ;; determine the text to be displayed
          (when  (and (s-present? new_suffix) tabnine-auto-balance)
	    (setq text (concat new_prefix new_suffix)))
          (tabnine--set-overlay-text ov text line-text-after)
          (overlay-put ov 'new_prefix new_prefix)
          (overlay-put ov 'new_suffix new_suffix)
          (overlay-put ov 'old_prefix old_prefix)
          (overlay-put ov 'old_suffix old_suffix)
	  (overlay-put ov 'user_intent user_intent))))))

(defun tabnine-clear-overlay ()
  "Clear TabNine overlay."
  (interactive)
  (when (tabnine--overlay-visible)
    (delete-overlay tabnine--overlay)
    (setq tabnine--real-posn nil)))

(defun tabnine-accept-completion (&optional transform-fn)
  "Accept completion.  Return t if there is a completion.
Use TRANSFORM-FN to transform completion if provided."
  (interactive)
  (when (tabnine--overlay-visible)
    (let* ((ov tabnine--overlay)
           (completion (overlay-get ov 'completion))
           (start (overlay-get ov 'start))
           (user_intent (overlay-get ov 'user_intent))
           (new_prefix (overlay-get ov 'new_prefix))
           (new_suffix (overlay-get ov 'new_suffix))
           (old_prefix (overlay-get ov 'old_prefix))
           (old_suffix (overlay-get ov 'old_suffix))
           (t-completion (funcall (or transform-fn #'identity) completion)))
      (tabnine-clear-overlay)
      ;; maybe should not delete this line
      ;; (delete-region start (line-end-position))
      (insert t-completion)
      ;; if it is a partial completion
      (if (and (s-prefix-p t-completion completion)
	       (not (s-equals-p t-completion completion)))
          (tabnine--set-overlay-text (tabnine--get-overlay) (s-chop-prefix t-completion completion))
	(when (and tabnine-auto-balance (s-present? old_suffix))
	  (delete-region (point)
			 (min (+ (point) (length old_suffix))
			      (point-max))))
	;; full completion
	;; (when (s-contains? (s-right 1 t-completion) "?})")
        ;;   (newline-and-indent))
	)
      t)))

(defmacro tabnine--define-accept-completion-by-action (func-name action)
  "Define function FUNC-NAME to accept completion by ACTION."
  `(defun ,func-name (&optional n)
     (interactive "p")
     (setq n (or n 1))
     (tabnine-accept-completion (lambda (completion)
                                  (with-temp-buffer
				    (insert completion)
				    (goto-char (point-min))
				    (funcall ,action n)
				    (buffer-substring-no-properties (point-min) (point)))))))

(tabnine--define-accept-completion-by-action tabnine-accept-completion-by-word #'forward-word)
(tabnine--define-accept-completion-by-action tabnine-accept-completion-by-line #'forward-line)
(tabnine--define-accept-completion-by-action tabnine-accept-completion-by-paragraph #'forward-paragraph)

(defun tabnine--get-line-count()
  "Get current buffer's line count."
  (save-excursion
    (goto-char (point-max))
    (line-number-at-pos)))


(defun tabnine--show-completion (old_prefix completion)
  "Show OLD_PREFIX's COMPLETION."
  (when (and (tabnine--satisfy-display-predicates))
    (tabnine--dbind (:new_prefix
		     :old_suffix :new_suffix :completion_metadata
		     (:kind :completion_kind :snippet_context (:snippet_id :response_time_ms :completion_index :user_intent))) completion
      (let ((uuid snippet_id)
	    (line (- (line-number-at-pos) 1))
	    (character (- (point) (line-beginning-position)))
	    (pos  (point)))
	;; Comment,
	;; Block,
	;; FunctionDeclaration,
	;; NoScope,
	;; NewLine,
	;; CustomTriggerPoints,
	;; (when (s-equals? user_intent "NewLine")
	;;   ;; check weather need a newline
	;;   (let ((buffer-line-count (tabnine--get-line-count)))
	;;     (when (<= (1- buffer-line-count) line)
	;;       (save-excursion
	;; 	(goto-char (point-max))
	;; 	(newline))))
	;;   (setq line (1+ line)))
	(when (s-starts-with? old_prefix  new_prefix)
	  (setq new_prefix (s-chop-prefix old_prefix new_prefix)))
	(tabnine--display-overlay-completion new_prefix new_suffix old_prefix old_suffix
					     user_intent (point))))))


;;;###autoload
(defun tabnine-complete ()
  "Complete at the current point."
  (interactive)
  (setq tabnine--buffer-changed nil)
  (setq tabnine--completion-cache nil)
  (setq tabnine--completion-idx 0)

  (let ((called-interactively (called-interactively-p 'interactive)))
    (let* ((result (tabnine--get-completion-result))
	   (completions (plist-get result :results))
	   (completions (tabnine--filter-completions completions))
           (completion (if (seq-empty-p completions) nil (seq-elt completions 0))))
      (unless completion
        (when called-interactively
	  (message "No overlay completion is available."))))))

;;
;; minor mode
;;

(defcustom tabnine-disable-predicates nil
  "A list of predicate functions with no argument to disable TabNine.
TabNine will not be triggered if any predicate returns t."
  :type '(repeat function)
  :group 'tabnine)


(defcustom tabnine-enable-predicates '(evil-insert-state-p tabnine--buffer-changed-p
							   tabnine--completion-triggers-p)
  "A list of predicate functions with no argument to enable TabNine.
TabNine will be triggered only if all predicates return t."
  :type '(repeat function)
  :group 'tabnine)

(defcustom tabnine-disable-display-predicates nil
  "A list of predicate functions with no argument to disable TabNine.
TabNine will not show completions if any predicate returns t."
  :type '(repeat function)
  :group 'tabnine)

(defcustom tabnine-enable-display-predicates nil
  "A list of predicate functions with no argument to enable TabNine.
TabNine will show completions only if all predicates return t."
  :type '(repeat function)
  :group 'tabnine)

(defcustom tabnine-completion-triggers " .(){}[],:'\",=<>/\\+-|&*%=$#@!\n\r\t\v"
  "Completion triggers.")


(defmacro tabnine--satisfy-predicates (enable disable)
  "Return t if satisfy all predicates in ENABLE and none in DISABLE."
  `(and (cl-every (lambda (pred)
		    (if (functionp pred) (funcall pred) t))
                  ,enable)
        (cl-notany (lambda (pred)
		     (if (functionp pred) (funcall pred) nil))
                   ,disable)))

(defun tabnine--satisfy-trigger-predicates ()
  "Return t if all trigger predicates are satisfied."
  (tabnine--satisfy-predicates tabnine-enable-predicates tabnine-disable-predicates))

(defun tabnine--satisfy-display-predicates ()
  "Return t if all display predicates are satisfied."
  (tabnine--satisfy-predicates tabnine-enable-display-predicates tabnine-disable-display-predicates))

(defvar tabnine-mode-map (make-sparse-keymap)
  "Keymap for TabNine minor mode.
Use this for custom bindings in `tabnine-mode'.")

;;;###autoload
(define-minor-mode tabnine-mode
  "Minor mode for TabNine."
  :init-value nil
  :lighter " TabNine"
  (tabnine-clear-overlay)
  (advice-add 'posn-at-point :before-until #'tabnine--posn-advice)
  (if tabnine-mode
      (progn
        (add-hook 'post-command-hook #'tabnine--post-command nil 'local)
        (add-hook 'before-change-functions #'tabnine--on-change nil 'local))
    (remove-hook 'post-command-hook #'tabnine--post-command 'local)
    (remove-hook 'before-change-functions #'tabnine--on-change 'local)))

(defun tabnine--posn-advice (&rest args)
  "Remap posn if necessary."
  (when tabnine-mode
    (let ((pos (or (car-safe args) (point))))
      (when (and tabnine--real-posn
                 (eq pos (car tabnine--real-posn)))
        (cdr tabnine--real-posn)))))


;;;###autoload
(define-global-minor-mode global-tabnine-mode
  tabnine-mode tabnine-mode)

(defun tabnine--on-change (&reset _args)
  (setq tabnine--buffer-changed t))

(defun tabnine--post-command ()
  "Complete in `post-command-hook' hook."
  (when (and this-command
	     (not (and (symbolp this-command)
		       (or
                        (s-starts-with-p "tabnine-" (symbol-name this-command))
                        (member this-command tabnine-clear-overlay-ignore-commands)
                        (tabnine--self-insert this-command)))))
    (tabnine-clear-overlay)
    (when tabnine--post-command-timer
      (cancel-timer tabnine--post-command-timer))
    (setq tabnine--post-command-timer
          (run-with-idle-timer tabnine-idle-delay
			       nil
			       #'tabnine--post-command-debounce
			       (current-buffer)))))

(defun tabnine--self-insert (command)
  "Handle the case where the char just inserted is the start of the completion.
If so, update the overlays and continue. COMMAND is the
command that triggered `post-command-hook'.
"

  (when (and (eq command 'self-insert-command)
	     (tabnine--overlay-visible)
	     (tabnine--satisfy-display-predicates))
    (let* ((ov tabnine--overlay)
           (completion (overlay-get ov 'completion)))
      ;; The char just inserted is the next char of completion
      (when (and (> (length completion) 1)
                 (eq last-command-event (elt completion 0)))
        (tabnine--set-overlay-text ov (substring completion 1))))))

(defun tabnine--post-command-debounce (buffer)
  "Complete in BUFFER."
  (when (and (buffer-live-p buffer)
	     (equal (current-buffer) buffer)
	     tabnine-mode
	     (tabnine--satisfy-trigger-predicates))
    (tabnine-complete)))


(defun tabnine--completion-triggers-p()
  "Check whether completion triggers."
  (let ((before (char-before))
	(symbol (thing-at-point 'symbol)))
    (or (and tabnine-completion-triggers before (s-contains? (char-to-string before) tabnine-completion-triggers))
	(and tabnine-minimum-prefix-length symbol (>= (length symbol) tabnine-minimum-prefix-length))
	(not tabnine-minimum-prefix-length))))


;; Advices
;;


;;
;; Hooks
;;

(defun ~advice/tabnine-prefetch (orig-func &rest args)
  (when (buffer-file-name)
    (tabnine-prefetch)))


(eval-after-load 'tabnine
  (advice-add 'switch-to-buffer :after #'~advice/tabnine-prefetch))


(provide 'tabnine)


;;; tabnine.el ends here
