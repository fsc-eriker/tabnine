;;; tabnine-chat.el --- TabNine Chat -*- lexical-binding: t -*-
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
;;; Commentary:
;;

;;; Code:

;;
;; Dependencies
;;

(require 'url)
(require 'url-http)
(require 'tabnine-core)
(require 'tabnine-util)

;; (declare-function tabnine-chat-menu "tabnine-chat-transient")
(declare-function tabnine-util--path-to-uri "tabnine-util")
(declare-function tabnine-util--language-id-buffer "tabnine-util")

(defconst tabnine-chat--buffer-name "*tabnine-chat*")

(defcustom tabnine-chat-default-mode (if (featurep 'markdown-mode)
					 'markdown-mode
				       'text-mode)
  "The default major mode for dedicated chat buffers.

If `markdown-mode' is available, it is used. Otherwise gptel
defaults to `text-mode'."
  :group 'tabnine
  :type 'symbol)

;; TODO: Handle `prog-mode' using the `comment-start' variable
(defcustom tabnine-chat-prompt-prefix-alist
  '((markdown-mode . "### ")
    (org-mode . "*** ")
    (text-mode . "### "))
  "String inserted after the response from TabNine Chat.

This is an alist mapping major modes to the prefix strings. This
is only inserted in dedicated tabnine chat buffers."
  :group 'tabnine
  :type '(alist :key-type symbol :value-type string))

(defcustom tabnine-api-server "https://api.tabnine.com"
  "TabNine api server address."
  :group 'tabnine
  :type 'string)

(defun tabnine-chat-prompt-string ()
  "TabNine chat prompt string."
  (or (alist-get major-mode tabnine-chat-prompt-prefix-alist) ""))

(defvar-local tabnine-chat--num-messages-to-send nil)
(defvar-local tabnine-chat--old-header-line nil)

(define-minor-mode tabnine-chat-mode
  "Minor mode for interacting with TabNine Chat."
  :lighter " TabNine Chat"
  :keymap (make-sparse-keymap)
  (if tabnine-chat-mode
      (setq tabnine-chat--old-header-line header-line-format
            header-line-format
            (list (concat (propertize " " 'display '(space :align-to 0))
                          (format "%s" (buffer-name)))
                  (propertize " Ready" 'face 'success)
                  '(:eval
                    (let* ((l1 (length gptel-model))
                           (num-exchanges
                            (if tabnine-chat--num-messages-to-send
                                (format "[Send: %s exchanges]" tabnine-chat--num-messages-to-send)
                              "[Send: buffer]"))
                           (l2 (length num-exchanges)))
                     (concat
                      (propertize
                       " " 'display `(space :align-to ,(max 1 (- (window-width) (+ 2 l1 l2)))))
                      (propertize
                       (button-buttonize num-exchanges
                        (lambda (&rest _) (gptel-menu)))
                       'mouse-face 'highlight
                       'help-echo
                       "Number of past exchanges to include with each request")
                      " "
                      (propertize
                       (button-buttonize (concat "[" tabnine-chat-model "]")
                            (lambda (&rest _) (gptel-menu)))
                           'mouse-face 'highlight
                           'help-echo "OpenAI GPT model in use"))))))
    (setq header-line-format tabnine-chat--old-header-line)))

(defun tabnine-chat--update-header-line (msg face)
  "Update header line with status MSG in FACE."
  (and tabnine-chat-mode (consp header-line-format)
    (setf (nth 1 header-line-format)
          (propertize msg 'face face))
    (force-mode-line-update)))

(defcustom tabnine-chat-stream t
  "Whether responses from TabNine Chat be played back as they are received.

This option is ignored unless Curl is in use (see `tabnine-chat-use-curl').

When set to nil, Emacs waits for the full response and inserts it
all at once. This wait is asynchronous.

'tis a bit silly."
  :group 'tabnine
  :type 'boolean)

(defcustom tabnine-chat-use-curl (and (executable-find "curl") t)
  "Whether gptel should prefer Curl when available."
  :group 'tabnine
  :type 'boolean)


;;
;; Variables
;;

(defvar tabnine-chat--conversation-id nil
  "The TabNine chat conversation id.")

(defun tabnine-chat--get-api-token()
  "Get API Token."
  (unless tabnine--access-token
    (tabnine-state))
  tabnine--access-token)

(defun tabnine-chat--get-conversion-id()
  "Get conversion ID."
  (unless tabnine-chat--conversation-id
    (setq tabnine-chat--conversation-id (tabnine-util--random-uuid)))
  tabnine-chat--conversation-id)

(defun tabnine-chat--editor-context()
  "Return the editor context for the current state."
  (let ((file-content (buffer-substring-no-properties (point-min) (point-max)))
	(selected-code (when (region-active-p)
			 (buffer-substring-no-properties
			  (region-beginning)
			  (region-end))))
	;; (selected-code-usages)
	;; (diagnosticsText)
	(file-uri (tabnine-util--path-to-uri (buffer-name)))
	(language (tabnine-util--language-id-buffer))
	(line-text-at-cursor (buffer-substring-no-properties (save-excursion
							       (beginning-of-line) (point))
							     (save-excursion
							       (end-of-line) (point))))
	;; (metadata)
	)
    (list
     :fileCode file-content
     :selectedCode selected-code
     ;; :selectedCodeUsages
     :diagnosticsText ""
     :fileUri file-uri
     :language language
     :lineTextAtCursor line-text-at-cursor
     :metadata nil)))

(defun tabnine-chat--text-by-method(method)
  "Get text by METHOD."
  (let ((text))
    (cond
     ((eq method 'explain-code)
      (setq text "Explain the selected code"))
     ((eq method 'generate-test-for-code)
      (setq text "Write tests for the selected code"))
     ((eq method 'document-code)
      (setq text "Add documentation for the selected code"))
     ((eq method 'fix-code)
      (setq text "Find errors in the selected code and fix them")))
    text))

(defun tabnine-chat--make-request (method)
  "TabNine api make request with METHOD.
Method can be explain-code, document-code, generate-test-for-code or fix-code."
  (let ((current-context (list
			  :id (tabnine-util--random-uuid)
			  :text (tabnine-chat--text-by-method method)
			  :by "user"
			  :editorContext (tabnine-chat--editor-context)
			  :diagnosticsText (tabnine-chat--get-diagnostics-text)
			  ;; :retrievalContext
			  )))
    (list
     :conversationId (tabnine-chat--get-conversion-id)
     :messageId (tabnine-util--random-uuid)
     :input (vector current-context)
     :isTelemetryEnabled :json-false)))

(defmacro tabnine-chat--send-request (method)
  "Tabnine api send request with METHOD."
  `(let* ((request (tabnine-chat--make-request ,method))
	  (encoded (tabnine-util--json-serialize request))
	  (url-request-method "POST")
	  (url-str (concat tabnine-api-server "/chat/generate_chat_response"))
	  (url-request-data (url-http--encode-string encoded))
	  ;; (url-request-coding-system 'utf-8)
	  (url-http-attempt-keepalives t)
	(url-request-extra-headers `(("Authorization" . ,(concat  "Bearer " (tabnine-chat--get-api-token)))
				  ("Content-Type" . "application/json")
				  ("Accept" . "*/*")))
	(buf (url-retrieve-synchronously url-str nil t)))
     ;; (switch-to-buffer buf)
    (let* ((results (tabnine-chat--url-parse-response buf))
	   (text (tabnine-chat--results-to-text results)))
      (when text
	(with-current-buffer (get-buffer-create tabnine-chat--buffer-name)
	  (goto-char (point-min))
	  (insert "\n==================START===================\n")
	  (insert text)
	  (insert "\n==================END===================\n\n")))
      (switch-to-buffer tabnine-chat--buffer-name))
    (when (bufferp buf)
      (kill-buffer buf))))

(defun tabnine-chat--get-diagnostics-text()
  "Get diagnostic text with flycheck."
  (let ((errors (tabnine-util--get-list-errors)))
    (string-join errors)))

(defun tabnine-chat--url-parse-response (response-buffer)
  "Parse response in RESPONSE-BUFFER."
  (when (buffer-live-p response-buffer)
    (with-current-buffer response-buffer
      (let* ((http-msg (progn (goto-char (point-min))
			      (string-trim
			       (buffer-substring
				(line-beginning-position)
				(line-end-position)))))
             (body (progn (goto-char (point-min))
			  (forward-paragraph)
			      (decode-coding-string
			       (buffer-substring-no-properties (point) (point-max))
			       'utf-8))))
	  (cond
           ((string-match-p "404 Not Found" http-msg);; token expired
	    (message "TabNine token is expired, set tabnine--access-token to nil.")
	    (setq tabnine--access-token nil))
	   ((string-match-p "200 OK" http-msg)
	    (let* ((ss (s-split "\n" (s-trim body)))
		   (ss (cl-remove-if (lambda(x) (not (s-present? x))) ss))
		   (json-ss (mapcar (lambda(x) (tabnine-util--read-json x)) ss)))
	      json-ss))
	   (t (message "unknow error: %s" http-msg)))))))

(defun tabnine-chat--results-to-text(results)
  "TabNine RESULTS in sequence to text."
  (when results
    (let ((text-arr (mapcar (lambda(x) (plist-get x :text)) results)))
      (string-join text-arr))))


;;
;; TabNine Chat Operations
;;

(defun tabnine-chat-explain-code()
  "TabNine chat explain code."
  (interactive)
  (tabnine-chat--send-request 'explain-code))

(defun tabnine-chat-generate-test-for-code()
  "TabNine chat generate test for code."
  (interactive)
  (tabnine-chat--send-request 'generate-test-for-code))

(defun tabnine-chat-document-code()
  "TabNine chat write document code."
  (interactive)
  (tabnine-chat--send-request 'document-code))

(defun tabnine-chat-fix-code()
  "Find errors in the selected code and fix them."
  (interactive)
  (tabnine-chat--send-request 'fix-code))


(provide 'tabnine-chat)

;;; tabnine-chat.el ends here
