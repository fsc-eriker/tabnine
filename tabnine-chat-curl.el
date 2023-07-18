;;; tabnine-chat-curl.el --- Curl Support for TabNine Chat -*- lexical-binding: t -*-

;; Copyright (C) 2023  Aaron Ji

;; Author: Aaron Ji;; <shuxiao9058@gmail.com>
;; Keywords: convenience

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

(require 'tabnine-core)
(require 'tabnine-chat)

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'map)

(defvar tabnine-chat-curl--process-alist nil
  "Alist of active TabNine Chat curl requests.")

(defun tabnine-chat-curl--get-args (info)
  "Produce list of arguments for calling Curl.

INFO is the operation info."
  (let* ((request (tabnine-chat--make-request (plist-get info :prompt)))
	 (encoded (tabnine-util--json-serialize request))
	 (data (url-http--encode-string encoded))
	 (url (format "%s/chat/generate_chat_response" tabnine-api-server))
         (headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " (tabnine-chat--get-api-token))))))
    (append
     (list "--location" "--silent" "--compressed" "--disable"
           (format "-X%s" "POST")
           ;; (format "-w(%s . %%{size_header})" token)
           (format "-m%s" 60)
           "-D-"
           (format "-d%s" data))
     (when (and tabnine-network-proxy (stringp tabnine-network-proxy)
		(not (string-empty-p tabnine-network-proxy)))
       (list "--proxy" tabnine-network-proxy
             "--proxy-negotiate"
             "--proxy-user" ":"))
     (cl-loop for (key . val) in headers
              collect (format "-H%s: %s" key val))
     (list url))))

;;TODO: The :transformer argument here is an alternate implementation of
;;`tabnine-chat-response-filter-functions'. The two need to be unified.
;;;###autoload
(defun tabnine-chat-curl-get-response (info &optional callback)
  "Retrieve response to prompt in INFO.

INFO is a plist with the following keys:
- :prompt (the prompt being sent)
- :buffer (the TabNine Chat buffer)
- :position (marker at which to insert the response).

Call CALLBACK with the response and INFO afterwards. If omitted
the response is inserted into the current buffer after point."
  (let* ((token (md5 (format "%s%s%s%s"
                             (random) (emacs-pid) (user-full-name)
                             (recent-keys))))
	 (args (tabnine-chat-curl--get-args info))
         (process (apply #'start-process "tabnine-chat-curl"
                         (generate-new-buffer tabnine-chat--curl-buffer-name) "curl" args)))
    (with-current-buffer (process-buffer process)
      (set-process-query-on-exit-flag process nil)
      (setf (alist-get process tabnine-chat-curl--process-alist)
            (nconc (list :token token
			 :callback (or callback
                                       (if tabnine-chat-stream
                                           #'tabnine-chat-curl--stream-insert-response
                                         #'tabnine-chat--insert-response))
                         :transformer (when (or (eq tabnine-chat-default-mode 'org-mode)
                                                (eq (buffer-local-value
                                                     'major-mode
                                                     (plist-get info :buffer))
                                                    'org-mode))
                                        (tabnine-chat--stream-convert-markdown->org)))
                   info))
      (if tabnine-chat-stream
          (progn (set-process-sentinel process #'tabnine-chat-curl--stream-cleanup)
                 (set-process-filter process #'tabnine-chat-curl--stream-filter))
        (set-process-sentinel process #'tabnine-chat-curl--sentinel)))))

(defun tabnine-chat-abort (buffer)
  "Stop any active tabnine-chat process associated with the current BUFFER."
  (interactive (list (current-buffer)))
  (unless tabnine-chat-use-curl
    (user-error "Cannot stop a `url-retrieve' request!"))
  (if-let* ((proc-attrs
            (cl-find-if
             (lambda (proc-list)
               (eq (plist-get (cdr proc-list) :buffer) buffer))
             tabnine-chat-curl--process-alist))
            (proc (car proc-attrs)))
      (progn
        (setf (alist-get proc tabnine-chat-curl--process-alist nil 'remove) nil)
        (set-process-sentinel proc #'ignore)
        (delete-process proc)
        (kill-buffer (process-buffer proc))
        (with-current-buffer buffer
          (when tabnine-chat-mode (tabnine-chat--update-header-line  " Ready" 'success)))
        (message "Stopped TabNine Chat request in buffer %S" (buffer-name buffer)))
    (message "No TabNine Chat request associated with buffer %S" (buffer-name buffer))))

;; TODO: Separate user-messaging from this function
(defun tabnine-chat-curl--stream-cleanup (process _status)
  "Process sentinel for TabNine Chat curl requests.

PROCESS and _STATUS are process parameters."
  (let ((proc-buf (process-buffer process)))
    (when tabnine-chat--debug
      (with-current-buffer proc-buf
        (clone-buffer tabnine-chat--debug-buffer-name 'show)))
    (let* ((info (alist-get process tabnine-chat-curl--process-alist))
           (tabnine-chat-buffer (plist-get info :buffer))
           (tracking-marker (plist-get info :tracking-marker))
           (start-marker (plist-get info :position))
           (http-status (plist-get info :http-status))
           (http-msg (plist-get info :status)))
      (if (equal http-status "200")
          (progn
            ;; Finish handling response
            (with-current-buffer (marker-buffer start-marker)
              (pulse-momentary-highlight-region (+ start-marker 2) tracking-marker)
              (when tabnine-chat-mode (save-excursion (goto-char tracking-marker)
                                               (insert "\n\n" (tabnine-chat-prompt-prefix-string)))))
            (with-current-buffer tabnine-chat-buffer
              (when tabnine-chat-mode (tabnine-chat--update-header-line  " Ready" 'success))))
        ;; Or Capture error message
        (with-current-buffer proc-buf
          (goto-char (point-max))
          (search-backward (plist-get info :token))
          (backward-char)
          (pcase-let* ((`(,_ . ,header-size) (read (current-buffer)))
                       (json-object-type 'plist)
                       (response (progn (goto-char header-size)
                                        (condition-case nil (json-read)
                                          (json-readtable-error 'json-read-error)))))
            (cond
             ((plist-get response :error)
              (let* ((error-plist (plist-get response :error))
                     (error-msg (plist-get error-plist :message))
                     (error-type (plist-get error-plist :type)))
                (message "TabNine Chat error: (%s) %s" http-msg error-msg)
                (setq http-msg (concat "("  http-msg ") " (string-trim error-type)))))
             ((eq response 'json-read-error)
              (message "TabNine Chat error (%s): Malformed JSON in response." http-msg))
             (t (message "TabNine Chat error (%s): Could not parse HTTP response." http-msg)))))
        (with-current-buffer tabnine-chat-buffer
          (when tabnine-chat-mode
            (tabnine-chat--update-header-line
             (format " Response Error: %s" http-msg) 'error))))
      (with-current-buffer tabnine-chat-buffer
        (run-hooks 'tabnine-chat-post-response-hook)))
    (setf (alist-get process tabnine-chat-curl--process-alist nil 'remove) nil)
    (kill-buffer proc-buf)))

(defun tabnine-chat-curl--stream-insert-response (response info)
  "Insert streaming RESPONSE from ChatGPT into the TabNine Chat buffer.

INFO is a mutable plist containing information relevant to this buffer.
See `tabnine-chat--url-get-response' for details."
  (let ((start-marker (plist-get info :position))
        (tracking-marker (plist-get info :tracking-marker))
        (transformer (plist-get info :transformer)))
    (when response
        (with-current-buffer (marker-buffer start-marker)
          (save-excursion
            (unless tracking-marker
              (tabnine-chat--update-header-line " Typing..." 'success)
              (goto-char start-marker)
              (unless (or (bobp) (plist-get info :in-place))
                (insert "\n\n"))
              (setq tracking-marker (set-marker (make-marker) (point)))
              (set-marker-insertion-type tracking-marker t)
              (plist-put info :tracking-marker tracking-marker))

            (when transformer
              (setq response (funcall transformer response)))

            (put-text-property 0 (length response) 'tabnine-chat 'response response)
            (goto-char tracking-marker)
            (insert response))))))

(defun tabnine-chat-curl--stream-filter (process output)
  "Filter for TabNine Chat curl process.
PROCESS is the process under watch, OUTPUT is the output received."
  (let* ((proc-info (alist-get process tabnine-chat-curl--process-alist)))
    (with-current-buffer (process-buffer process)
      ;; Insert output
      (save-excursion
        (goto-char (process-mark process))
        (insert output)
        (set-marker (process-mark process) (point)))

      ;; Find HTTP status
      (unless (plist-get proc-info :http-status)
        (save-excursion
          (goto-char (point-min))
          (when-let* (((not (= (line-end-position) (point-max))))
                      (http-msg (buffer-substring (line-beginning-position)
                                                  (line-end-position)))
                      (http-status
                       (save-match-data
                         (and (string-match "HTTP/[.0-9]+ +\\([0-9]+\\)" http-msg)
                              (match-string 1 http-msg)))))
            (plist-put proc-info :http-status http-status)
            (plist-put proc-info :status (string-trim http-msg))))
        ;; Handle read-only TabNine Chat buffer
        (when (with-current-buffer (plist-get proc-info :buffer)
                (or buffer-read-only
                    (get-char-property (plist-get proc-info :position) 'read-only)))
          (message "Buffer is read only, displaying reply in buffer \"*TabNine Chat response*\"")
          (display-buffer
           (with-current-buffer (get-buffer-create "*TabNine Chat response*")
             (goto-char (point-max))
             (move-marker (plist-get proc-info :position) (point) (current-buffer))
             (current-buffer))
           '((display-buffer-reuse-window
              display-buffer-pop-up-window)
             (reusable-frames . visible)))))

      (when-let ((http-msg (plist-get proc-info :status))
                 (http-status (plist-get proc-info :http-status)))
        ;; Find data chunk(s) and run callback
        (when (equal http-status "200")
          (funcall (or (plist-get proc-info :callback)
                       #'tabnine-chat-curl--stream-insert-response)
                   (let* ((json-object-type 'plist)
                          (content-strs))
		     (condition-case nil
			 (while (not (eobp))
			   (when-let* ((line-content (buffer-substring-no-properties
						      (save-excursion
							(beginning-of-line) (point))
						      (save-excursion
							(end-of-line) (point))))
				       (response (tabnine-util--read-json line-content))
				       (content (plist-get response :text)))
			 (push content content-strs))
		       (forward-line))
		      (error
		       (forward-line)))
                     (apply #'concat (nreverse content-strs)))
                   proc-info))))))

(defun tabnine-chat-curl--sentinel (process _status)
  "Process sentinel for TabNine Chat curl requests.

PROCESS and _STATUS are process parameters."
  (let ((proc-buf (process-buffer process)))
    (when tabnine-chat--debug
      (with-current-buffer proc-buf
        (clone-buffer tabnine-chat--debug-buffer-name 'show)))
    (when-let* (((eq (process-status process) 'exit))
                (proc-info (alist-get process tabnine-chat-curl--process-alist))
                (proc-token (plist-get proc-info :token))
                (proc-callback (plist-get proc-info :callback)))
      (pcase-let ((`(,response ,http-msg ,error)
                   (tabnine-chat-curl--parse-response proc-buf proc-token)))
        (plist-put proc-info :status http-msg)
        (when error (plist-put proc-info :error error))
        (funcall proc-callback response proc-info)))
    (setf (alist-get process tabnine-chat-curl--process-alist nil 'remove) nil)
    (kill-buffer proc-buf)))

(defun tabnine-chat-curl--parse-response (buf token)
  "Parse the buffer BUF with curl's response.

TOKEN is used to disambiguate multiple requests in a single
buffer."
  (with-current-buffer buf
    (progn
      (goto-char (point-max))
      (search-backward token)
      (backward-char)
      (pcase-let* ((`(,_ . ,header-size) (read (current-buffer))))
          ;; (if (search-backward token nil t)
          ;;     (search-forward ")" nil t)
          ;;   (goto-char (point-min)))
          (goto-char (point-min))

          (if-let* ((http-msg (string-trim
                               (buffer-substring (line-beginning-position)
                                                 (line-end-position))))
                    (http-status
                     (save-match-data
                       (and (string-match "HTTP/[.0-9]+ +\\([0-9]+\\)" http-msg)
                            (match-string 1 http-msg))))
                    (json-object-type 'plist)
                    (response (progn (goto-char header-size)
                                     (condition-case nil
                                         (json-read)
                                       (json-readtable-error 'json-read-error)))))
              (cond
               ((equal http-status "200")
                (list (string-trim
                       (map-nested-elt response '(:choices 0 :message :content)))
                      http-msg))
                ((plist-get response :error)
                 (let* ((error-plist (plist-get response :error))
                        (error-msg (plist-get error-plist :message))
                        (error-type (plist-get error-plist :type)))
                   (list nil (concat "(" http-msg ") " (string-trim error-type)) error-msg)))
                ((eq response 'json-read-error)
                 (list nil (concat "(" http-msg ") Malformed JSON in response.")
                       "Malformed JSON in response"))
                (t (list nil (concat "(" http-msg ") Could not parse HTTP response.")
                         "Could not parse HTTP response.")))
            (list nil (concat "(" http-msg ") Could not parse HTTP response.")
                  "Could not parse HTTP response."))))))

(provide 'tabnine-chat-curl)

;;; tabnine-chat-curl.el ends here
