;;; tabnine.el --- An unofficial TabNine package with TabNine Chat supported -*- lexical-binding: t -*-
;;
;; Copyright (c) 2023  Aaron Ji, Tommy Xiang, John Gong
;;
;; Author: Aaron Ji <shuxiao9058@gmail.com>
;;         Tommy Xiang <tommyx058@gmail.com>
;;         John Gong <gjtzone@hotmail.com>
;;
;; Keywords: convenience
;; Version: 0.0.1
;; URL: https://github.com/shuxiao9058/tabnine/
;; Package-Requires: ((emacs "28.1") (dash "2.16.0") (s "1.12.0") (editorconfig "0.9.1") (vterm "0.0.2") (language-id "0.5.1") (transient "0.4.0"))
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
;; Description:
;;
;; An unofficial TabNine package with TabNine Chat supported.
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

(require 'tabnine-core)
(require 'tabnine-chat)
(require 'tabnine-chat-transient)

(provide 'tabnine)

;;; tabnine.el ends here
