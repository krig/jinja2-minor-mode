;; -*- coding: utf-8; lexical-binding: t -*-
;;; jinja2-minor-mode --- Jinja2 template minor mode

;; Copyright (C) 2021 Kristoffer Grönlund

;; Author: Kristoffer Grönlund <krig@koru.se>
;; URL: https://github.com/krig/jinja2-minor-mode
;; Keywords: jinja2, languages, faces
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file incorporates work covered by the following copyright and
;; permission notice:

;;   Licensed under the Apache License, Version 2.0 (the "License"); you may not
;;   use this file except in compliance with the License.  You may obtain a copy
;;   of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;;   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
;;   License for the specific language governing permissions and limitations
;;   under the License.


;;; Commentary:

;; Parts of this code was borrowed from jinja2-mode mode. (Thanks Florian Mounier)
;; Parts of this code was borrowed from mmm-mako mode. (Thanks Philip Jenvey)
;; Parts of this code was borrowed from mmm-jinja2 mode. (Thanks Ben Hayden)


;;; Code:

(require 'custom)
(require 'font-lock)

(defgroup jinja2 nil
  "Minor mode for editing jinja2 code."
  :prefix "jinja2-"
  :group 'languages)

(defcustom jinja2-user-keywords nil
  "Custom user keywords."
  :type '(repeat string)
  :group 'jinja2)

(defcustom jinja2-user-filters nil
  "Custom user filters."
  :type '(repeat string)
  :group 'jinja2)

(defcustom jinja2-user-functions nil
  "Custom user functions."
  :type '(repeat string)
  :group 'jinja2)

(defconst jinja2-builtin-blocks
  '("block" "for" "if" "raw" "macro" "set"
    "filter" "call" "trans" "with"
    "autoescape"))

(defconst jinja2-builtin-constants
  '("True" "False" "true" "false" "None" "none"))

(defconst jinja2-builtin-filters
  '("abs" "attr" "batch" "capitalize"
    "center" "default" "dictsort"
    "escape" "filesizeformat" "first"
    "float" "forceescape" "format"
    "groupby" "indent" "int" "join"
    "last" "length" "list" "lower"
    "map" "max" "min" "pprint"
    "random" "reject" "rejectattr"
    "replace"
    "reverse" "round" "safe" "select" "selectattr" "slice"
    "sort" "string" "striptags" "sum"
    "title" "tojson" "trim" "truncate" "unique" "upper"
    "urlencode"
    "urlize" "wordcount" "wordwrap" "xmlattr"))

(defconst jinja2-builtin-tests
  '("callable" "defined" "divisibleby"
    "eq" "escaped" "even" "ge" "gt" "in"
    "iterable" "le" "lower" "lt" "mapping"
    "ne" "none" "number" "odd" "sameas"
    "sequence" "string" "undefined" "upper"))

(defconst jinja2-builtin-functions
  '("range" "lipsum" "dict" "cycler"
    "reset" "next" "current" "joiner"
    "namespace"))

(defconst jinja2-builtin-keywords
  '("import" "extends" "in" "scoped" "from" "as" "include"
    "ignore" "missing" "with" "without" "elif" "else" "not"
    "and" "or" "is" "pluralize" "do" "continue" "break"
    "set" "super"))

(defun jinja2-list-keywords ()
  "Jinja2 keywords."
  (append
   jinja2-user-keywords
   jinja2-builtin-keywords))

(defun jinja2-list-filters ()
  "Jinja2 filters."
  (append
   jinja2-user-filters
   jinja2-builtin-filters))

(defun jinja2-list-functions ()
  "Jinja2 functions."
  (append
   jinja2-user-functions
   jinja2-builtin-functions))

(defun jinja2-templatep (limit)
  "True if point is inside a jinja2 template scope, looking no further than LIMIT."
  (save-excursion
    (goto-char (point-at-bol))
    (while (search-forward "{" nil t)
      (let ((pos (point)))
        )))
  (let ((result (save-excursion
                  (re-search-backward (rx (or "{%" "{{" "{%-" "{%+")) (point-at-bol) t))))
    (message "Result: %s Point: %s BOL: %s" result (point) (point-at-bol))
    result))

(defun jinja2-matcher (re)
  "Return a function to match RE in a jinja context."
  (lambda (limit)
    (and (jinja2-templatep limit) (re-search-forward re (min (point-at-eol) limit) t))))

(defun jinja2-register-keywords (addp)
  "Install/remove jinja2 keywords in current buffer. If ADDP is non-nil, install else remove."
  (let ((keywords
         `(
           (,(jinja2-matcher (rx-to-string `(seq word-start ,(append '(or) (jinja2-list-keywords)) word-end)))
            (0 font-lock-keyword-face))
           (,(jinja2-matcher (rx-to-string `(seq word-start ,(append '(or) (jinja2-list-functions)) word-end)))
            (0 font-lock-builtin-face))
           (,(jinja2-matcher (rx-to-string `(seq word-start (group (+ word)) word-end (* whitespace) "(")))
            (1 font-lock-function-name-face))
           (,(jinja2-matcher (rx (seq "|" (* whitespace) word-start (group (eval (append '(or) (jinja2-list-filters)))) word-end)))
            (1 font-lock-builtin-face))
           (,(jinja2-matcher (rx (seq word-start (eval (append '(or) jinja2-builtin-tests)) word-end)))
            (0 font-lock-builtin-face))
           (,(jinja2-matcher (rx (seq word-start (eval (append '(or) jinja2-builtin-constants)) word-end)))
            (0 font-lock-constant-face))
           (,(jinja2-matcher (rx (seq word-start (eval (append '(or) jinja2-builtin-blocks)) word-end)))
            (0 font-lock-keyword-face))
           (,(jinja2-matcher (rx (seq word-start (eval (append '(or) (mapcar (lambda (s) (concat "end" s)) jinja2-builtin-blocks))) word-end)))
            (0 font-lock-keyword-face))
           (,(rx (or "{%" "%}" "{%-" "-%}" "{%+")) (0 font-lock-preprocessor-face t))
           (,(rx (or "{{" "}}")) (0 font-lock-preprocessor-face t))
           (,(rx (or "{#" "#}")) (0 font-lock-comment-delimiter-face t))
           (,(rx (seq
                  "{#"
                  (* whitespace)
                  (group
                   (*? anything))
                  (* whitespace)
                  "#}"))
            (1 font-lock-comment-face t))
           )))
    (if addp
        (font-lock-add-keywords nil keywords)
      (font-lock-remove-keywords nil keywords))))


;;;###autoload
(define-minor-mode jinja2-minor-mode
  "Add jinja2 template handling to other modes."
  :lighter " j2"
  :group 'jinja2
  (jinja2-register-keywords jinja2-minor-mode)
  (font-lock-flush))

(provide 'jinja2-minor-mode)
;;; jinja2-minor-mode ends here
