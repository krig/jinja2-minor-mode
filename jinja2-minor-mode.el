;;; jinja2-minor-mode --- Jinja2 template minor mode
;; -*- coding: utf-8; lexical-binding: t -*-

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

(defcustom jinja2-user-functions nil
  "Custom user functions."
  :type '(repeat string)
  :group 'jinja2)

(defun jinja2-builtin-keywords ()
  "Jinja2 keywords."
  (append
   jinja2-user-keywords
   '("if" "for" "block" "filter" "with"
     "raw" "macro" "autoescape" "trans" "call"
     "else" "elif" "end"
     "as" "autoescape" "debug" "extends"
     "firstof" "in" "include" "load"
     "now" "regroup" "ssi" "templatetag"
     "url" "widthratio" "elif" "true"
     "false" "none" "False" "True" "None"
     "loop" "super" "caller" "varargs"
     "kwargs" "break" "continue" "is"
     "not" "or" "and"
     "do" "pluralize" "set" "from" "import"
     "context" "with" "without" "ignore"
     "missing" "scoped")))

(defun jinja2-builtin-functions ()
  "Jinja2 builtin functions."
  (append
   jinja2-user-functions
   '("abs" "attr" "batch" "capitalize"
     "center" "default" "dictsort"
     "escape" "filesizeformat" "first"
     "float" "forceescape" "format"
     "groupby" "indent" "int" "join"
     "last" "length" "list" "lower"
     "pprint" "random" "replace"
     "reverse" "round" "safe" "slice"
     "sort" "string" "striptags" "sum"
     "title" "trim" "truncate" "upper"
     "urlize" "wordcount" "wordwrap" "xmlattr")))

(defconst jinja2-font-lock-keywords
  `(
    (,(rx-to-string `(and word-start ,(append '(or) (jinja2-builtin-keywords)) word-end))
     (0 font-lock-keyword-face))
    (,(rx-to-string `(and word-start ,(append '(or) (jinja2-builtin-functions)) word-end))
     (0 font-lock-builtin-face))

    (,(rx (or "{%" "%}" "{%-" "-%}")) (0 font-lock-type-face t))
    (,(rx (or "{{" "}}")) (0 font-lock-type-face t))
    (,(rx "{#"
          (* whitespace)
          (group
           (*? anything))
          (* whitespace)
          "#}")
     (1 font-lock-comment-face t))
    (,(rx (or "{#" "#}")) (0 font-lock-comment-delimiter-face t))
    ))

(defun jinja2-register-keywords (addp)
  "Install/remove jinja2 keywords in current buffer. If ADDP is non-nil, install else remove."
  (funcall (if addp 'font-lock-add-keywords 'font-lock-remove-keywords)
           nil jinja2-font-lock-keywords))


;;;###autoload
(define-minor-mode jinja2-minor-mode
  "Add jinja2 template handling to other modes."
  :lighter " j2"
  :group 'jinja2
  (jinja2-register-keywords jinja2-minor-mode)
  (font-lock-flush))

(provide 'jinja2-minor-mode)
;;; jinja2-minor-mode ends here
