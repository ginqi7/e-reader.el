;;; e-reader.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'nov)
(require 'ekp)
(require 'image-slicing)

(defcustom e-reader-line-piexl 1000
  "Pixel height per line for rendering text.")

(defcustom e-reader-replacements
  '(("’" . "'")
    ("‐" . ""))
  "Alist of character replacements to apply during text rendering.
Each element is a cons cell (FROM . TO) where FROM is replaced by TO.")

(defcustom e-reader-font-family
  nil
  "Default font family for rendering text.
If nil, use the current default font.")

(defcustom e-reader-font-size
  300
  "Default font size for rendering text.
Value is in points (1/72 of an inch).")

;;; Examples:
;; (setq e-reader-font-family "Alegreya")
;; (setq e-reader-font-size 300)

(defvar e-reader--org-links-hash (make-hash-table)
  "Hash table storing temporary mappings of org-style file links.
Used by `e-reader-kp--replace-org-links' and `e-reader-kp--reverse-org-links'.")

(defun e-reader-kp--replace-org-links (str)
  "Replace org-style file links with hash placeholders in STR.
Each [[file:...]] link is replaced with its hash for temporary storage."
  (replace-regexp-in-string
   "\\[\\[file:.*\\]\\]"
   (lambda (match)
     (puthash (number-to-string (sxhash match)) match e-reader--org-links-hash)
     (number-to-string (sxhash match)))
   str))

(defun e-reader-kp--reverse-org-links (str)
  "Restore org-style file links from hash placeholders in STR.
Reverses the transformation done by `e-reader-kp--replace-org-links'."
  (dolist (key (hash-table-keys e-reader--org-links-hash))
    (setq str (string-replace key (gethash key e-reader--org-links-hash) str)))
  (setq e-reader--org-links-hash (make-hash-table))
  str)

(defun e-reader--replace-text (str)
  "Apply character replacements to STR based on `e-reader-replacements'.
Each replacement in the alist is applied sequentially."
  (dolist (replacement e-reader-replacements)
    (setq str (string-replace (car replacement) (cdr replacement) str)))
  str)

;; (e-reader-kp--reverse-org-links (e-reader-kp--replace-org-links "[[file:123]]\nhello"))

(defmacro e-reader-kp-with-font (body)
  "1. get current default font family and font size
2. set default font to (set-face-attribute 'default nil :family e-reader-font-family :height e-reader-font-size)
3. run body
4. revers default font to the font in step 1."
  (declare (indent 1))
  `(let ((orig-family (face-attribute 'default :family))
         (orig-height (face-attribute 'default :height)))
     (unwind-protect
         (progn
           (when e-reader-font-family
             (set-face-attribute 'default nil :family e-reader-font-family))
           (when e-reader-font-size
             (set-face-attribute 'default nil :height e-reader-font-size))
           ,body)
       (set-face-attribute 'default nil :family orig-family)
       (set-face-attribute 'default nil :height orig-height))))

(defun e-reader-kp-render ()
  "Render the current buffer using ekp with custom font settings.
Applies line pixel constraints and font customization before rendering."
  (interactive)
  (ekp-param-set 8 4 3 6 3 2 0 2 0)
  (let ((line-pixel e-reader-line-piexl)
        (buffer-read-only nil)
        (txt))
    (save-excursion
      (e-reader-kp-with-font
          (setq txt (e-reader-kp--reverse-org-links
                     (ekp-pixel-justify
                      (e-reader--replace-text
                       (e-reader-kp--replace-org-links
                        (buffer-string)))
                      line-pixel))))
      (erase-buffer)
      (insert txt)
      (e-reader-font-setup)
      (image-slicing-mode))))

(defun e-reader-font-setup ()
  "Apply font customization from `e-reader-font-family' and `e-reader-font-size'.
Adds face remappings for the default face in the current buffer."
    (face-remap-add-relative 'default :family e-reader-font-family)
  (when e-reader-font-size
    (face-remap-add-relative 'default :height e-reader-font-size)))

(provide 'e-reader)
;;; e-reader.el ends here
