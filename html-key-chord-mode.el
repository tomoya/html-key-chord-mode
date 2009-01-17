;;; html-key-chord-mode.el --- support html tagging key chord
;; -*- coding: utf-8; mode:emacs-lisp -*-

;; Copyright (C) 2008 Tomoya Otake
;; Author: Tomoya Otake <tomoya.ton@gmail.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; * Description
;;
;; html-key-chord-mode is minor mode of Emacs that use key-chord.el.
;; This mode can be coded very quickly.
;;
;; * Usage
;;
;; Just put the code like below into your .emacs:
;;
;; (require 'html-key-chord-mode)


(require 'key-chord)
;; http://www.emacswiki.org/emacs/key-chord.el

(require 'xyzzy)
;; http://github.com/tomoya/xyzzy.el/tree

(defvar html-key-chord-mode nil) ; mode 変数。これで状態判定
(defvar hkc-mode-map
  (let ((map (make-sparse-keymap)))
    
    (define-key map "\C-ct" 'quote-line-by)
    (define-key map "\M-t" 'quote-region-by)

    map)
  "The keymap of html-key-chord-mode.")

(or (assq 'hkc-mode-map minor-mode-map-alist)
    (push (cons 'html-key-chord-mode hkc-mode-map) minor-mode-map-alist))

;; mode 行の設定。なくてもOK。
(if (not (assq 'html-key-chord-mode minor-mode-alist))
    (setq minor-mode-alist
          (cons '(html-key-chord-mode " hkc")
                minor-mode-alist)))

;; mode の On / Off をするための関数
(defun html-key-chord-mode (arg)
  "html-key-chord minor-mode"
  (interactive "P")
  ;; mode variable settings
  (setq html-key-chord-mode (if arg
			   (> (prefix-numeric-value arg) 0)
			 (not html-key-chord-mode)))
  (cond (html-key-chord-mode
	 (message "HTML Key Chord mode on"))
	(t
	 (message "HTML Key Chord mode off"))))


;; 行をタグでマークアップする関数のコマンド版
(defun quote-line-by (tag)
  "現在カーソルのある行をミニバッファに入力したタグで囲います。"
  (interactive "*sTag: ") ; s => 文字入力を指示する。
  (quote-by-tag tag))

;; リージョンをタグでマークアップする関数のコマンド版
(defun quote-region-by (tag)
  "リージョンで選択している範囲をミニバッファで入力したタグで囲います。"
  (interactive "*sTag: ") ; s => 文字入力を指示する。
  (quote-by-tag tag t))


(defun quote-by-tag (tag &optional behavior)
  "タグを受けとってスタートタグにセットした後、class や id などを除去してエンドタグにセットします。
そして、行、またはリージョンで囲う関数へタグを渡します。
behavior に、nil 以外の値を受けとった場合のみ、リージョンで囲う関数を使います。"
  (setq starttag tag) ; スタートタグはそのままセット
  (setq endtag (replace-regexp-in-string "\\s .*\$" "" starttag nil nil)) ; エンドダグは class / id を除去
  ;; string-match と replace-match
  ;; を使う方法が一番初歩っぽいけど、subr.el に定義されている
  ;; replace-regexp-in-string を使った方が楽っぽかったので、そうしました。

  ;; 2つ目の引数 behavior が non-nil のときは、リージョンをタグでくくる
  (cond (behavior
	 (quote-region-by-tag starttag endtag))
	(t
	 (quote-line-by-tag starttag endtag))))


(defun quote-region-by-tag (starttag endtag)
  "受けとったタグで、リージョンを囲います。"
  (let ((begin (mark))
        (end (point)))
    (when (> begin end)
      (let ((tmp begin))
        (setq begin end
              end tmp)))
    (save-excursion
      (goto-char end)
      (insert "</" endtag ">")
      (goto-char begin)
      (insert "<" starttag ">"))))


(defun quote-line-by-tag (starttag endtag)
  "受けとったタグで、行を囲います。"
  (save-excursion
    (end-of-line)
    (insert "</" endtag ">")
    (beginning-of-line)
    (skip-white-forward)
    (insert "<" starttag ">")))


;; 2文字のタグを一気に登録。
;; バッククォート ` とカンマ , を使う事で、変数を渡す前に展開できるんだって! elim++
(mapc (lambda (tag)
	(key-chord-define hkc-mode-map tag `(lambda () (interactive) (quote-by-tag ,tag)) ))
      (list
       "dl"
       "dt"
       "dd"
       "ol"
       "ul"
       "li"
       "th"
       "tr"
       "h1"
       "h2"
       "h3"
       "h4"
       "h5"
       "h6"
       "em"))

;; 2文字以外のタグを個別登録

;; テンプレート
;; (key-chord-define hkc-mode-map "2keys" (lambda () (interactive) (quote-by-tag "tagname")) )
;; あなたの .emacs などに書いて、自由に追加できます。
(key-chord-define hkc-mode-map "pp" (lambda () (interactive) (quote-by-tag "p")) )
(key-chord-define hkc-mode-map "bq" (lambda () (interactive) (quote-by-tag "blockquote")) )


(provide 'html-key-chord-mode)

;;; html-key-chord-mode.el ends here
