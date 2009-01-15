;;; html-key-chord-mode.el --- support html tagging key chord
;; -*- coding: utf-8; mode:emacs-lisp -*-

(require 'key-chord)
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


;; 行をタグでくくる関数の対話版
(defun quote-line-by (tag)
  "現在カーソルのある行をミニバッファに入力したタグで囲います。"
  (interactive "*sTag: ") ; s => 文字入力を指示する。
  (quote-by-tag tag))

;; 行をタグでくくる関数の対話版
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

  ;; behavior が t のときは、リージョンをタグでくくる
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
;; 2つ目の lambda 関数の tag までリストが届かない><
;; 届いてないんじゃなくて、展開されずに渡されてしまっていて、呼び出されたときに、何？状態だったみたい
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
(key-chord-define hkc-mode-map "pp" (lambda () (interactive) (quote-by-tag "p")) )
(key-chord-define hkc-mode-map "bq" (lambda () (interactive) (quote-by-tag "blockquote")) )
;; (key-chord-define hkc-mode-map "dl" (lambda () (interactive) (quote-by-tag "dl")) )
;; (key-chord-define hkc-mode-map "dt" (lambda () (interactive) (quote-by-tag "dt")) )
;; (key-chord-define hkc-mode-map "dd" (lambda () (interactive) (quote-by-tag "dd")) )
;; (key-chord-define hkc-mode-map "ul" (lambda () (interactive) (quote-by-tag "ul")) )
;; (key-chord-define hkc-mode-map "li" (lambda () (interactive) (quote-by-tag "li")) )
;; (key-chord-define hkc-mode-map "ol" (lambda () (interactive) (quote-by-tag "ol")) )
;; (key-chord-define hkc-mode-map "h1" (lambda () (interactive) (quote-by-tag "h1")) )
;; (key-chord-define hkc-mode-map "h2" (lambda () (interactive) (quote-by-tag "h2")) )
;; (key-chord-define hkc-mode-map "h3" (lambda () (interactive) (quote-by-tag "h3")) )
;; (key-chord-define hkc-mode-map "h4" (lambda () (interactive) (quote-by-tag "h4")) )
;; (key-chord-define hkc-mode-map "h5" (lambda () (interactive) (quote-by-tag "h5")) )
;; (key-chord-define hkc-mode-map "h6" (lambda () (interactive) (quote-by-tag "h6")) )
;; (key-chord-define hkc-mode-map "em" (lambda () (interactive) (quote-by-tag "em")) )

(provide 'html-key-chord-mode)

;;; html-key-chord-mode.el ends here
