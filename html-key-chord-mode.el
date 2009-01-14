;;; html-key-chord-mode.el --- support html tagging key chord
;; -*- coding: utf-8; mode:emacs-lisp -*-

(require 'key-chord)
(defvar html-key-chord-mode nil) ; mode 変数。これで状態判定
(defvar hkc-mode-map
  (let ((map (make-sparse-keymap)))
    
    (define-key map "\C-ca" 'ask-tagname)

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

;; 受けとったタグでくくる関数
(defun quote-line-by-tag (tag)
  (save-excursion
    (end-of-line)
    (insert "</" tag ">")
    (beginning-of-line)
    (skip-white-forward)
    (insert "<" tag ">")))

;; debug func
(defun ask-tagname (quotetag)
  (interactive "*sTag: ") ; s => 文字入力を指示する。
  (message quotetag))

;; 2文字のタグを一気に登録。
(setq taglist (list
	       "dl"
	       "dt"
	       "dd"
	       "ol"
	       "ul"
	       "li"
	       "th"
	       "h1"
	       "h2"
	       "h3"
	       "h4"
	       "h5"
	       "h6"
	       "em"))

(mapc (lambda (tag)
	(key-chord-define-global tag (lambda () (interactive) (quote-line-by-tag tag)) ))
      taglist)

;; 2文字以外のタグを個別登録
;; テンプレート
;; (key-chord-define hkc-mode-map "2keys" (lambda () (interactive) (quote-line-by-tag "tagname")) )
(key-chord-define hkc-mode-map "pp" (lambda () (interactive) (quote-line-by-tag "p")) )
(key-chord-define hkc-mode-map "bq" (lambda () (interactive) (quote-line-by-tag "blockquote")) )

(provide 'html-key-chord-mode)

;;; html-key-chord-mode.el ends here
