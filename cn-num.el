

(defvar cn-num--digit-alist '(("零" . 0) ("一" . 1) ("二" . 2) ("三" . 3) ("四" . 4) ("五" . 5) ("六" . 6) ("七" . 7) ("八" . 8) ("九" . 9)))
(defvar cn-num--weight-alist '(("" . 0) ("十" . 1) ("百" . 2) ("千" . 3)))
(defvar cn-num--high-weight-alist '(("" . 0) ("万" . 4) ("亿" . 8) ("兆" . 12) ("京" . 16) ("垓" . 20) ("秭" . 24) ("穰" . 28)))

(defun cn-num--get-arabic-digit (chinese-digit)
  (assoc-default chinese-digit cn-num--digit-alist))

(defun cn-num--get-chinese-digit (arabic-digit)
  (or (car (rassoc arabic-digit cn-num--digit-alist)) nil))

(defun cn-num--get-weight-value-by-name (weight-name weight-alist)
  (assoc-default weight-name weight-alist))

(defun cn-num--get-weight-name-by-value (weight-value weight-alist)
  (or (car (rassoc weight-value weight-alist)) nil))

(defun cn-num--get-weight-value (weight-name)
  (cn-num--get-weight-value-by-name weight-name cn-num--weight-alist))

(defun cn-num--get-weight-name (weight-value)
  (cn-num--get-weight-name-by-value weight-value cn-num--weight-alist))

(defun cn-num--get-high-weight-value (weight-name)
  (cn-num--get-weight-value-by-name weight-name cn-num--high-weight-alist))

(defun cn-num--get-high-weight-name (weight-value)
  (cn-num--get-weight-name-by-value weight-value cn-num--high-weight-alist))

(defun cn-num--convert-chinese-to-arabic-iter (rest-str result weight-alist weight-index over-weight-convertor)
  ;;(message "call cn-num--convert-arabic-to-chinese-iter with param: rest-str=%s, result=%d, weight-alist=%s, weight-index=%d, over-weight-convertor=%s."
	   ;;rest-str result weight-alist weight-index over-weight-convertor)
  (setq rest-str (replace-regexp-in-string "零" "" rest-str))
  (if (= (length rest-str) 0)
    result
    (let* ((rest-list)
           (weight-at-index (nth weight-index weight-alist))
           (weight-separator (car weight-at-index))
           (weight-value (cdr weight-at-index)))
      ;;(message "weight-at-index=%s." weight-at-index)
      (if (= 0 weight-value)
	  (+ result (* (funcall over-weight-convertor rest-str) (expt 10 weight-value)))
	(setq rest-list (split-string rest-str weight-separator))
	;;(message "string list after split by %s: %s" weight-separator rest-list)
        (if (not (= 2 (length rest-list)))
          (cn-num--convert-chinese-to-arabic-iter
	    rest-str
            result
            weight-alist
            (- weight-index 1)
            over-weight-convertor)
          (cn-num--convert-chinese-to-arabic-iter
            (cadr rest-list)
            (+ result (* (funcall over-weight-convertor (car rest-list)) (expt 10 weight-value)))
            weight-alist
            (- weight-index 1)
            over-weight-convertor))))))


(defun cn-num--convert-chinese-to-arabic-small-than-10000 (chinese-number)
  (cn-num--convert-chinese-to-arabic-iter chinese-number 0 cn-num--weight-alist (- (length cn-num--weight-alist) 1) 'cn-num--get-arabic-digit))

(defun cn-num--convert-chinese-to-arbic (chinese-number)
  (cn-num--convert-chinese-to-arabic-iter chinese-number 0 cn-num--high-weight-alist (- (length cn-num--high-weight-alist) 1) 'cn-num--convert-chinese-to-arabic-small-than-10000))

(defun cn-num--convert-arabic-to-chinese-iter (arabic-number result weight-alist weight-index over-weight-convertor zero-prefix)
  (message "call cn-num--convert-arabic-to-chinese-iter with param: arabic-number=%d, result=%s, weight-alist=%s, weight-index=%d, over-weight-convertor=%s, zero-prefix=%s."
	   arabic-number result weight-alist weight-index over-weight-convertor zero-prefix)
  (if (< weight-index 0)
      result
      (let* ((weight-at-index (nth weight-index weight-alist))
	     (quotient (/ arabic-number (expt 10 (cdr weight-at-index))))
	     (remainder (% arabic-number (expt 10 (cdr weight-at-index)))))
        ;;(message "arabic-number=%d, weight-at-index=%s, quotient=%d, remainder=%d." arabic-number weight-at-index quotient remainder)
        (if (= 0 quotient)
          (cn-num--convert-arabic-to-chinese-iter
            arabic-number
	    (concat result "零")
            weight-alist
            (- weight-index 1)
            over-weight-convertor
            zero-prefix)
          (let ((newresult (concat result (funcall over-weight-convertor quotient) (car weight-at-index))))
            (cn-num--convert-arabic-to-chinese-iter
              remainder
	      newresult
              weight-alist
              (- weight-index 1)
              over-weight-convertor
              (< 0 (length newresult))))))))

(defun cn-num--compress-chinese-zero(chinese-number enable-zero-prefix)
  (message "compress-chinese-zero: %s." chinese-number)
  (let ((after-compress chinese-number))
    (setq after-compress (replace-regexp-in-string "\\(.*\\)\\([^零]\\)零+$" "\\1\\2" chinese-number))
    (unless enable-zero-prefix
      (setq after-compress (replace-regexp-in-string "^零+\\([^零]\\)\\(.*\\)$" "\\1\\2" after-compress)))
    (setq after-compress (replace-regexp-in-string "零+" "零" after-compress))
    (message "after compress: %s." after-compress)
    after-compress))
          	 
(defun cn-num--convert-arabic-to-chinese-small-than-10000 (chinese-number &optional zero-prefix)
  (cn-num--compress-chinese-zero (cn-num--convert-arabic-to-chinese-iter chinese-number "" cn-num--weight-alist (- (length cn-num--weight-alist) 1) 'cn-num--get-chinese-digit zero-prefix) zero-prefix))

(defun cn-num--convert-arabic-to-chinese (chinese-number)
  (cn-num--compress-chinese-zero (cn-num--convert-arabic-to-chinese-iter chinese-number "" cn-num--high-weight-alist (- (length cn-num--high-weight-alist) 1) (lambda (x) (cn-num--convert-arabic-to-chinese-small-than-10000 x t)) nil) nil))

(provide 'cn-num)
