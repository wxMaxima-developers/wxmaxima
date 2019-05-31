(format t "<suppressOutput>")
;; wxMaxima xml format (based on David Drysdale MathML printing)
;; Andrej Vodopivec,  2004-2014
;; Gunter Königsmann, 2014-2018
;;  SPDX-License-Identifier: GPL-2.0+

;; MathML-printing
;; Created by David Drysdale (DMD), December 2002/January 2003
;;
;; closely based on the original TeX conversion code in mactex.lisp,
;; for which the following credits apply:
;;   (c) copyright 1987, Richard J. Fateman
;;   small corrections and additions: Andrey Grozin, 2001
;;   additional additions: Judah Milgram (JM), September 2001
;;   additional corrections: Barton Willis (BLW), October 2001
;; Method:

;; Producing wxml from a Maxima internal expression is done by
;; a reversal of the parsing process.  Fundamentally, a
;; traversal of the expression tree is produced by the program,
;; with appropriate substitutions and recognition of the
;; infix / prefix / postfix / matchfix relations on symbols. Various
;; changes are made to this so that MathML will like the results.

;(format t "<wxxml-start/>")

;; This is necessary to make file and directory names that contain special characters
;; work under windows.
(progn
  #+sbcl (setf sb-impl::*default-external-format* :UTF-8)

  (in-package :maxima)

  (declare-top
   (special lop rop $inchar)
   (*expr wxxml-lbp wxxml-rbp))

  ;; Use rounded parenthesis for matrices by default
  (setq $lmxchar #\()
  (setq $rmxchar #\()

  ;; A few variables whose value can be configured from wxMaxima
  (defvar *wx-plot-num* 0 "The serial number of the current plot")
  (defvar $wxfilename "" "The filename of the current wxMaxima worksheet")
  (defvar $wxdirname "" "The directory the current wxMaxima worksheet lies in")
  (defvar $wxanimate_autoplay nil "Automatically playback new animations?")
  (defvar wxUserConfDir "" "The location wxMaxima looks for maxima's config files in")
  (defvar wxHelpDir "" "The location wxMaxima searches for help files in")
  (defvar wxMaximaLispLocation "" "The location wxMaxima searches for lisp files in")
  (defvar $wxplot_size '((mlist simp) 800 600) "The size of new plots")
  (defvar $wxmaximaversion t "The wxMaxima version")
  (defprop $wxmaximaversion read-only-assign assign)
  (defvar $wxwidgetsversion t "The wxWidgets version wxMaxima is using.")
  (defvar $wxsubscripts t
    "Recognize TeX-style subscripts")
  (defvar $wxplot_pngcairo nil "Use gnuplot's pngcairo terminal for new plots?")
  (defmvar $wxplot_old_gnuplot nil)


  ;; Escape all chars that need escaping in XML
  (defun wxxml-fix-string (x)
    (if (stringp x)
	(let* ((tmp-x (string-substitute "&amp;" #\& x))
	       (tmp-x (string-substitute "&lt;" #\< tmp-x))
	       (tmp-x (string-substitute "&gt;" #\> tmp-x))
	       (tmp-x (string-substitute "&#13;" #\Return tmp-x))
	       (tmp-x (string-substitute "&#13;" #\Linefeed tmp-x))
	       (tmp-x (string-substitute "&#13;" #\Newline tmp-x)))
	  tmp-x)
      x))

  ;; Allow the user to communicate what to display in the statusbar whilst
  ;; the current program is running
  (defun $wxstatusbar (&rest status)
    #+clisp (finish-output)
    (format t "<statusbar>~a</statusbar>~%" (wxxml-fix-string
					     (apply '$sconcat status)))
    #+clisp (finish-output)
    )


;;; Without this command encountering unicode characters might cause
;;; Maxima to stop responding on windows.
  #+(and clisp win32) (setf (stream-external-format *socket-connection*) charset:utf-8)
  #+(and clisp win32) (setf custom:*default-file-encoding* charset:utf-8)

;;; Muffle compiler-notes globally
  #+sbcl (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
  (defmacro no-warning (form)
    #+sbcl `(handler-bind
		((style-warning #'muffle-warning)
		 (sb-ext:compiler-note #'muffle-warning))
	      ,form)
    #+clisp `(let ((custom:*suppress-check-redefinition* t)) ,form)
    #-(or sbcl clisp) `(progn ,form))

  (defun read-wxmaxima-version (v)
    (let* ((d1 (position #\. v))
	   (year (subseq v 0 d1))
	   (d2 (position #\. v :start (1+ d1)))
	   (month (subseq v (1+ d1) d2))
	   (rest (subseq v (1+ d2))))
      (list '(mlist simp) (parse-integer year) (parse-integer month) rest)))

  (defvar $maxima_frontend nil)
  (defvar $maxima_frontend_version nil)
  (setf (symbol-value '$maxima_frontend) "wxMaxima")

  (defun $wxbuild_info ()
    (let ((year (sixth cl-user:*maxima-build-time*))
	  (month (fifth cl-user:*maxima-build-time*))
	  (day (fourth cl-user:*maxima-build-time*))
	  (hour (third cl-user:*maxima-build-time*))
	  (minute (second cl-user:*maxima-build-time*))
	  (seconds (first cl-user:*maxima-build-time*)))
      (format t "wxMaxima version: ~a~%" $wxmaximaversion)
      (format t "using wxWidgets version: ~a~%" $wxwidgetsversion)
      (format t "Maxima version: ~a~%" *autoconf-version*)
      (format t "Maxima build date: ~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d~%"
	      year month day hour minute seconds)
      (format t "Host type: ~a~%" *autoconf-host*)
      (format t "System type: ~a ~a ~a~%" (software-type) (software-version) (machine-type))
      (format t "Lisp implementation type: ~a~%" (lisp-implementation-type))
      (format t "Lisp implementation version: ~a~%" (lisp-implementation-version))
      (format t "~%~%wxMaxima's idea of the directory layout is:~%User configuration dir: ~a~%" wxUserConfDir)
      (format t "Help dir: ~a~%" wxHelpDir))
    "")

  (defmfun $wxbug_report ()
    (format t "wxMaxima is a graphical front end for Maxima, which does the mathematics in the background.~%")
    (format t "If you encounter a mathematical problem, it is probably a Maxima bug und should be submitted there.~%")
    (format t "~%The Maxima bug database is available at~%")
    (format t "    https://sourceforge.net/p/maxima/bugs~%")
    (format t "Submit bug reports by following the 'Create Ticket' link on that page.~%")
    (format t "To report a Maxima bug, you must have a Sourceforge account.~%~%")
    (format t "A problem in the graphical user interface is probably a wxMaxima bug.~%")
    (format t "The wxMaxima bug database is available at~%")
    (format t "    https://github.com/wxMaxima-developers/wxmaxima/issues?direction=desc&sort=created&state=open~%")
    (format t "Submit bug reports by following the 'New issue' link on that page.~%~%")
    (format t "Please check before submitting, if your bug was already reported.~%~%")
    (format t "Please include the following information with your bug report:~%")
    (format t "-------------------------------------------------------------~%")
    ($wxbuild_info)
    (format t "-------------------------------------------------------------~%"))

  (defvar *var-tag* '("<v>" "</v>"))

  (defun wxxml-get (x p)
    (if (symbolp x) (get x p)))

  (defun wxxml-array (x l r &aux f)
    (if (eq 'mqapply (caar x))
	(setq f (cadr x)
	      x (cdr x)
	      l (wxxml f (append l (list "<i><p>")) (list "</p>")
		       'mparen 'mparen))
      (setq f (caar x)
	    l (wxxml f (append l '("<i><r>"))
		     (list "</r>") lop 'mfunction)))
    (setq r (nconc (wxxml-list (cdr x) (list "<r>")
			       (list "</r></i>") "<v>,</v>") r))
    (nconc l r))

  (defmacro make-tag (val tag)
    ``((wxxmltag simp) ,,val ,,tag))

  (defun $wxxmltag (val tag)
    (make-tag ($sconcat val) ($sconcat tag)))

  (defun string-substitute (newstring oldchar x &aux matchpos)
    (setq matchpos (position oldchar x))
    (if (null matchpos) x
      (concatenate 'string
		   (subseq x 0 matchpos)
		   newstring
		   (string-substitute newstring oldchar
				      (subseq x (1+ matchpos))))))

;;; First we have the functions which are called directly by wxxml and its
;;; descendants
  (defun $wxdeclare_subscript (x &optional (opt t))
    (unless (listp x)
      (setq x (list '(mlist simp) x)))
    (dolist (s (cdr x))
      ($put s opt '$wxxml_subscript))
    opt)

  (defun $wxdeclare_subscripted (x &optional (opt t))
    (unless (listp x)
      (setq x (list '(mlist simp) x)))
    (dolist (s (cdr x))
      ($put s opt '$wxxml_subscripted))
    opt)

  (defun subscriptp (x)
    (unless (symbolp x)
      (return-from subscriptp x))
    (let* ((name (subseq (maybe-invert-string-case (symbol-name x)) 1))
	   (pos (search "_" name :from-end t))
	   #-gcl (*readtable* (copy-readtable nil)))
      #-gcl (setf (readtable-case *readtable*) :invert)
      (when pos
	(let* ((sub (subseq name (+ pos 1)))
	       (sub-var (subseq name 0 pos))
	       (sub-var-symb (read-from-string (concatenate 'string "$" sub-var)))
	       (sub-symb (read-from-string (concatenate 'string "$" sub)))
	       (sub-int (ignore-errors
			  (parse-integer sub))))
	  (when (and (> (length sub-var) 0)
		     (or sub-int
			 (eq $wxsubscripts '$all)
			 (= (length sub) 1)
			 (= (length sub-var) 1)
			 ($get x '$wxxml_subscripted)
			 ($get sub-symb '$wxxml_subscript)))
	    (format nil  "<i altCopy=\"~{~a~}\"><r>~a</r><r>~a</r></i>"
		    (mstring x)
		    (or (get sub-var-symb 'wxxmlword)
			(format nil "<v>~a</v>" sub-var))
		    (if sub-int
			(format nil "<n>~a</n>" sub-int)
                      (format nil "<v>~a</v>" sub))))))))

  (defun wxxmlnumformat (atom)
    (let (r firstpart exponent)
      (cond ((integerp atom)
	     (format nil "<n>~{~c~}</n>" (exploden atom)))
	    (t
	     (setq r (exploden atom))
	     (setq exponent (member 'e r :test #'string-equal))
	     (cond ((null exponent)
		    (format nil "<n>~{~c~}</n>" r))
		   (t
		    (setq firstpart
			  (nreverse (cdr (member 'e (reverse r)
						 :test #'string-equal))))
		    (if (char= (cadr exponent) #\+)
			(setq exponent (cddr exponent))
		      (setq exponent (cdr exponent)))
		    (format nil
			    "<r><n>~{~c~}</n><h>*</h><e><n>10</n><n>~{~c~}</n></e></r>"
			    firstpart exponent)))))))

  (defun wxxml-stripdollar (sym &aux pname)
    (or (symbolp sym)
	(return-from wxxml-stripdollar
		     (wxxml-fix-string (format nil "~a" sym))))
    (setq pname (maybe-invert-string-case (symbol-name sym)))
    (setq pname (cond ((and (> (length pname) 0)
			    (member (elt pname 0) '(#\$ #\&) :test #'eq))
		       (subseq pname 1))
		      ((and (> (length pname) 0)
			    (equal (elt pname 0) #\%))
		       (if $noundisp
			   (concatenate 'string "'"
					(subseq pname 1))
			 (subseq pname 1)))
		      ($lispdisp
		       (concatenate 'string "?" pname))
		      (t pname)))
    (setq pname (wxxml-fix-string pname))
    (concatenate 'string (car *var-tag*) pname (cadr *var-tag*)))

  (defun wxxml-atom (x l r &aux tmp-x)
    (append l
	    (list (cond ((numberp x) (wxxmlnumformat x))
			((and (symbolp x) (get x 'wxxmlword)))
			((and (symbolp x) (get x 'reversealias))
			 (wxxml-stripdollar (get x 'reversealias)))
			((stringp x)
			 (setq tmp-x (wxxml-fix-string x))
			 (if (and (boundp '$stringdisp) $stringdisp)
			     (setq tmp-x (format nil "\"~a\"" tmp-x)))
			 (concatenate 'string "<st>" tmp-x "</st>"))
			((arrayp x)
			 (format nil "<v>#{Lisp array [~{~a~^,~}]}</v>"
				 (array-dimensions x)))
			((functionp x)
			 (format nil "<v>~a</v>"
				 (wxxml-fix-string
				  (stripdollar
				   (maybe-invert-string-case (format nil "~A" x))))))
			((streamp x)
			 (format nil "<v>#{Stream [~A]</v>}"
				 (stream-element-type x)))
			((member (type-of x) '(GRAPH DIGRAPH))
			 (format nil "<v>~a</v>" x))
			((typep x 'structure-object)
			 (let ((tmp-string (format nil "~s" x)))
			   (format nil "<st>~a</st>" (wxxml-fix-string tmp-string))))
			((hash-table-p x)
			 (format nil "<v>#{HashTable}</v>"))
			((and $wxsubscripts (subscriptp x)))
			(t (wxxml-stripdollar x))))
	    r))

  ;; we could patch this so sin x rather than sin(x), but instead we made
  ;; sin a prefix operator
  (defun wxxml-function (x l r)
    (setq l
	  (let ((*var-tag* '("<fnm>" "</fnm>")))
	    (wxxml (caar x) (append l '("<fn>"))
		   nil 'mparen 'mparen))
	  r (wxxml (cons '(mprogn) (cdr x)) nil (append '("</fn>") r)
		   'mparen 'mparen))
    (append l r))

  (defun wxxml-defstruct (x l r)
    (let ((L1 (cdr (get (caar x) 'defstruct-template)))
	  (L2 (cdr x)))
      (wxxml-function
       (cons (car x)
	     (mapcar #'(lambda (e1 e2) (if (eq e1 e2) e1 `((mequal) ,e1 ,e2))) L1 L2))
       l r)))

  (defun wxxml-matchfix-dim (x l r)
    (setq l (append l
		    (list (wxxml-dissym-to-string (car (get (caar x) 'dissym)))))
	  r (append (list (wxxml-dissym-to-string (cdr (get (caar x) 'dissym))))
		    r)
	  x (wxxml-list (cdr x) nil r "<t>,</t>"))
    (append l x))

  (defun wxxml-nary (x l r)
    (let* ((op (caar x))
	   (sym (cond ((member op '(mtimes wxtimes) :test #'eq)
		       (if $stardisp
			   "<t>*</t>"
			 "<h>*</h>"))
					;((wxxmlsym op))
		      ((eq (get op 'dimension) 'dimension-nary)
		       (wxxml-dissym-to-string (get op 'dissym)))))
	   (y (cdr x))
	   (ext-lop lop)
	   (ext-rop rop))
      (cond ((null y)
	     (wxxml-function x l r)) ; this should not happen
	    ((null (cdr y))
	     (wxxml-function x l r)) ; this should not happen, too
	    (t (do ((nl) (lop ext-lop op)
		    (rop op (if (null (cdr y)) ext-rop op)))
		   ((null (cdr y))
		    (setq nl (nconc nl (wxxml (car y) l r lop rop))) nl)
		   (setq nl (nconc nl (wxxml (car y) l (list sym) lop rop))
			 y (cdr y)
			 l nil))))))

  (defun wxxml (x l r lop rop)
    ;; x is the expression of interest; l is the list of strings to its
    ;; left, r to its right. lop and rop are the operators on the left
    ;; and right of x in the tree, and will determine if parens must
    ;; be inserted
    (setq x (nformat x))
    (cond ((atom x) (wxxml-atom x l r))
	  ((not (listp (car x)))
	   (wxxml (cons '(mlist simp) x) l r lop rop))
	  ((or (<= (wxxml-lbp (caar x)) (wxxml-rbp lop))
	       (> (wxxml-lbp rop) (wxxml-rbp (caar x))))
	   (wxxml-paren x l r))
	  ;; special check needed because macsyma notates arrays peculiarly
	  ((member 'array (cdar x) :test #'eq) (wxxml-array x l r))
	  ;; dispatch for object-oriented wxxml-ifiying
	  ((wxxml-get (caar x) 'wxxml) (funcall (get (caar x) 'wxxml) x l r))
	  ((equal (wxxml-get (caar x) 'dimension) 'dimension-infix)
	   (wxxml-infix x l r))
	  ((equal (wxxml-get (caar x) 'dimension) 'dimension-match)
	   (wxxml-matchfix-dim x l r))
	  ((equal (wxxml-get (caar x) 'dimension) 'dimension-nary)
	   (wxxml-nary x l r))
	  ((equal (wxxml-get (caar x) 'dimension) 'dimension-postfix)
	   (wxxml-postfix x l r))
	  ((wxxml-get (caar x) 'defstruct-template)
	   (wxxml-defstruct x l r))
	  (t (wxxml-function x l r))))

  (defun wxxml-paren (x l r)
    (wxxml x (append l '("<r><p>")) (cons "</p></r>" r) 'mparen 'mparen))

  ;; set up a list , separated by symbols (, * ...)  and then tack on the
  ;; ending item (e.g. "]" or perhaps ")"
  (defun wxxml-list (x l r sym)
    (if (null x) r
      (do ((nl))
	  ((null (cdr x))
	   (setq nl (nconc nl (wxxml (car x)  l r 'mparen 'mparen)))
	   nl)
	  (setq nl (nconc nl (wxxml (car x)  l (list sym) 'mparen 'mparen))
		x (cdr x)
		l nil))))

;;; Now we have functions which are called via property lists

  (defun wxxml-prefix (x l r)
    (wxxml (cadr x) (append l (wxxmlsym (caar x))) r (caar x) rop))

  (defun wxxml-infix (x l r)
    ;; check for 2 args
    (if (or (null (cddr x)) (cdddr x)) (wna-err (caar x)))
    (setq l (wxxml (cadr x) l nil lop (caar x)))
    (wxxml (caddr x) (append l (wxxmlsym (caar x))) r (caar x) rop))

  (defun wxxml-postfix (x l r)
    (wxxml (cadr x) l (append (wxxmlsym (caar x)) r) lop (caar x)))

  (defun wxxml-nofix (x l r) (wxxml (caar x) l r (caar x) rop))

  (defun wxxml-matchfix (x l r)
    (setq l (append l (car (wxxmlsym (caar x))))
	  ;; car of wxxmlsym of a matchfix operator is the lead op
	  r (append (cdr (wxxmlsym (caar x))) r)
	  ;; cdr is the trailing op
	  x (wxxml-list (cdr x) nil r "<t>,</t>"))
    (append l x))


  (defun wxxml-dissym-to-string (lst &aux pname)
    (setq pname
	  (wxxml-fix-string (format nil "~{~a~}" lst)))
    (concatenate 'string "<v>" pname "</v>"))

  (defun wxxmlsym (x)
    (or (get x 'wxxmlsym)
	(get x 'strsym)
	(and (get x 'dissym)
	     (list (wxxml-dissym-to-string (get x 'dissym))))
	(list (stripdollar x))))

  (defun wxxmlword (x)
    (or (get x 'wxxmlword)
	(stripdollar x)))

  (defprop bigfloat wxxml-bigfloat wxxml)

  ;;(defun mathml-bigfloat (x l r) (declare (ignore l r)) (fpformat x))
  (defun wxxml-bigfloat (x l r)
    (append l '("<n>") (fpformat x) '("</n>") r))

  (defprop mprog  "<fnm>block</fnm>" wxxmlword)
  (defprop $true  "<t>true</t>"  wxxmlword)
  (defprop $false "<t>false</t>" wxxmlword)

  (defprop mprogn wxxml-matchfix wxxml)
  (defprop mprogn (("<r><p>") "</p></r>") wxxmlsym)

  (defprop mlist wxxml-matchfix wxxml)
  (defprop mlist (("<r><t>[</t>")"<t>]</t></r>") wxxmlsym)

  (defprop $set wxxml-matchfix wxxml)
  (defprop $set (("<r><t>{</t>")"<t>}</t></r>") wxxmlsym)

  (defprop mabs wxxml-matchfix wxxml)
  (defprop mabs (("<r><a>")"</a></r>") wxxmlsym)

  (defprop $conjugate wxxml-matchfix wxxml)
  (defprop $conjugate (("<r><cj>")"</cj></r>") wxxmlsym)

  (defprop %conjugate wxxml-matchfix wxxml)
  (defprop %conjugate (("<r><cj>")"</cj></r>") wxxmlsym)

  (defprop mbox wxxml-mbox wxxml)
  (defprop mlabox wxxml-mbox wxxml)

  (defprop mbox 10. wxxml-rbp)
  (defprop mbox 10. wxxml-lbp)

  (defprop mlabbox 10. wxxml-rbp)
  (defprop mlabbox 10. wxxml-lbp)

  (defun wxxml-mbox (x l r)
    (setq l (wxxml (cadr x) (append l '("<r><hl>")) nil 'mparen 'mparen)
	  r (append '("</hl></r>") r))
    (append l r))

  (defprop mqapply wxxml-mqapply wxxml)

  (defun wxxml-mqapply (x l r)
    (setq l (wxxml (cadr x) (append l '("<fn>"))
		   (list "<p>" ) lop 'mfunction)
	  r (wxxml-list (cddr x) nil (cons "</p></fn>" r) "<t>,</t>"))
    (append l r))


  (defprop $zeta "<g>zeta</g>" wxxmlword)
  (defprop %zeta "<g>zeta</g>" wxxmlword)

  ;;
  ;; Greek characters
  ;;
  (defprop $%alpha "<g>%alpha</g>" wxxmlword)
  (defprop $alpha "<g>alpha</g>" wxxmlword)
  (defprop $%beta "<g>%beta</g>" wxxmlword)
  (defprop $beta "<g>beta</g>" wxxmlword)
  (defprop $%gamma "<g>%gamma</g>" wxxmlword)
  (defprop %gamma "<g>gamma</g>" wxxmlword)
  (defprop $%delta "<g>%delta</g>" wxxmlword)
  (defprop $delta "<g>delta</g>" wxxmlword)
  (defprop $%epsilon "<g>%epsilon</g>" wxxmlword)
  (defprop $epsilon "<g>epsilon</g>" wxxmlword)
  (defprop $%zeta "<g>%zeta</g>" wxxmlword)
  (defprop $%eta "<g>%eta</g>" wxxmlword)
  (defprop $eta "<g>eta</g>" wxxmlword)
  (defprop $%theta "<g>%theta</g>" wxxmlword)
  (defprop $theta "<g>theta</g>" wxxmlword)
  (defprop $%iota "<g>%iota</g>" wxxmlword)
  (defprop $iota "<g>iota</g>" wxxmlword)
  (defprop $%kappa "<g>%kappa</g>" wxxmlword)
  (defprop $kappa "<g>kappa</g>" wxxmlword)
  (defprop $%lambda "<g>%lambda</g>" wxxmlword)
  (defprop $lambda "<g>lambda</g>" wxxmlword)
  (defprop $%mu "<g>%mu</g>" wxxmlword)
  (defprop $mu "<g>mu</g>" wxxmlword)
  (defprop $%nu "<g>%nu</g>" wxxmlword)
  (defprop $nu "<g>nu</g>" wxxmlword)
  (defprop $%xi "<g>%xi</g>" wxxmlword)
  (defprop $xi "<g>xi</g>" wxxmlword)
  (defprop $%omicron "<g>%omicron</g>" wxxmlword)
  (defprop $omicron "<g>omicron</g>" wxxmlword)
  (defprop $%pi "<s>%pi</s>" wxxmlword)
  (defprop $pi "<g>pi</g>" wxxmlword)
  (defprop $%rho "<g>%rho</g>" wxxmlword)
  (defprop $rho "<g>rho</g>" wxxmlword)
  (defprop $%sigma "<g>%sigma</g>" wxxmlword)
  (defprop $sigma "<g>sigma</g>" wxxmlword)
  (defprop $%tau "<g>%tau</g>" wxxmlword)
  (defprop $tau "<g>tau</g>" wxxmlword)
  (defprop $%upsilon "<g>%upsilon</g>" wxxmlword)
  (defprop $upsilon "<g>upsilon</g>" wxxmlword)
  (defprop $%phi "<g>%phi</g>" wxxmlword)
  (defprop $phi "<g>phi</g>" wxxmlword)
  (defprop $%chi "<g>%chi</g>" wxxmlword)
  (defprop $chi "<g>chi</g>" wxxmlword)
  (defprop $%psi "<g>%psi</g>" wxxmlword)
  (defprop $psi "<g>psi</g>" wxxmlword)
  (defprop $%omega "<g>%omega</g>" wxxmlword)
  (defprop $omega "<g>omega</g>" wxxmlword)
  (defprop |$%Alpha| "<g>%Alpha</g>" wxxmlword)
  (defprop |$Alpha| "<g>Alpha</g>" wxxmlword)
  (defprop |$%Beta| "<g>%Beta</g>" wxxmlword)
  (defprop |$Beta| "<g>Beta</g>" wxxmlword)
  (defprop |$%Gamma| "<g>%Gamma</g>" wxxmlword)
  (defprop |$Gamma| "<g>Gamma</g>" wxxmlword)
  (defprop |$%Delta| "<g>%Delta</g>" wxxmlword)
  (defprop |$Delta| "<g>Delta</g>" wxxmlword)
  (defprop |$%Epsilon| "<g>%Epsilon</g>" wxxmlword)
  (defprop |$Epsilon| "<g>Epsilon</g>" wxxmlword)
  (defprop |$%Zeta| "<g>%Zeta</g>" wxxmlword)
  (defprop |$Zeta| "<g>Zeta</g>" wxxmlword)
  (defprop |$%Eta| "<g>%Eta</g>" wxxmlword)
  (defprop |$Eta| "<g>Eta</g>" wxxmlword)
  (defprop |$%Theta| "<g>%Theta</g>" wxxmlword)
  (defprop |$Theta| "<g>Theta</g>" wxxmlword)
  (defprop |$%Iota| "<g>%Iota</g>" wxxmlword)
  (defprop |$Iota| "<g>Iota</g>" wxxmlword)
  (defprop |$%Kappa| "<g>%Kappa</g>" wxxmlword)
  (defprop |$Kappa| "<g>Kappa</g>" wxxmlword)
  (defprop |$%Lambda| "<g>%Lambda</g>" wxxmlword)
  (defprop |$Lambda| "<g>Lambda</g>" wxxmlword)
  (defprop |$%Mu| "<g>%Mu</g>" wxxmlword)
  (defprop |$Mu| "<g>Mu</g>" wxxmlword)
  (defprop |$%Nu| "<g>%Nu</g>" wxxmlword)
  (defprop |$Nu| "<g>Nu</g>" wxxmlword)
  (defprop |$%Xi| "<g>%Xi</g>" wxxmlword)
  (defprop |$Xi| "<g>Xi</g>" wxxmlword)
  (defprop |$%Omicron| "<g>%Omicron</g>" wxxmlword)
  (defprop |$Omicron| "<g>Omicron</g>" wxxmlword)
  (defprop |$%Rho| "<g>%Rho</g>" wxxmlword)
  (defprop |$Rho| "<g>Rho</g>" wxxmlword)
  (defprop |$%Sigma| "<g>%Sigma</g>" wxxmlword)
  (defprop |$Sigma| "<g>Sigma</g>" wxxmlword)
  (defprop |$%Tau| "<g>%Tau</g>" wxxmlword)
  (defprop |$Tau| "<g>Tau</g>" wxxmlword)
  (defprop |$%Upsilon| "<g>%Upsilon</g>" wxxmlword)
  (defprop |$Upsilon| "<g>Upsilon</g>" wxxmlword)
  (defprop |$%Phi| "<g>%Phi</g>" wxxmlword)
  (defprop |$Phi| "<g>Phi</g>" wxxmlword)
  (defprop |$%Chi| "<g>%Chi</g>" wxxmlword)
  (defprop |$Chi| "<g>Chi</g>" wxxmlword)
  (defprop |$%Psi| "<g>%Psi</g>" wxxmlword)
  (defprop |$Psi| "<g>Psi</g>" wxxmlword)
  (defprop |$%Omega| "<g>%Omega</g>" wxxmlword)
  (defprop |$Omega| "<g>Omega</g>" wxxmlword)
  (defprop |$%Pi| "<g>%Pi</g>" wxxmlword)
  (defprop |$Pi| "<g>Pi</g>" wxxmlword)

  (defprop $%i "<s>%i</s>" wxxmlword)
  (defprop $%e "<s>%e</s>" wxxmlword)
  (defprop $inf "<s>inf</s>" wxxmlword)
  (defprop $minf "<t>-</t><s>inf</s>" wxxmlword)

  (defprop mreturn "return" wxxmlword)

  (defprop mquote wxxml-prefix wxxml)
  (defprop mquote ("<t>'</t>") wxxmlsym)
  (defprop mquote "<t>'</t>" wxxmlword)
  (defprop mquote 201. wxxml-rbp)

  (defprop msetq wxxml-infix wxxml)
  (defprop msetq ("<t>:</t>") wxxmlsym)
  (defprop msetq "<t>:</t>" wxxmlword)
  (defprop msetq 180. wxxml-rbp)
  (defprop msetq 20. wxxml-rbp)

  (defprop mset wxxml-infix wxxml)
  (defprop mset ("<t>::</t>") wxxmlsym)
  (defprop mset "<t>::</t>" wxxmlword)
  (defprop mset 180. wxxml-lbp)
  (defprop mset 20. wxxml-rbp)

  (defprop mdefine wxxml-infix wxxml)
  (defprop mdefine ("<t>:=</t>") wxxmlsym)
  (defprop mdefine "<t>:=</t>" wxxmlword)
  (defprop mdefine 180. wxxml-lbp)
  (defprop mdefine 20. wxxml-rbp)

  (defprop mdefmacro wxxml-infix wxxml)
  (defprop mdefmacro ("<t>::=</t>") wxxmlsym)
  (defprop mdefmacro "<t>::=</t>" wxxmlword)
  (defprop mdefmacro 180. wxxml-lbp)
  (defprop mdefmacro 20. wxxml-rbp)

  (defprop marrow wxxml-infix wxxml)
  (defprop marrow ("<t>-></t>") wxxmlsym)
  (defprop marrow "<t>-></t>" wxxmlword)
  (defprop marrow 25 wxxml-lbp)
  (defprop marrow 25 wxxml-rbp)

  (defprop mfactorial wxxml-postfix wxxml)
  (defprop mfactorial ("<t>!</t>") wxxmlsym)
  (defprop mfactorial "<t>!</t>" wxxmlword)
  (defprop mfactorial 160. wxxml-lbp)

  (defprop mexpt wxxml-mexpt wxxml)
  (defprop mexpt 140. wxxml-lbp)
  (defprop mexpt 139. wxxml-rbp)

  (defprop %sum 90. wxxml-rbp)
  (defprop %product 95. wxxml-rbp)

  ;; insert left-angle-brackets for mncexpt. a^<t> is how a^^n looks.

  (defun wxxml-mexpt (x l r)
    (cond ((atom (cadr x))
	   (wxxml-mexpt-simple x l r))
	  ((member 'array (caadr x))
	   (wxxml-mexpt-array x l r))
	  (t
	   (wxxml-mexpt-simple x l r))))

  (defun wxxml-mexpt-array (x l r)
    (let* ((nc (eq (caar x) 'mncexpt))
	   f (xarr (cadr x))
	   (xexp (nformat (caddr x))))
      ;; the index part
      (if (eq 'mqapply (caar xarr))
	  (setq f (cadr xarr)
		xarr (cdr xarr)
		l (wxxml f (append l (list "<ie><p>")) (list "</p>")
			 'mparen 'mparen))
	(setq f (caar xarr)
	      l (wxxml f (append l (if nc
				       (list "<ie mat=\"true\"><r>")
				     (list "<ie><r>")))
		       (list "</r>") lop 'mfunction)))
      (setq  l (append l (wxxml-list (cdr xarr) (list "<r>")
				     (list "</r>") "<v>,</v>")))
      ;; The exponent part
      (setq r (if (mmminusp xexp)
		  ;; the change in base-line makes parens unnecessary
		  (wxxml (cadr xexp) '("<r><v>-</v>")
			 (cons "</r></ie>" r) 'mparen 'mparen)
		(if (and (integerp xexp) (< xexp 10))
		    (wxxml xexp nil
			   (cons "</ie>" r) 'mparen 'mparen)
		  (wxxml xexp (list "<r>")
			 (cons "</r></ie>" r) 'mparen 'mparen)
		  )))
      (append l r)))

  (defun wxxml-mexpt-simple (x l r)
    (let((nc (eq (caar x) 'mncexpt)))
      (setq l (wxxml (cadr x) (append l (if nc
					    '("<e mat=\"true\"><r>")
					  '("<e><r>")))
		     nil lop (caar x))
	    r (if (mmminusp (setq x (nformat (caddr x))))
		  ;; the change in base-line makes parens unnecessary
		  (wxxml (cadr x) '("</r><r><v>-</v>")
			 (cons "</r></e>" r) 'mminus 'mminus)
		(if (and (integerp x) (< x 10))
		    (wxxml x (list "</r>")
			   (cons "</e>" r) 'mparen 'mparen)
		  (wxxml x (list "</r><r>")
			 (cons "</r></e>" r) 'mparen 'mparen)
		  )))
      (append l r)))

  (defprop mncexpt wxxml-mexpt wxxml)

  (defprop mncexpt 135. wxxml-lbp)
  (defprop mncexpt 134. wxxml-rbp)

  (defprop mnctimes wxxml-nary wxxml)
  (defprop mnctimes "<t>.</t>" wxxmlsym)
  (defprop mnctimes "<t>.</t>" wxxmlword)
  (defprop mnctimes 110. wxxml-lbp)
  (defprop mnctimes 109. wxxml-rbp)

  (defprop mtimes wxxml-nary wxxml)
  (defprop mtimes "<h>*</h>" wxxmlsym)
  (defprop mtimes "<t>*</t>" wxxmlword)
  (defprop mtimes 120. wxxml-lbp)
  (defprop mtimes 120. wxxml-rbp)

  (defprop wxtimes wxxml-nary wxxml)
  (defprop wxtimes "<h>*</h>" wxxmlsym)
  (defprop wxtimes "<t>*</t>" wxxmlword)
  (defprop wxtimes 120. wxxml-lbp)
  (defprop wxtimes 120. wxxml-rbp)

  (defprop %sqrt wxxml-sqrt wxxml)

  (defun wxxml-sqrt (x l r)
    (wxxml (cadr x) (append l  '("<q>"))
	   (append '("</q>") r) 'mparen 'mparen))

  (defprop mquotient wxxml-mquotient wxxml)
  (defprop mquotient ("<t>/</t>") wxxmlsym)
  (defprop mquotient "<t>/</t>" wxxmlword)
  (defprop mquotient 122. wxxml-lbp) ;;dunno about this
  (defprop mquotient 123. wxxml-rbp)

  (defun wxxml-mquotient (x l r)
    (if (or (null (cddr x)) (cdddr x)) (wna-err (caar x)))
    (setq l (wxxml (cadr x) (append l '("<f><r>")) nil 'mparen 'mparen)
	  r (wxxml (caddr x) (list "</r><r>")
		   (append '("</r></f>")r) 'mparen 'mparen))
    (append l r))

  (defprop $matrix wxxml-matrix-test wxxml)

  (defun wxxml-matrix-test (x l r)
    (if (every #'$listp (cdr x))
	(wxxml-matrix x l r)
      (wxxml-function x l r)))

  (defun wxxml-matrix(x l r) ;;matrix looks like ((mmatrix)((mlist) a b) ...)
    (cond ((null (cdr x))
	   (append l `("<fn><fnm>matrix</fnm><p/></fn>") r))
	  ((and (null (cddr x))
		(null (cdadr x)))
	   (append l `("<fn><fnm>matrix</fnm><p><t>[</t><t>]</t></p></fn>") r))
	  (t
	   (append l (cond
		      ((find 'inference (car x))
		       (list "<tb inference=\"true\">"))
		      ((find 'special (car x))
		       (list (format nil "<tb special=\"true\" rownames=~s colnames=~s>"
				     (if (find 'rownames (car x)) "true" "false")
				     (if (find 'colnames (car x)) "true" "false"))))
		      ((string= $lmxchar #\()
		       (list "<tb roundedParens=\"true\">"))
		      (t
		       (list "<tb>")))
		   (mapcan #'(lambda (y)
			       (cond ((null (cdr y))
				      (list "<mtr><mtd><mspace/></mtd></mtr>"))
				     (t
				      (wxxml-list (cdr y)
						  (list "<mtr><mtd>")
						  (list "</mtd></mtr>")
						  "</mtd><mtd>"))))
			   (cdr x))
		   `("</tb>") r))))

  ;; macsyma sum or prod is over integer range, not  low <= index <= high
  ;; wxxml is lots more flexible .. but

  (defprop %sum wxxml-sum wxxml)
  (defprop %lsum wxxml-lsum wxxml)
  (defprop %product wxxml-sum wxxml)
  (defprop $sum wxxml-sum wxxml)
  (defprop $lsum wxxml-lsum wxxml)
  (defprop $product wxxml-sum wxxml)

  ;; easily extended to union, intersect, otherops

  (defun wxxml-lsum(x l r)
    (let ((op "<sm type=\"lsum\"><r>")
	  ;; gotta be one of those above
	  (s1 (wxxml (cadr x) nil nil 'mparen rop));; summand
	  (index ;; "index = lowerlimit"
	   (wxxml `((min simp) , (caddr x), (cadddr x))
		  nil nil 'mparen 'mparen)))
      (append l `(,op ,@index
		      "</r><r><mn/></r><r>"
		      ,@s1 "</r></sm>") r)))

  (defun wxxml-sum(x l r)
    (let ((op (if (or (eq (caar x) '%sum)
		      (eq (caar x) '$sum))
		  "<sm><r>"
		"<sm type=\"prod\"><r>"))
	  (s1 (wxxml (cadr x) nil nil 'mparen rop));; summand
	  (index ;; "index = lowerlimit"
	   (wxxml `((mequal simp) ,(caddr x) ,(cadddr x))
		  nil nil 'mparen 'mparen))
	  (toplim (wxxml (car (cddddr x)) nil nil 'mparen 'mparen)))
      (append l `( ,op ,@index "</r><r>" ,@toplim
		       "</r><r>"
		       ,@s1 "</r></sm>") r)))

  (defprop %integrate wxxml-int wxxml)
  (defprop $integrate wxxml-int wxxml)

  (defun wxxml-int (x l r)
    (let ((s1 (wxxml (cadr x) nil nil 'mparen 'mparen));;integrand delims / & d
	  (var (wxxml (caddr x) nil nil 'mparen rop))) ;; variable
      (cond ((= (length x) 3)
	     (append l `("<in def=\"false\"><r>"
			 ,@s1
			 "</r><r><s>d</s>"
			 ,@var
			 "</r></in>") r))
	    (t ;; presumably length 5
	     (let ((low (wxxml (nth 3 x) nil nil 'mparen 'mparen))
		   ;; 1st item is 0
		   (hi (wxxml (nth 4 x) nil nil 'mparen 'mparen)))
	       (append l `("<in><r>"
			   ,@low
			   "</r><r>"
			   ,@hi
			   "</r><r>"
			   ,@s1
			   "</r><r><s>d</s>"
			   ,@var "</r></in>") r))))))

  (defprop %limit wxxml-limit wxxml)

  (defprop mrarr wxxml-infix wxxml)
  (defprop mrarr ("<t>-></t>") wxxmlsym)
  (defprop mrarr 80. wxxml-lbp)
  (defprop mrarr 80. wxxml-rbp)

  (defun wxxml-limit (x l r) ;; ignoring direction, last optional arg to limit
    (let ((s1 (wxxml (second x) nil nil 'mparen rop));; limitfunction
	  (subfun ;; the thing underneath "limit"
	   (wxxml `((mrarr simp) ,(third x)
		    ,(fourth x)) nil nil 'mparen 'mparen)))
      (case (fifth x)
	    ($plus
	     (append l `("<lm><fnm>lim</fnm><r>"
			 ,@subfun "<v>+</v></r><r>"
			 ,@s1 "</r></lm>") r))
	    ($minus
	     (append l `("<lm><fnm>lim</fnm><r>"
			 ,@subfun "<t>-</t></r><r>"
			 ,@s1 "</r></lm>") r))
	    (otherwise
	     (append l `("<lm><fnm>lim</fnm><r>"
			 ,@subfun "</r><r>"
			 ,@s1 "</r></lm>") r)))))

  (defprop %at wxxml-at wxxml)
  ;; e.g.  at(diff(f(x)),x=a)
  (defun wxxml-at (x l r)
    (let ((s1 (wxxml (cadr x) nil nil lop rop))
	  (sub (wxxml (caddr x) nil nil 'mparen 'mparen)))
      (append l '("<at><r>") s1
	      '("</r><r>") sub '("</r></at>") r)))

  ;;binomial coefficients

  (defprop %binomial wxxml-choose wxxml)


  (defun wxxml-choose (x l r)
    `(,@l
      "<p print=\"no\"><f line=\"no\"><r>"
      ,@(wxxml (cadr x) nil nil 'mparen 'mparen)
      "</r><r>"
      ,@(wxxml (caddr x) nil nil 'mparen 'mparen)
      "</r></f></p>"
      ,@r))


  (defprop rat wxxml-rat wxxml)
  (defprop rat 120. wxxml-lbp)
  (defprop rat 121. wxxml-rbp)
  (defun wxxml-rat(x l r) (wxxml-mquotient x l r))

  (defprop mplus wxxml-mplus wxxml)
  (defprop mplus 100. wxxml-lbp)
  (defprop mplus 100. wxxml-rbp)

  (defun wxxml-mplus (x l r)
    (cond ((member 'trunc (car x) :test #'eq)
	   (setq r (cons "<v>+</v><t>...</t>" r))))
    (cond ((null (cddr x))
	   (if (null (cdr x))
	       (wxxml-function x l r)
	     (wxxml (cadr x) l r 'mplus rop)))
	  (t (setq l (wxxml (cadr x) l nil lop 'mplus)
		   x (cddr x))
	     (do ((nl l)  (dissym))
		 ((null (cdr x))
		  (if (mmminusp (car x)) (setq l (cadar x) dissym
					       (list "<v>-</v>"))
		    (setq l (car x) dissym (list "<v>+</v>")))
		  (setq r (wxxml l dissym r 'mplus rop))
		  (append nl r))
		 (if (mmminusp (car x)) (setq l (cadar x) dissym
					      (list "<v>-</v>"))
		   (setq l (car x) dissym (list "<v>+</v>")))
		 (setq nl (append nl (wxxml l dissym nil 'mplus 'mplus))
		       x (cdr x))))))

  (defprop mminus wxxml-prefix wxxml)
  (defprop mminus ("<v>-</v>") wxxmlsym)
  (defprop mminus "<v>-</v>" wxxmlword)
  (defprop mminus 101. wxxml-rbp)
  (defprop mminus 101. wxxml-lbp)

  (defprop $~ wxxml-infix wxxml)
  (defprop $~ ("<t>~</t>") wxxmlsym)
  (defprop $~ "<t>~</t>" wxxmlword)
  (defprop $~ 134. wxxml-lbp)
  (defprop $~ 133. wxxml-rbp)

  (defprop min wxxml-infix wxxml)
  (defprop min ("<fnm>in</fnm>") wxxmlsym)
  (defprop min "<fnm>in</fnm>" wxxmlword)
  (defprop min 80. wxxml-lbp)
  (defprop min 80. wxxml-rbp)

  (defprop mequal wxxml-infix wxxml)
  (defprop mequal ("<v>=</v>") wxxmlsym)
  (defprop mequal "<v>=</v>" wxxmlword)
  (defprop mequal 80. wxxml-lbp)
  (defprop mequal 80. wxxml-rbp)

  (defprop mnotequal wxxml-infix wxxml)
  (defprop mnotequal ("<t>#</t>") wxxmlsym)
  (defprop mnotequal 80. wxxml-lbp)
  (defprop mnotequal 80. wxxml-rbp)

  (defprop mgreaterp wxxml-infix wxxml)
  (defprop mgreaterp ("<t>&gt;</t>") wxxmlsym)
  (defprop mgreaterp "<t>&gt;</t>" wxxmlword)
  (defprop mgreaterp 80. wxxml-lbp)
  (defprop mgreaterp 80. wxxml-rbp)

  (defprop mgeqp wxxml-infix wxxml)
  (defprop mgeqp ("<t>&gt;=</t>") wxxmlsym)
  (defprop mgeqp "<t>&gt;=</t>" wxxmlword)
  (defprop mgeqp 80. wxxml-lbp)
  (defprop mgeqp 80. wxxml-rbp)

  (defprop mlessp wxxml-infix wxxml)
  (defprop mlessp ("<t>&lt;</t>") wxxmlsym)
  (defprop mlessp "<t>&lt;</t>" wxxmlword)
  (defprop mlessp 80. wxxml-lbp)
  (defprop mlessp 80. wxxml-rbp)

  (defprop mleqp wxxml-infix wxxml)
  (defprop mleqp ("<t>&lt;=</t>") wxxmlsym)
  (defprop mleqp "<t>&lt;=</t>" wxxmlword)
  (defprop mleqp 80. wxxml-lbp)
  (defprop mleqp 80. wxxml-rbp)

  (defprop mnot wxxml-prefix wxxml)
  (defprop mnot ("<fnm altCopy=\"not \">not</fnm>") wxxmlsym)
  (defprop mnot "<fnm>not</fnm>" wxxmlword)
  (defprop mnot 70. wxxml-rbp)

  (defprop mand wxxml-nary wxxml)
  (defprop mand "<mspace/><fnm>and</fnm><mspace/>" wxxmlsym)
  (defprop mand "<fnm>and</fnm>" wxxmlword)
  (defprop mand 60. wxxml-lbp)
  (defprop mand 60. wxxml-rbp)

  (defprop mor wxxml-nary wxxml)
  (defprop mor "<mspace/><fnm>or</fnm><mspace/>" wxxmlsym)
  (defprop mor "<fnm>or</fnm>" wxxmlword)
  (defprop mor 50. wxxml-lbp)
  (defprop mor 50. wxxml-rbp)


  (defprop mcond wxxml-mcond wxxml)
  (defprop mcond 25. wxxml-lbp)
  (defprop mcond 25. wxxml-rbp)

  (defprop %derivative wxxml-derivative wxxml)
  (defprop %derivative 120. wxxml-lbp)
  (defprop %derivative 119. wxxml-rbp)

  (defprop $diff wxxml-derivative wxxml)
  (defprop $diff 120. wxxml-lbp)
  (defprop $diff 119. wxxml-rbp)

  (defun wxxml-derivative (x l r)
    (if (and $derivabbrev
	     (every #'integerp (odds (cddr x) 0))
	     (every #'atom (odds (cddr x) 1)))
	(append l (wxxml-d-abbrev x) r)
      (wxxml (wxxml-d x) (append l '("<d>"))
	     (append '("</d>") r) 'mparen 'mparen)))

  (defun $derivabbrev (a)
    (if a
	(progn
	  (defprop %derivative 130. wxxml-lbp)
	  (defprop %derivative 129. wxxml-rbp)
	  (setq $derivabbrev t))
      (progn
	(defprop %derivative 120. wxxml-lbp)
	(defprop %derivative 119. wxxml-rbp)
	(setq $derivabbrev nil))))

  (defun wxxml-d-abbrev-subscript (l_vars l_ords &aux var_xml)
    (let ((sub ()))
      (loop while l_vars do
	    (setq var_xml (car (wxxml (car l_vars) nil nil 'mparen 'mparen)))
	    (loop for i from 1 to (car l_ords) do
		  (setq sub (cons var_xml sub)))
	    (setq l_vars (cdr l_vars)
		  l_ords (cdr l_ords)))
      (reverse sub)))

  (defun wxxml-d-abbrev (x)
    (let*
	((difflist (cddr x))
	 (ords (odds  difflist 0))
	 (ords (cond ((null ords) '(1))
		     (t ords)))
	 (vars (odds difflist 1))
	 (fun (wxxml (cadr x) nil nil 'mparen 'mparen)))
      (append '("<i d=\"1\"><r>") fun '("</r>")
	      '("<r>") (wxxml-d-abbrev-subscript vars ords) '("</r></i>"))))

  (defun wxxml-d (x)
    ;; format the macsyma derivative form so it looks
    ;; sort of like a quotient times the deriva-dand.
    (let*
	(($simp t)
	 (arg (cadr x)) ;; the function being differentiated
	 (difflist (cddr x)) ;; list of derivs e.g. (x 1 y 2)
	 (ords (odds difflist 0)) ;; e.g. (1 2)
	 (ords (cond ((null ords) '(1))
		     (t ords)))
	 (vars (odds difflist 1)) ;; e.g. (x y)
	 (dsym '((wxxmltag simp) "d" "s"))
	 (numer `((mexpt) ,dsym ((mplus) ,@ords))) ; d^n numerator
	 (denom (cons '(mtimes)
		      (mapcan #'(lambda(b e)
				  `(,dsym ,(simplifya `((mexpt) ,b ,e) nil)))
			      vars ords))))
      `((wxtimes)
	((mquotient) ,(simplifya numer nil) ,denom)
	,arg)))

  (defun wxxml-mcond (x l r)
    (let ((res ()))
      (setq res (wxxml (cadr x) '("<fnm>if</fnm><mspace/>")
		       '("<mspace/><fnm>then</fnm><mspace/>") 'mparen 'mparen))
      (setq res (append res (wxxml (caddr x) nil
				   '("<mspace/>") 'mparen 'mparen)))
      (let ((args (cdddr x)))
	(loop while (>= (length args) 2) do
	      (cond
	       ((and (= (length args) 2) (eql (car args) t))
		(unless (or (eql (cadr args) '$false) (null (cadr args)))
		  (setq res (wxxml (cadr args)
				   (append res '("<fnm>else</fnm><mspace/>"))
				   nil 'mparen 'mparen))))
	       (t
		(setq res (wxxml (car args)
				 (append res '("<fnm>elseif</fnm><mspace/>"))
				 (wxxml (cadr args)
					'("<mspace/><fnm>then</fnm><mspace/>")
					'("<mspace/>") 'mparen 'mparen)
				 'mparen 'mparen))))
	      (setq args (cddr args)))
	(append l res r))))

  (defprop mdo wxxml-mdo wxxml)
  (defprop mdo 30. wxxml-lbp)
  (defprop mdo 30. wxxml-rbp)
  (defprop mdoin wxxml-mdoin wxxml)
  (defprop mdoin 30. wxxml-rbp)

  (defun wxxml-lbp (x)
    (cond ((wxxml-get x 'wxxml-lbp))
	  (t(lbp x))))

  (defun wxxml-rbp (x)
    (cond ((wxxml-get x 'wxxml-rbp))
	  (t(lbp x))))

  ;; these aren't quite right

  (defun wxxml-mdo (x l r)
    (wxxml-list (wxxmlmdo x) l r "<mspace/>"))

  (defun wxxml-mdoin (x l r)
    (wxxml-list (wxxmlmdoin x) l r "<mspace/>"))

  (defun wxxmlmdo (x)
    (nconc (cond ((second x) (list (make-tag "for" "fnm") (second x))))
	   (cond ((equal 1 (third x)) nil)
		 ((third x)  (list (make-tag "from" "fnm") (third x))))
	   (cond ((equal 1 (fourth x)) nil)
		 ((fourth x)
		  (list (make-tag "step" "fnm")  (fourth x)))
		 ((fifth x)
		  (list (make-tag "next" "fnm") (fifth x))))
	   (cond ((sixth x)
		  (list (make-tag "thru" "fnm") (sixth x))))
	   (cond ((null (seventh x)) nil)
		 ((eq 'mnot (caar (seventh x)))
		  (list (make-tag "while" "fnm") (cadr (seventh x))))
		 (t (list (make-tag "unless" "fnm") (seventh x))))
	   (list (make-tag "do" "fnm") (eighth x))))

  (defun wxxmlmdoin (x)
    (nconc (list (make-tag "for" "fnm") (second x)
		 (make-tag "in" "fnm") (third x))
	   (cond ((sixth x)
		  (list (make-tag "thru" "fnm") (sixth x))))
	   (cond ((null (seventh x)) nil)
		 ((eq 'mnot (caar (seventh x)))
		  (list (make-tag "while" "fnm") (cadr (seventh x))))
		 (t (list (make-tag "unless" "fnm") (seventh x))))
	   (list (make-tag "do" "fnm") (eighth x))))


  (defun wxxml-matchfix-np (x l r)
    (setq l (append l (car (wxxmlsym (caar x))))
	  ;; car of wxxmlsym of a matchfix operator is the lead op
	  r (append (cdr (wxxmlsym (caar x))) r)
	  ;; cdr is the trailing op
	  x (wxxml-list (cdr x) nil r ""))
    (append l x))

  (defprop text-string wxxml-matchfix-np wxxml)
  (defprop text-string (("<t>")"</t>") wxxmlsym)

  (defprop mtext wxxml-matchfix-np wxxml)
  (defprop mtext (("")"") wxxmlsym)

  (defvar *wxxml-mratp* nil)

  (defun wxxml-mlable (x l r)
    (wxxml (caddr x)
	   (append l
		   (if (cadr x)
		       (list
			(format nil "<lbl>(~A)~A </lbl>"
				(stripdollar (maybe-invert-string-case (symbol-name (cadr x))))
				*wxxml-mratp*))
		     nil))
	   r 'mparen 'mparen))

  (defprop mlable wxxml-mlable wxxml)
  (defprop mlabel wxxml-mlable wxxml)

  (defun wxxml-spaceout (x l r)
    (append l (list " " (make-string (cadr x) :initial-element #\.) "") r))

  (defprop spaceout wxxml-spaceout wxxml)

  (defun mydispla (x)
    #+clisp (finish-output)
    (let ((*print-circle* nil)
	  (*wxxml-mratp* (format nil "~{~a~}" (cdr (checkrat x)))))
      (mapc #'princ
	    (wxxml x '("<mth>") '("</mth>") 'mparen 'mparen)))
    #+clisp (finish-output)
    )

  (setf *alt-display2d* 'mydispla)

  (defun $set_display (tp)
    (cond
     ((eq tp '$none)
      (setq $display2d nil))
     ((eq tp '$ascii)
      (setq $display2d t)
      (setf *alt-display2d* nil))
     ((eq tp '$xml)
      (setq $display2d t)
      (setf *alt-display2d* 'mydispla))
     (t
      (format t "Unknown display type")
      (setq tp '$unknown)))
    tp)

  ;;
  ;; inference_result from the stats package
  ;;

  (defun wxxml-inference (x l r)
    (let ((name (cadr x))
	  (values (caddr x))
	  (dis (cadddr x))
	  (m ()))
      (labels
       ((build-eq (e)
		  `((mequal simp) ,(cadr e) ,(caddr e))))
       (dolist (i (cdr dis))
	 (setq m (append m `(((mlist simp) ,(build-eq (nth i values)))))))
       (setq m (cons `((mlist simp) ,name) m))
       (setq m (cons '($matrix simp inference) m))
       (wxxml m l r 'mparen 'mparen))))

  (defprop $inference_result wxxml-inference wxxml)

  (defun wxxml-amatrix (x l r)
    (let* ((nr ($@-function x '$nr))
	   (nc ($@-function x '$nc))
	   (M (simplifya ($genmatrix
			  `((lambda) ((mlist) i j) (mfuncall '$get_element ,x i j))
			  nr nc)
			 t)))
      (wxxml-matrix M l r)))

  (defprop $amatrix wxxml-amatrix wxxml)

  ;;
  ;; orthopoly functions
  ;;

  (defun wxxml-pochhammer (x l r)
    (let ((n (cadr x))
	  (k (caddr x)))
      (append l
	      (list (format nil "<i altCopy=\"~{~a~}\"><p>" (mstring x)))
	      (wxxml n nil nil 'mparen 'mparen)
	      (list "</p><r>")
	      (wxxml k nil nil 'mparen 'mparen)
	      (list "</r></i>")
	      r)))

  (defprop $pochhammer wxxml-pochhammer wxxml)

  (defun wxxml-orthopoly (x l r)
    (let* ((fun-name (caar x))
	   (disp-name (get fun-name 'wxxml-orthopoly-disp))
	   (args (cdr x)))
      (append l
	      (list (format nil "<fn altCopy=\"~{~a~}\">" (mstring x)))
	      (if (nth 2 disp-name)
		  (list (format nil "<ie><fnm>~a</fnm><r>" (car disp-name)))
		(list (format nil "<i><fnm>~a</fnm><r>" (car disp-name))))
	      (wxxml (nth (nth 1 disp-name) args) nil nil 'mparen 'mparen)
	      (when (nth 2 disp-name)
		(append (list "</r><r>")
			(when (nth 3 disp-name) (list "<p>"))
			(wxxml-list (or (nth 5 disp-name)
					(mapcar (lambda (i) (nth i args)) (nth 2 disp-name)))
				    nil nil ",")
			(when (nth 3 disp-name) (list "</p>"))
			(list "</r>")))
	      (if (nth 2 disp-name)
		  (list "</ie>")
		(list "</r></i>"))
	      (list "<p>")
	      (wxxml-list (mapcar (lambda (i) (nth i args)) (nth 4 disp-name)) nil nil ",")
	      (list "</p></fn>")
	      r)))

  (dolist (ortho-pair
	   '(($laguerre "L" 0 nil nil (1))
	     (%laguerre "L" 0 nil nil (1))
	     ($legendre_p "P" 0 nil nil (1))
	     (%legendre_p "P" 0 nil nil (1))
	     ($legendre_q "Q" 0 nil nil (1))
	     (%legendre_q "Q" 0 nil nil (1))
	     ($chebyshev_t "T" 0 nil nil (1))
	     (%chebyshev_t "T" 0 nil nil (1))
	     ($chebyshev_u "U" 0 nil nil (1))
	     (%chebyshev_u "U" 0 nil nil (1))
	     ($hermite "H" 0 nil nil (1))
	     (%hermite "H" 0 nil nil (1))
	     ($spherical_bessel_j "J" 0 nil nil (1))
	     (%spherical_bessel_j "J" 0 nil nil (1))
	     ($spherical_bessel_y "Y" 0 nil nil (1))
	     (%spherical_bessel_y "Y" 0 nil nil (1))
	     ($assoc_legendre_p "P" 0 (1) nil (2))
	     (%assoc_legendre_p "P" 0 (1) nil (2))
	     ($assoc_legendre_q "Q" 0 (1) nil (2))
	     (%assoc_legendre_q "Q" 0 (1) nil (2))
	     ($jacobi_p "P" 0 (1 2) t (3))
	     (%jacobi_p "P" 0 (1 2) t (3))
	     ($gen_laguerre "L" 0 (1) t (2))
	     (%gen_laguerre "L" 0 (1) t (2))
	     ($spherical_harmonic "Y" 0 (1) nil (2 3))
	     (%spherical_harmonic "Y" 0 (1) nil (2 3))
	     ($ultraspherical "C" 0 (1) t (2))
	     (%ultraspherical "C" 0 (1) t (2))
	     ($spherical_hankel1 "H" 0 t t (1) (1))
	     (%spherical_hankel1 "H" 0 t t (1) (1))
	     ($spherical_hankel2 "H" 0 t t (1) (2))
	     (%spherical_hankel2 "H" 0 t t (1) (2))))
    (setf (get (car ortho-pair) 'wxxml) 'wxxml-orthopoly)
    (setf (get (car ortho-pair) 'wxxml-orthopoly-disp) (cdr ortho-pair)))

;;;
;;; This is the display support only - copy/paste will not work
;;;

  (defmvar $pdiff_uses_prime_for_derivatives nil)
  (defmvar $pdiff_prime_limit 3)
  (defmvar $pdiff_uses_named_subscripts_for_derivatives nil)
  (defmvar $pdiff_diff_var_names (list '(mlist) '|$x| '|$y| '|$z|))

  (setf (get '%pderivop 'wxxml) 'wxxml-pderivop)
  (setf (get '$pderivop 'wxxml) 'wxxml-pderivop)

  (defun wxxml-pderivop (x l r)
    (cond ((and $pdiff_uses_prime_for_derivatives (eq 3 (length x)))
	   (let* ((n (car (last x)))
		  (p))

	     (cond ((<= n $pdiff_prime_limit)
		    (setq p (make-list n :initial-element "'")))
		   (t
		    (setq p (list "(" n ")"))))
	     (append (append l '("<r>"))
		     (let ((*var-tag* (list "<fnm>" "</fnm>"))) (wxxml (cadr x) nil nil lop rop))
		     p
		     (list "</r>")  r)))

	  ((and $pdiff_uses_named_subscripts_for_derivatives
		(< (apply #'+ (cddr x)) $pdiff_prime_limit))
	   (let ((n (cddr x))
		 (v (mapcar #'stripdollar (cdr $pdiff_diff_var_names)))
		 (p))
	     (cond ((> (length n) (length v))
		    (merror "Not enough elements in pdiff_diff_var_names to display the expression")))
	     (dotimes (i (length n))
	       (setq p (append p (make-list (nth i n)
					    :initial-element (nth i v)))))
	     (append (append l '("<i><r>"))
		     (wxxml (cadr x) nil nil lop rop)
		     (list "</r><r>") p (list "</r></i>") r)))
	  (t
	   (append (append l '("<i><r>"))
		   (wxxml (cadr x) nil nil lop rop)
		   (list "</r><r>(")
		   (wxxml-list (cddr x) nil nil ",")
		   (list ")</r></i>") r))))

  ;;
  ;; Plotting support
  ;;

  (defprop wxxmltag wxxml-tag wxxml)

  (defun wxxml-tag (x l r)
    (let ((name (cadr x))
	  (tag (caddr x))
	  (prop (cadddr x)))
      (if prop
	  (append l (list (format nil "<~a ~a>~a</~a>" tag prop name tag)) r)
	(append l (list (format nil "<~a>~a</~a>" tag name tag)) r))))


  (defvar *image-counter* 0)

  ;; A suitable name for a .gnuplot file
  (defun wxplot-gnuplotfilename ()
    (incf *wx-plot-num*)
    (format nil "maxout_~d_~d.gnuplot" (getpid) *wx-plot-num*))

  ;; A suitable name for a .data file
  (defun wxplot-datafilename ()
    (incf *wx-plot-num*)
    (format nil "maxout_~d_~d.data" (getpid) *wx-plot-num*))

  (defun wxplot-filename (&optional (suff t))
    (incf *image-counter*)
    (plot-temp-file (if suff
			(format nil "maxout_~d_~d.png" (getpid) *image-counter*)
		      (format nil "maxout_~d_~d" (getpid) *image-counter*))))

  ;; The "solid" has to be changed to "dashed" as soon as plot() starts
  ;; supporting other line styles than "solid" or "dots".
  (defun $wxplot_preamble ()
    (let ((frmt
	   (cond
	    ($wxplot_old_gnuplot "set terminal png picsize ~d ~d; set zeroaxis;")
	    ($wxplot_pngcairo "set terminal pngcairo solid background \"white\" enhanced font \"arial,10\" fontscale 1.0 size ~d,~d; set zeroaxis;")
	    (t "set terminal png size ~d,~d; set zeroaxis;"))))
      (format nil frmt
	      ($first $wxplot_size)
	      ($second $wxplot_size))))

  (defun $int_range (lo &optional hi (st 1))
    (unless (integerp lo)
      ($error "int_range: first argument is not an integer."))
    (unless (or (null hi) (integerp hi))
      ($error "int_range: second argument is not an integer."))
    (when (null hi)
      (setq hi lo)
      (setq lo 1))
    (cons '(mlist simp) (loop :for i :from lo :to hi :by st :collect i)))

  (defvar *default-framerate* 2)
  (defvar $wxanimate_framerate *default-framerate*)
  (defun slide-tag (images)
    (if (eql *default-framerate* $wxanimate_framerate)
	($ldisp
	 (list '(wxxmltag simp)
	       (wxxml-fix-string (format nil "~{~a;~}" images))
	       "slide"
	       (if (eql $wxanimate_autoplay 't)
		   "running=\"true\" del=\"yes\""
		 "running=\"false\" del=\"yes\""))
	 (format nil "~%"))
      ($ldisp
       (list '(wxxmltag simp)
	     (wxxml-fix-string
	      (format nil "~{~a;~}" images))
	     "slide"
	     (if (eql $wxanimate_autoplay 't)
		 (format nil "fr=\"~a\" running=\"true\" del=\"yes\"" $wxanimate_framerate)
	       (format nil "fr=\"~a\" running=\"false\" del=\"yes\"" $wxanimate_framerate)))
       (format nil "~%"))))

  (defun wxanimate (scene)
    (let* ((scene (cdr scene))
	   (a (car scene))
	   (a-range (meval (cadr scene)))
	   (expr (caddr scene))
	   (args (cdddr scene))
	   (frameno 1)
	   (images ()))
      (when (integerp a-range)
	(setq a-range (cons '(mlist simp) (loop for i from 1 to a-range collect i))))
      (dolist (aval (reverse (cdr a-range)))
	($wxstatusbar (format nil "Preparing Frame #~d" frameno))
	(setf frameno (+ 1 frameno))
	(let ((preamble ($wxplot_preamble))
	      (system-preamble (get-plot-option-string '$gnuplot_preamble 2))
	      (filename (wxplot-filename))
	      (expr (maxima-substitute aval a expr)))
	  (when (string= system-preamble "false")
	    (setq system-preamble ""))
	  (setq preamble (format nil "~a; ~a" preamble system-preamble))
	  (dolist (arg args)
	    (if (and (listp arg) (eql (cadr arg) '$gnuplot_preamble))
		(setq preamble (format nil "~a; ~a"
				       preamble (meval (maxima-substitute aval a (caddr arg)))))))
	  (apply #'$plot2d `(,(meval expr) ,@(mapcar #'meval args)
			     ((mlist simp) $plot_format $gnuplot)
			     ((mlist simp) $gnuplot_term ,(if $wxplot_pngcairo '$pngcairo '$png))
			     ((mlist simp) $gnuplot_preamble ,preamble)
			     ((mlist simp) $gnuplot_out_file ,filename)))
	  (setq images (cons filename images))))
      (when images
	(slide-tag images)))
    "")

  (defmspec $with_slider (scene)
    (wxanimate scene))

  (defmspec $wxanimate (scene)
    (wxanimate scene))

  (defvar *windows-OS* (string= *autoconf-win32* "true"))

  (defun get-file-name-opt (scene)
    (let (opts filename)
      (loop for opt in scene do
	    (if (and (not (atom opt))
		     (eq (caar opt) 'mequal)
		     (eq (cadr opt) '$file_name))
		(setq filename (caddr opt))
	      (setq opts (cons opt opts))))
      (values (reverse opts) filename)))

  (defun get-pic-size-opt ()
    (cond
     ((eq ($get '$draw '$version) 1)
      `(((mequal simp) $pic_width ,($first $wxplot_size))
	((mequal simp) $pic_height ,($second $wxplot_size))))
     (t
      `(((mequal simp) $dimensions ,$wxplot_size)))))

  (defun wxanimate-draw (scenes scene-head)
    (unless ($get '$draw '$version) ($load "draw"))
    (multiple-value-bind (scene file-name) (get-file-name-opt (cdr scenes))
			 (let* ((a (meval (car scene)))
				(a-range (meval (cadr scene)))
				(*windows-OS* t)
				(args (cddr scene))
				(frameno 1)
				(images ()))
			   (when (integerp a-range)
			     (setq a-range (cons '(mlist simp) (loop for i from 1 to a-range collect i))))
			   (if file-name
			       ;; If file_name is set, draw the animation into gif using gnuplot
			       (let (imgs)
				 (dolist (aval (reverse (cdr a-range)))
				   (setq imgs (cons
					       (cons scene-head
						     (mapcar #'(lambda (arg) (meval (maxima-substitute aval a arg)))
							     args))
					       imgs)))
				 ($apply '$draw
					 (append
					  `((mlist simp)
					    ((mequal simp) $terminal $animated_gif)
					    ((mequal simp) $file_name ,file-name))
					  (get-pic-size-opt)
					  imgs))
				 "")
			     ;; If file_name is not set, show the animation in wxMaxima
			     (progn
			       (dolist (aval (reverse (cdr a-range)))
				 ($wxstatusbar (format nil "Preparing Frame #~d" frameno))
				 (setf frameno (+ 1 frameno))
				 (let* ((filename (wxplot-filename nil))
					(args (cons scene-head
						    (mapcar #'(lambda (arg) (meval (maxima-substitute aval a arg)))
							    args))))
				   (setq images (cons (format nil "~a.png" filename) images))
				   ($apply '$draw
					   (append
					    `((mlist simp)
					      ((mequal simp) $terminal ,(if $wxplot_pngcairo '$pngcairo '$png))
					      ((mequal simp) $file_name ,filename))
					    (get-pic-size-opt)
					    (list args)))))
			       (when images
				 (slide-tag images))))
			   "")))

  (defmspec $wxanimate_draw (scene)
    (wxanimate-draw scene '($gr2d)))

  (defmspec $with_slider_draw (scene)
    (wxanimate-draw scene '($gr2d)))

  (defmspec $with_slider_draw2d (scene)
    (wxanimate-draw scene '($gr2d)))

  (defmspec $with_slider_draw3d (scene)
    (wxanimate-draw scene '($gr3d)))

  (defmspec $wxanimate_draw3d (scene)
    (wxanimate-draw scene '($gr3d)))

  (defun $wxplot2d (&rest args)
    (let ((preamble ($wxplot_preamble))
	  (system-preamble (get-plot-option-string '$gnuplot_preamble 2))
	  (filename (wxplot-filename)))
      (when (string= system-preamble "false")
	(setq system-preamble ""))
      (setq preamble (format nil "~a; ~a" preamble system-preamble))
      (dolist (arg args)
	(if (and (listp arg) (eql (cadr arg) '$gnuplot_preamble))
	    (setq preamble (format nil "~a; ~a" preamble (caddr arg)))))
      (apply #'$plot2d `(,@args
			 ((mlist simp) $plot_format $gnuplot)
			 ((mlist simp) $gnuplot_term ,(if $wxplot_pngcairo '$pngcairo '$png))
			 ((mlist simp) $gnuplot_preamble ,preamble)
			 ((mlist simp) $gnuplot_out_file ,filename)))
      ($ldisp `((wxxmltag simp) ,(wxxml-fix-string filename) "img")))
    "")

  (defun $wxplot3d (&rest args)
    (let ((preamble ($wxplot_preamble))
	  (system-preamble (get-plot-option-string '$gnuplot_preamble 2))
	  (filename (wxplot-filename)))
      (when (string= system-preamble "false")
	(setq system-preamble ""))
      (setq preamble (format nil "~a; ~a" preamble system-preamble))
      (dolist (arg args)
	(if (and (listp arg) (eql (cadr arg) '$gnuplot_preamble))
	    (setq preamble (format nil "~a; ~a"
				   preamble (caddr arg)))))
      (apply #'$plot3d `(,@args
			 ((mlist simp) $plot_format $gnuplot)
			 ((mlist simp) $gnuplot_term ,(if $wxplot_pngcairo '$pngcairo '$png))
			 ((mlist simp) $gnuplot_preamble ,preamble)
			 ((mlist simp) $gnuplot_out_file ,filename)))
      ($ldisp `((wxxmltag simp) ,(wxxml-fix-string filename) "img")))
    "")


  (defun $wxdraw2d (&rest args)
    (apply #'$wxdraw
	   (list (cons '($gr2d) args))))

  (defun $wxdraw3d (&rest args)
    (apply #'$wxdraw
	   (list (cons '($gr3d) args))))

  (defvar $display_graphics t)

  (defun option-sublist (lst)
    (cons '(mlist simp)
	  (loop for l in lst
		when (and (listp l) (= ($length l) 2))
		collect l)))

  (defun $wxdraw (&rest args)
    (unless ($get '$draw '$version) ($load "draw"))
    (let* ((file_name_spec ($assoc '$file_name
				   (option-sublist (append (cdar args)
							   (cdr args)))))
	   (gnuplotfilename (wxplot-gnuplotfilename))
	   (datafilename (wxplot-datafilename))
	   (filename (or file_name_spec (wxplot-filename nil)))
	   (*windows-OS* t)
	   res)
      (setq res ($apply '$draw
			(append
			 '((mlist simp))
			 args
			 `(((mequal simp) $terminal ,(if $wxplot_pngcairo '$pngcairo '$png))
			   ((mequal simp) $gnuplot_file_name ,gnuplotfilename)
			   ((mequal simp) $data_file_name ,datafilename)
			   ((mequal simp) $file_name ,filename))
			 (cond
			  ((eq ($get '$draw '$version) 1)
			   `(((mequal simp) $pic_width ,($first $wxplot_size))
			     ((mequal simp) $pic_height ,($second $wxplot_size))))
			  (t
			   `(((mequal simp) $dimensions ,$wxplot_size)))))))
      (if $display_graphics
	  (progn
	    ($ldisp `((wxxmltag simp) ,(wxxml-fix-string (format nil "~a.png" filename)) "img"
		      ,(if file_name_spec
			   (format nil "del=\"no\" gnuplotsource=\"~a/~a\" gnuplotdata=\"~a/~a\"" $maxima_tempdir gnuplotfilename $maxima_tempdir datafilename)
			 (format nil "del=\"yes\" gnuplotsource=\"~a/~a\" gnuplotdata=\"~a/~a\"" $maxima_tempdir gnuplotfilename $maxima_tempdir datafilename)
			 )
		      ))
	    (setq res ""))
	(setf res `((wxxmltag simp) ,(wxxml-fix-string (format nil "~a.png" filename)) "img")))
      res))

  (defmspec $wxdraw_list (args)
    (unless ($get '$draw '$version) ($load "draw"))
    (let (($display_graphics nil))
      ($ldisp (cons '(mlist simp) (mapcar #'meval (cdr args)))))
    '$done)

  (defun $wximplicit_plot (&rest args)
    (let ((preamble ($wxplot_preamble))
	  (system-preamble (get-plot-option-string '$gnuplot_preamble 2))
	  (filename (wxplot-filename)))
      (when (string= system-preamble "false")
	(setq system-preamble ""))
      (setq preamble (format nil "~a; ~a" preamble system-preamble))
      (dolist (arg args)
	(if (and (listp arg) (eql (cadr arg) '$gnuplot_preamble))
	    (setq preamble (format nil "~a; ~a"
				   preamble (caddr arg)))))
      ($apply '$implicit_plot `((mlist simp) ,@args
				((mlist simp) $plot_format $gnuplot)
				((mlist simp) $gnuplot_term ,(if $wxplot_pngcairo '$pngcairo '$png))
				((mlist simp) $gnuplot_preamble ,preamble)
				((mlist simp) $gnuplot_out_file ,filename)))
      ($ldisp `((wxxmltag simp) ,(wxxml-fix-string filename) "img")))
    "")


  (defun $wxcontour_plot (&rest args)
    (let ((preamble ($wxplot_preamble))
	  ($plot_options $plot_options)
	  (system-preamble (get-plot-option-string '$gnuplot_preamble 2))
	  (filename (wxplot-filename)))
      (when (string= system-preamble "false")
	(setq system-preamble ""))
      (setq preamble (format nil "~a; ~a" preamble system-preamble))
      (dolist (arg args)
	(if (and (listp arg) (eql (cadr arg) '$gnuplot_preamble))
	    (setq preamble (format nil "~a; ~a" preamble (caddr arg)))))
      (apply #'$contour_plot `(,@args
			       ((mlist simp) $gnuplot_term ,(if $wxplot_pngcairo '$pngcairo '$png))
			       ((mlist simp) $plot_format $gnuplot)
			       ((mlist simp) $gnuplot_preamble ,preamble)
			       ((mlist simp) $gnuplot_out_file ,filename)))

      ($ldisp `((wxxmltag simp) ,(wxxml-fix-string filename) "img")))
    "")

  (defun $show_image (file)
    ($ldisp `((wxxmltag simp) ,(wxxml-fix-string file) "img" "del=\"no\"")))

  ;;
  ;; Port of Barton Willis's texput function.
  ;;

  (defun $wxxmlput (e s &optional tx lbp rbp)

    (when (stringp e)
      (setf e (define-symbol e)))

    (cond (($listp s)
	   (setq s (margs s)))
	  ((stringp s)
	   (setq s (list s)))
	  ((atom s)
	   (setq s (list (wxxml-stripdollar ($sconcat s))))))

    (when (or (null lbp) (not (integerp lbp)))
      (setq lbp 180))
    (when (or (null rbp) (not (integerp rbp)))
      (setq rbp 180))
    (cond ((null tx)
	   (if (stringp (nth 0 s))
	       (putprop e (nth 0 s) 'wxxmlword)
             (let ((fun-name (gensym))
                   (fun-body
                    `(append l
                             (list
                              (let ((f-x (mfuncall ',s x)))
                                (if (stringp f-x)
                                    f-x
				  (merror "wxxml: function ~s did not return a string.~%"
					  ($sconcat ',(nth 0 s))))))
                             r)))
               (setf (symbol-function fun-name) (coerce `(lambda (x l r) ,fun-body) 'function))
               (setf (get e 'wxxml) fun-name))))
	  ((eq tx '$matchfix)
	   (putprop e 'wxxml-matchfix 'wxxml)
	   (cond ((< (length s) 2)
		  (merror
		   "Improper 2nd argument to `wxxmlput' for matchfix operator."))
		 ((eq (length s) 2)
		  (putprop e (list (list (nth 0 s)) (nth 1 s)) 'wxxmlsym))
		 (t
		  (putprop
		   e (list (list (nth 0 s)) (nth 1 s) (nth 2 s)) 'wxxmlsym))))
	  ((eq tx '$prefix)
	   (putprop e 'wxxml-prefix 'wxxml)
	   (putprop e s 'wxxmlsym)
	   (putprop e lbp 'wxxml-lbp)
	   (putprop e rbp 'wxxml-rbp))
	  ((eq tx '$infix)
	   (putprop e 'wxxml-infix 'wxxml)
	   (putprop e  s 'wxxmlsym)
	   (putprop e lbp 'wxxml-lbp)
	   (putprop e rbp 'wxxml-rbp))
	  ((eq tx '$postfix)
	   (putprop e 'wxxml-postfix 'wxxml)
	   (putprop e  s 'wxxmlsym)
	   (putprop e lbp 'wxxml-lbp))
	  (t (merror "Improper arguments to `wxxmlput'."))))

;;;;;;;;;;;;;
  ;; Auto-loaded functions
;;;;

  (setf (get '$lbfgs 'autoload) "lbfgs")
  (setf (get '$lcm 'autoload) "functs")

;;;;;;;;;;;;;
  ;; Statistics functions
;;;;

  (defvar $draw_compound t)

  (defmacro create-statistics-wrapper (fun wxfun)
    `(defun ,wxfun (&rest args)
       (let (($draw_compound nil) res)
	 (declare (special $draw_compound))
	 (setq res ($apply ',fun (cons '(mlist simp) args)))
	 ($apply '$wxdraw2d res))))

  (create-statistics-wrapper $histogram $wxhistogram)
  (create-statistics-wrapper $scatterplot $wxscatterplot)
  (create-statistics-wrapper $barsplot $wxbarsplot)
  (create-statistics-wrapper $piechart $wxpiechart)
  (create-statistics-wrapper $boxplot $wxboxplot)

  (dolist (fun '($histogram
		 $scatterplot
		 $barsplot
		 $piechart
		 $boxplot))
    (setf (get fun 'autoload) "descriptive"))

  (dolist (fun '($mean
		 $median
		 $var
		 $std
		 $test_mean
		 $test_means_difference
		 $test_normality
		 $simple_linear_regression
		 $subsample))
    (setf (get fun 'autoload) "stats"))

  (setf (get '$lsquares_estimates 'autoload) "lsquares")

  (setf (get '$to_poly_solve 'autoload) "to_poly_solve")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Redefine load so that it prints the list of functions
  ;; used for autocompletion.
  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun symbol-to-xml (s)
    (wxxml-fix-string (format nil "~a" (maybe-invert-string-case (symbol-name (stripdollar s))))))

  (defun print_unit (unit)
    (format nil "<unit>~a</unit>" (symbol-to-xml unit)))

  (defun $print_function (fun)
    (let ((fun-name (symbol-to-xml (caar fun)))
	  (*print-circle* nil)
	  (args (mapcar (lambda (u)
			  (cond ((atom u) (symbol-to-xml u))
				((eq (caar u) 'mlist)
				 ($concat "[" (symbol-to-xml
					       (if (atom (cadr u)) (cadr u) (cadadr u))) "]"))
				(t (symbol-to-xml (cadr u)))))
			(cdr fun))))
      (format nil "<function>~a</function><template>~a(~{&lt;~a&gt;~^, ~})</template>" fun-name fun-name args)))

  (defun print_value (val)
    (format nil "<value>~a</value>" (symbol-to-xml val)))

  (defun $add_function_template (&rest functs)
    (let ((*print-circle* nil))
      (format t "<wxxml-symbols>~{~a~^$~}</wxxml-symbols>" (mapcar #'$print_function functs))
      (cons '(mlist simp) functs)))


  ;; A function that determines all symbols for autocompletion
  (defun wxPrint_autocompletesymbols ()
    #+clisp (finish-output)
    (format t "<wxxml-symbols>")
    ;; Function names and rules
    (format t "~{~a~^$~}"
	    (append (mapcar #'$print_function (cdr ($append $functions $macros)))
		    (mapcar #'print_value (cdr ($append $values $rules)))))
    ;; Idea from Robert Dodier:
    ;; Variables defined with mdef don't appear in $values nor do they in $myoptions
    ;; but they appear in *variable-initial-values*
    (maphash (lambda (key val)
	       (declare (ignore val))
	       (if (eq (char (format nil "~a" key) 0) #\$ )
		   (format t "~a" (print_value key))))
	     *variable-initial-values*)

    ;; ezunits publishes all known units in a function.
    (if (boundp '$known_units)
    	(no-warning
    	 (format t "~{~a~^$~}"
    		 (mapcar #'print_unit (cdr ($known_units))))))
    (format t "</wxxml-symbols>")
    #+clisp (finish-output)
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Communication between wxMaxima and wxMaxima about variables and directories
;;;

;;; Communicate the contents of variables to wxMaxima
  (defun wx-print-variable (var)
    (format t "<variable>~%<name>~a</name>" (symbol-to-xml var))
    (ignore-errors
      (let (($display2d nil))
	    (mtell "<value>~M</value>" (wxxml-fix-string(eval var)))))
    (format t "</variable>"))

  (defun wx-query-variable (var)
    (format t "<variables>")
    (wx-print-variable var)
    (format t "</variables>~%"))

  (defun wx-print-variables ()
    #+clisp (finish-output)
    (format t "<variables>")
    (wx-print-variable '$maxima_userdir)
    (wx-print-variable '$maxima_tempdir)
    (wx-print-variable '*maxima-infodir*)
    (wx-print-variable '*maxima-htmldir*)
					;  (wx-print-variable '*maxima-topdir*)
    (wx-print-variable '$gnuplot_command)
    (wx-print-variable '*maxima-demodir*)
    (wx-print-variable '*maxima-sharedir*)
    (wx-print-variable '*autoconf-version*)
    (wx-print-variable '*autoconf-host*)
    (format t "<variable><name>*lisp-name*</name><value>~a</value></variable>"
	    #+sbcl (ensure-readably-printable-string (lisp-implementation-type))
	    #-sbcl (lisp-implementation-type))
    (format t "<variable><name>*lisp-version*</name><value>~a</value></variable>"
	    #+sbcl (ensure-readably-printable-string (lisp-implementation-version))
	    #-sbcl (lisp-implementation-version))
    (format t "</variables>~%")
    #+clisp (finish-output)
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; A function that allows wxMaxima to set maxima's current directory to that
  ;; of the worksheet.
  (defun wx-cd (dir)
    (handler-case
      (progn
        (let ((dir (cond ((pathnamep dir) dir)
	  	       ((stringp dir)
	  		(make-pathname :directory (pathname-directory dir)
	    			       :host (pathname-host dir)
	  			       :device (pathname-device dir)))
	  	       (t (error "cd(dir): dir must be a string or pathname.")))))
	  #+  allegro (excl:chdir dir)
	  #+clisp (ext:cd dir)
	  #+cmu (setf (ext:default-directory) dir)
	  #+cormanlisp (ccl:set-current-directory dir)
	  #+gcl (si::chdir dir)
	  #+lispworks (hcl:change-directory dir)
	  #+lucid (lcl:working-directory dir)
	  #+sbcl (sb-posix:chdir dir)
	  #+sbcl (setf *default-pathname-defaults* (sb-ext:native-pathname (format nil "~A~A" (sb-posix:getcwd) "/")))
	  #+ccl (ccl:cwd dir)
	  #+ecl (si::chdir dir)
         ;;; Officially gcl supports (si:chdir dir), too. But the version
         ;;; shipped with debian and ubuntu (at least in Feb 2017) doesn't.
	  #+gcl (xchdir dir)
	  #+gcl (setf *default-pathname-defaults* dir)

	  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl ccl ecl) (format t
           "Info: wxMathml.cpp: Changing the working dir during a maxima session isn't implemented for this lisp.")
  	  (namestring dir)
	  (wx-print-variables)))
      (error (c)        (format t "Warning: Can set maxima's working directory but cannot change it during the maxima session :~%~&~%")
          (values 0 c))))

;;;;;;;;;;;;;;;;;;;;;
  ;; table_form implementation

  (defun make-zeros (n)
    (cons '(mlist simp) (loop for i from 1 to n collect "")))
  (defun take-first (l n)
    (if (= n 0) nil (cons (first l) (take-first (rest l) (- n 1)))))

  (defun $table_form (mat &rest opts)
    (when (mapatom mat)
      ($error "table_form: the argument should not be an atom."))
    (setq mat ($args mat))
    (unless (every #'$listp (cdr mat))
      (setq mat (cons '(mlist simp)
		      (mapcar (lambda (e) (list '(mlist simp) e))
			      (cdr mat)))))
    (setq opts (cons '(mlist simp) opts))
    (let ((row-names ($assoc '$row_names opts))
	  (col-names ($assoc '$column_names opts))
	  (m (apply #'max (mapcar '$length (cdr mat))))
	  (n (length (cdr mat)))
	  (mtrx '(special)))
      (when ($assoc '$transpose opts)
	(rotatef m n))
      (when (eq row-names '$auto)
	(setq row-names (cons '(mlist simp) (loop for i from 1 to n collect i))))
      (when (eq col-names '$auto)
	(setq col-names (cons '(mlist simp) (loop for i from 1 to m collect i))))
      (when row-names
	(setq row-names ($append row-names (make-zeros (- n ($length row-names)))))
	(setq row-names (cons '(mlist simp) (take-first (cdr row-names) n))))
      (when col-names
	(setq col-names ($append col-names (make-zeros (- m ($length col-names)))))
	(setq col-names (cons '(mlist simp) (take-first (cdr col-names) m))))
      (when (and row-names col-names)
	(setq col-names ($cons "" col-names)))
      (setq mat (cons '(mlist simp) (mapcar
				     (lambda (r) ($append r (make-zeros (- m ($length r)))))
				     (cdr mat))))
      (setq mat ($apply '$matrix mat))
      (when ($assoc '$transpose opts)
	(setq mat ($transpose mat)))
      (when row-names
	(setq mat (cons '($matrix simp)
			(mapcar #'$append (cdr ($transpose row-names)) (cdr mat))))
	(setq mtrx (cons 'rownames mtrx)))
      (when col-names
	(setq mat (cons '(matrix simp)
			(cons col-names (cdr mat))))
	(setq mtrx (cons 'colnames mtrx)))
      ($ldisp (cons (append '($matrix simp) mtrx) (cdr mat)))
      '$done))

  (putprop '$table_form t 'evfun)

  ;; Load the initial functions (from mac-init.mac)
  (let ((*print-circle* nil))
    (format t "<wxxml-symbols>~{~a~^$~}</wxxml-symbols>~%"
	    (mapcar #'$print_function (cdr ($append $functions $macros)))))

  (no-warning
   (defun mredef-check (fnname)
     (declare (ignore fnname))
     t))

;;; A function that loads bitmaps from files as a slideshow.
;;; Todo: Replace this function by at least half-way-optimized LISP code.
  (progn
    (defprop $wxanimate_from_imgfiles t translated)
    (add2lnc '$wxanimate_from_imgfiles $props)
    (defmtrfun ($wxanimate_from_imgfiles $any mdefine t nil)
      ($x)
      (declare (special $x))
      (progn
	#+clisp (finish-output)
	(simplify (mfunction-call $printf t '"<mth><slide"))
	(cond
	 ((is-boole-check (trd-msymeval $wxanimate_autoplay '$wxanimate_autoplay))
	  (simplify (mfunction-call $printf t '" running=\"false\""))))
	(cond
	 ((like
	   (simplify
	    `((mfactorial)
	      ,(trd-msymeval $wxanimate_framerate '$wxanimate_framerate)))
	   '$wxanimate_framerate)
	  (simplify
	   (mfunction-call $printf t '" fr=\"~d\""
			   (trd-msymeval $wxanimate_framerate
					 '$wxanimate_framerate)))))
	(simplify (mfunction-call $printf t '">"))
	(do (($i)
	     (mdo (cdr $x) (cdr mdo)))
	    ((null mdo) '$done)
	    (declare (special $i))
	    (setq $i (car mdo))
	    (simplify (mfunction-call $printf t '"~a;" $i)))
	(simplify (mfunction-call $printf t '"</slide></mth>"))
	#+clisp (finish-output)
	)
      ))


  (when ($file_search "wxmaxima-init")
    ($load "wxmaxima-init"))

;;;
;;; Now that we have loaded the init file we can rewrite of the function load
;;; (maxima/src/mload.lisp) to displays functions and variable names after
;;; loading a Maxima package so the autocomplete functionality knows which
;;; function and variable names exists.
;;;
;;; We also inform wxMaxima about the name of the package and about the fact
;;; if it was loaded by another package.
;;;

  (defvar *wxmaxima-nested-loads* 0 "How many load commands are nested, currently?")
  (setf (symbol-function 'load_original_wxmaxima) (symbol-function '$load))
  
  (no-warning
   (defun $load (filename)

     ;; Inform wxMaxima that we load a file.
     (if (eq *wxmaxima-nested-loads* 0)
	 (progn
	   (format t "<variables>")
	   (format t "<variable><name>*wx-load-file-name*</name><value>~a</value></variable>"
		   (wxxml-fix-string filename))
	   (format t "</variables>~%")))
     (setq *wxmaxima-nested-loads* (+ *wxmaxima-nested-loads* 1))
     ;; Load the file
     (unwind-protect
	 (load_original_wxmaxima filename)
       (progn
	 ;; After loading the file: Tell wxMaxima we have finished loading the file
	 ;; and what autocompletable symbols we know about.
	 (setq *wxmaxima-nested-loads* (- *wxmaxima-nested-loads* 1))
	 (if (eq *wxmaxima-nested-loads* 0)
	     (wxPrint_autocompletesymbols))
	 ))))
  (format t "</suppressOutput>~%")
  ;; Publish all new global variables maxima might contain to wxMaxima's
  ;; autocompletion feature.
  (wxPrint_autocompletesymbols)
  (wx-print-variables)
  (finish-output)
)
