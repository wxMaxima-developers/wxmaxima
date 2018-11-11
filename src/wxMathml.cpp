#include "wxMathml.h"

wxMathML::wxMathML()
    { 
      m_wxMathML =
	wxString("(format t \"<suppressOutput>\")\n") +
	wxString(";; wxMaxima xml format (based on David Drysdale MathML printing)\n") +
	wxString(";; Andrej Vodopivec,  2004-2014\n") +
	wxString(";; Gunter Königsmann, 2014-2018\n") +
	wxString(";;  SPDX-License-Identifier: GPL-2.0+\n") +
	wxString("\n") +
	wxString(";; MathML-printing\n") +
	wxString(";; Created by David Drysdale (DMD), December 2002/January 2003\n") +
	wxString(";;\n") +
	wxString(";; closely based on the original TeX conversion code in mactex.lisp,\n") +
	wxString(";; for which the following credits apply:\n") +
	wxString(";;   (c) copyright 1987, Richard J. Fateman\n") +
	wxString(";;   small corrections and additions: Andrey Grozin, 2001\n") +
	wxString(";;   additional additions: Judah Milgram (JM), September 2001\n") +
	wxString(";;   additional corrections: Barton Willis (BLW), October 2001\n") +
	wxString(";; Method:\n") +
	wxString("\n") +
	wxString(";; Producing wxml from a Maxima internal expression is done by\n") +
	wxString(";; a reversal of the parsing process.  Fundamentally, a\n") +
	wxString(";; traversal of the expression tree is produced by the program,\n") +
	wxString(";; with appropriate substitutions and recognition of the\n") +
	wxString(";; infix / prefix / postfix / matchfix relations on symbols. Various\n") +
	wxString(";; changes are made to this so that MathML will like the results.\n") +
	wxString("\n") +
	wxString(";(format t \"<wxxml-start/>\")\n") +
	wxString("\n") +
	wxString(";; This is necessary to make file and directory names that contain special characters\n") +
	wxString(";; work under windows.\n") +
	wxString("(progn\n") +
	wxString("  #+sbcl (setf sb-impl::*default-external-format* :UTF-8)\n") +
	wxString("\n") +
	wxString("  (in-package :maxima)\n") +
	wxString("\n") +
	wxString("  (declare-top\n") +
	wxString("   (special lop rop $inchar)\n") +
	wxString("   (*expr wxxml-lbp wxxml-rbp))\n") +
	wxString("\n") +
	wxString("  ;; Use rounded parenthesis for matrices by default\n") +
	wxString("  (setq $lmxchar #\\()\n") +
	wxString("  (setq $rmxchar #\\()\n") +
	wxString("\n") +
	wxString("  ;; A few variables whose value can be configured from wxMaxima\n") +
	wxString("  (defvar *wx-plot-num* 0 \"The serial number of the current plot\")\n") +
	wxString("  (defvar $wxfilename \"\" \"The filename of the current wxMaxima worksheet\")\n") +
	wxString("  (defvar $wxdirname \"\" \"The directory the current wxMaxima worksheet lies in\")\n") +
	wxString("  (defvar $wxanimate_autoplay nil \"Automatically playback new animations?\")\n") +
	wxString("  (defvar wxUserConfDir \"\" \"The location wxMaxima looks for maxima\'s config files in\")\n") +
	wxString("  (defvar wxHelpDir \"\" \"The location wxMaxima searches for help files in\")\n") +
	wxString("  (defvar wxMaximaLispLocation \"\" \"The location wxMaxima searches for lisp files in\")\n") +
	wxString("  (defvar $wxplot_size \'((mlist simp) 800 600) \"The size of new plots\")\n") +
	wxString("  (defvar $wxmaximaversion t \"The wxMaxima version\")\n") +
	wxString("  (defprop $wxmaximaversion read-only-assign assign)\n") +
	wxString("  (defvar $wxwidgetsversion t \"The wxWidgets version wxMaxima is using.\")\n") +
	wxString("  (defvar $wxsubscripts t\n") +
	wxString("    \"Recognize TeX-style subscripts\")\n") +
	wxString("  (defvar $wxplot_pngcairo nil \"Use gnuplot\'s pngcairo terminal for new plots?\")\n") +
	wxString("  (defmvar $wxplot_old_gnuplot nil)\n") +
	wxString("\n") +
	wxString("\n") +
	wxString("  ;; Escape all chars that need escaping in XML\n") +
	wxString("  (defun wxxml-fix-string (x)\n") +
	wxString("    (if (stringp x)\n") +
	wxString("	(let* ((tmp-x (string-substitute \"&amp;\" #\\& x))\n") +
	wxString("	       (tmp-x (string-substitute \"&lt;\" #\\< tmp-x))\n") +
	wxString("	       (tmp-x (string-substitute \"&gt;\" #\\> tmp-x))\n") +
	wxString("	       (tmp-x (string-substitute \"&#13;\" #\\Return tmp-x))\n") +
	wxString("	       (tmp-x (string-substitute \"&#13;\" #\\Linefeed tmp-x))\n") +
	wxString("	       (tmp-x (string-substitute \"&#13;\" #\\Newline tmp-x)))\n") +
	wxString("	  tmp-x)\n") +
	wxString("      x))\n") +
	wxString("\n") +
	wxString("  ;; Allow the user to communicate what to display in the statusbar whilst\n") +
	wxString("  ;; the current program is running\n") +
	wxString("  (defun $wxstatusbar (&rest status)\n") +
	wxString("    (format t \"<statusbar>~a</statusbar>~%\" (wxxml-fix-string\n") +
	wxString("					     (apply \'$sconcat status))))\n") +
	wxString("\n") +
	wxString("\n") +
	wxString(";;; Without this command encountering unicode characters might cause\n") +
	wxString(";;; Maxima to stop responding on windows.\n") +
	wxString("  #+(and clisp win32) (setf (stream-external-format *socket-connection*) charset:utf-8)\n") +
	wxString("  #+(and clisp win32) (setf custom:*default-file-encoding* charset:utf-8)\n") +
	wxString("\n") +
	wxString(";;; Muffle compiler-notes globally\n") +
	wxString("  #+sbcl (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))\n") +
	wxString("  (defmacro no-warning (form)\n") +
	wxString("    #+sbcl `(handler-bind\n") +
	wxString("		((style-warning #\'muffle-warning)\n") +
	wxString("		 (sb-ext:compiler-note #\'muffle-warning))\n") +
	wxString("	      ,form)\n") +
	wxString("    #+clisp `(let ((custom:*suppress-check-redefinition* t)) ,form)\n") +
	wxString("    #-(or sbcl clisp) `(progn ,form))\n") +
	wxString("\n") +
	wxString("  (defun read-wxmaxima-version (v)\n") +
	wxString("    (let* ((d1 (position #\\. v))\n") +
	wxString("	   (year (subseq v 0 d1))\n") +
	wxString("	   (d2 (position #\\. v :start (1+ d1)))\n") +
	wxString("	   (month (subseq v (1+ d1) d2))\n") +
	wxString("	   (rest (subseq v (1+ d2))))\n") +
	wxString("      (list \'(mlist simp) (parse-integer year) (parse-integer month) rest)))\n") +
	wxString("\n") +
	wxString("  (defun $wxbuild_info ()\n") +
	wxString("    (let ((year (sixth cl-user:*maxima-build-time*))\n") +
	wxString("	  (month (fifth cl-user:*maxima-build-time*))\n") +
	wxString("	  (day (fourth cl-user:*maxima-build-time*))\n") +
	wxString("	  (hour (third cl-user:*maxima-build-time*))\n") +
	wxString("	  (minute (second cl-user:*maxima-build-time*))\n") +
	wxString("	  (seconds (first cl-user:*maxima-build-time*)))\n") +
	wxString("      (format t \"wxMaxima version: ~a~%\" $wxmaximaversion)\n") +
	wxString("      (format t \"using wxWidgets version: ~a~%\" $wxwidgetsversion)\n") +
	wxString("      (format t \"Maxima version: ~a~%\" *autoconf-version*)\n") +
	wxString("      (format t \"Maxima build date: ~4,\'0d-~2,\'0d-~2,\'0d ~2,\'0d:~2,\'0d:~2,\'0d~%\"\n") +
	wxString("	      year month day hour minute seconds)\n") +
	wxString("      (format t \"Host type: ~a~%\" *autoconf-host*)\n") +
	wxString("      (format t \"System type: ~a ~a ~a~%\" (software-type) (software-version) (machine-type))\n") +
	wxString("      (format t \"Lisp implementation type: ~a~%\" (lisp-implementation-type))\n") +
	wxString("      (format t \"Lisp implementation version: ~a~%\" (lisp-implementation-version))\n") +
	wxString("      (format t \"~%~%wxMaxima\'s idea of the directory layout is:~%User configuration dir: ~a~%\" wxUserConfDir)\n") +
	wxString("      (format t \"Help dir: ~a~%\" wxHelpDir)\n") +
	wxString("      (format t \"Maxima lisp dir: ~a~%\" wxMaximaLispLocation))\n") +
	wxString("    \"\")\n") +
	wxString("\n") +
	wxString("  (defmfun $wxbug_report ()\n") +
	wxString("    (format t \"wxMaxima is a graphical front end for Maxima, which does the mathematics in the background.~%\")\n") +
	wxString("    (format t \"If you encounter a mathematical problem, it is probably a Maxima bug und should be submitted there.~%\")\n") +
	wxString("    (format t \"~%The Maxima bug database is available at~%\")\n") +
	wxString("    (format t \"    https://sourceforge.net/p/maxima/bugs~%\")\n") +
	wxString("    (format t \"Submit bug reports by following the \'Create Ticket\' link on that page.~%\")\n") +
	wxString("    (format t \"To report a Maxima bug, you must have a Sourceforge account.~%~%\")\n") +
	wxString("    (format t \"A problem in the graphical user interface is probably a wxMaxima bug.~%\")\n") +
	wxString("    (format t \"The wxMaxima bug database is available at~%\")\n") +
	wxString("    (format t \"    https://github.com/wxMaxima-developers/wxmaxima/issues?direction=desc&sort=created&state=open~%\")\n") +
	wxString("    (format t \"Submit bug reports by following the \'New issue\' link on that page.~%~%\")\n") +
	wxString("    (format t \"Please check before submitting, if your bug was already reported.~%~%\")\n") +
	wxString("    (format t \"Please include the following information with your bug report:~%\")\n") +
	wxString("    (format t \"-------------------------------------------------------------~%\")\n") +
	wxString("    ($wxbuild_info)\n") +
	wxString("    (format t \"-------------------------------------------------------------~%\"))\n") +
	wxString("\n") +
	wxString("  (defvar *var-tag* \'(\"<v>\" \"</v>\"))\n") +
	wxString("\n") +
	wxString("  (defun wxxml-get (x p)\n") +
	wxString("    (if (symbolp x) (get x p)))\n") +
	wxString("\n") +
	wxString("  (defun wxxml-array (x l r &aux f)\n") +
	wxString("    (if (eq \'mqapply (caar x))\n") +
	wxString("	(setq f (cadr x)\n") +
	wxString("	      x (cdr x)\n") +
	wxString("	      l (wxxml f (append l (list \"<i><p>\")) (list \"</p>\")\n") +
	wxString("		       \'mparen \'mparen))\n") +
	wxString("      (setq f (caar x)\n") +
	wxString("	    l (wxxml f (append l \'(\"<i><r>\"))\n") +
	wxString("		     (list \"</r>\") lop \'mfunction)))\n") +
	wxString("    (setq r (nconc (wxxml-list (cdr x) (list \"<r>\")\n") +
	wxString("			       (list \"</r></i>\") \"<v>,</v>\") r))\n") +
	wxString("    (nconc l r))\n") +
	wxString("\n") +
	wxString("  (defmacro make-tag (val tag)\n") +
	wxString("    ``((wxxmltag simp) ,,val ,,tag))\n") +
	wxString("\n") +
	wxString("  (defun $wxxmltag (val tag)\n") +
	wxString("    (make-tag ($sconcat val) ($sconcat tag)))\n") +
	wxString("\n") +
	wxString("  (defun string-substitute (newstring oldchar x &aux matchpos)\n") +
	wxString("    (setq matchpos (position oldchar x))\n") +
	wxString("    (if (null matchpos) x\n") +
	wxString("      (concatenate \'string\n") +
	wxString("		   (subseq x 0 matchpos)\n") +
	wxString("		   newstring\n") +
	wxString("		   (string-substitute newstring oldchar\n") +
	wxString("				      (subseq x (1+ matchpos))))))\n") +
	wxString("\n") +
	wxString(";;; First we have the functions which are called directly by wxxml and its\n") +
	wxString(";;; descendants\n") +
	wxString("  (defun $wxdeclare_subscript (x &optional (opt t))\n") +
	wxString("    (unless (listp x)\n") +
	wxString("      (setq x (list \'(mlist simp) x)))\n") +
	wxString("    (dolist (s (cdr x))\n") +
	wxString("      ($put s opt \'$wxxml_subscript))\n") +
	wxString("    opt)\n") +
	wxString("\n") +
	wxString("  (defun $wxdeclare_subscripted (x &optional (opt t))\n") +
	wxString("    (unless (listp x)\n") +
	wxString("      (setq x (list \'(mlist simp) x)))\n") +
	wxString("    (dolist (s (cdr x))\n") +
	wxString("      ($put s opt \'$wxxml_subscripted))\n") +
	wxString("    opt)\n") +
	wxString("\n") +
	wxString("  (defun subscriptp (x)\n") +
	wxString("    (unless (symbolp x)\n") +
	wxString("      (return-from subscriptp x))\n") +
	wxString("    (let* ((name (subseq (maybe-invert-string-case (symbol-name x)) 1))\n") +
	wxString("	   (pos (search \"_\" name :from-end t))\n") +
	wxString("	   #-gcl (*readtable* (copy-readtable nil)))\n") +
	wxString("      #-gcl (setf (readtable-case *readtable*) :invert)\n") +
	wxString("      (when pos\n") +
	wxString("	(let* ((sub (subseq name (+ pos 1)))\n") +
	wxString("	       (sub-var (subseq name 0 pos))\n") +
	wxString("	       (sub-var-symb (read-from-string (concatenate \'string \"$\" sub-var)))\n") +
	wxString("	       (sub-symb (read-from-string (concatenate \'string \"$\" sub)))\n") +
	wxString("	       (sub-int (ignore-errors\n") +
	wxString("			  (parse-integer sub))))\n") +
	wxString("	  (when (and (> (length sub-var) 0)\n") +
	wxString("		     (or sub-int\n") +
	wxString("			 (eq $wxsubscripts \'$all)\n") +
	wxString("			 (= (length sub) 1)\n") +
	wxString("			 (= (length sub-var) 1)\n") +
	wxString("			 ($get x \'$wxxml_subscripted)\n") +
	wxString("			 ($get sub-symb \'$wxxml_subscript)))\n") +
	wxString("	    (format nil  \"<i altCopy=\\\"~{~a~}\\\"><r>~a</r><r>~a</r></i>\"\n") +
	wxString("		    (mstring x)\n") +
	wxString("		    (or (get sub-var-symb \'wxxmlword)\n") +
	wxString("			(format nil \"<v>~a</v>\" sub-var))\n") +
	wxString("		    (if sub-int\n") +
	wxString("			(format nil \"<n>~a</n>\" sub-int)\n") +
	wxString("                      (format nil \"<v>~a</v>\" sub))))))))\n") +
	wxString("\n") +
	wxString("  (defun wxxmlnumformat (atom)\n") +
	wxString("    (let (r firstpart exponent)\n") +
	wxString("      (cond ((integerp atom)\n") +
	wxString("	     (format nil \"<n>~{~c~}</n>\" (exploden atom)))\n") +
	wxString("	    (t\n") +
	wxString("	     (setq r (exploden atom))\n") +
	wxString("	     (setq exponent (member \'e r :test #\'string-equal))\n") +
	wxString("	     (cond ((null exponent)\n") +
	wxString("		    (format nil \"<n>~{~c~}</n>\" r))\n") +
	wxString("		   (t\n") +
	wxString("		    (setq firstpart\n") +
	wxString("			  (nreverse (cdr (member \'e (reverse r)\n") +
	wxString("						 :test #\'string-equal))))\n") +
	wxString("		    (if (char= (cadr exponent) #\\+)\n") +
	wxString("			(setq exponent (cddr exponent))\n") +
	wxString("		      (setq exponent (cdr exponent)))\n") +
	wxString("		    (format nil\n") +
	wxString("			    \"<r><n>~{~c~}</n><h>*</h><e><n>10</n><n>~{~c~}</n></e></r>\"\n") +
	wxString("			    firstpart exponent)))))))\n") +
	wxString("\n") +
	wxString("  (defun wxxml-stripdollar (sym &aux pname)\n") +
	wxString("    (or (symbolp sym)\n") +
	wxString("	(return-from wxxml-stripdollar\n") +
	wxString("		     (wxxml-fix-string (format nil \"~a\" sym))))\n") +
	wxString("    (setq pname (maybe-invert-string-case (symbol-name sym)))\n") +
	wxString("    (setq pname (cond ((and (> (length pname) 0)\n") +
	wxString("			    (member (elt pname 0) \'(#\\$ #\\&) :test #\'eq))\n") +
	wxString("		       (subseq pname 1))\n") +
	wxString("		      ((and (> (length pname) 0)\n") +
	wxString("			    (equal (elt pname 0) #\\%))\n") +
	wxString("		       (if $noundisp\n") +
	wxString("			   (concatenate \'string \"\'\"\n") +
	wxString("					(subseq pname 1))\n") +
	wxString("			 (subseq pname 1)))\n") +
	wxString("		      ($lispdisp\n") +
	wxString("		       (concatenate \'string \"?\" pname))\n") +
	wxString("		      (t pname)))\n") +
	wxString("    (setq pname (wxxml-fix-string pname))\n") +
	wxString("    (concatenate \'string (car *var-tag*) pname (cadr *var-tag*)))\n") +
	wxString("\n") +
	wxString("  (defun wxxml-atom (x l r &aux tmp-x)\n") +
	wxString("    (append l\n") +
	wxString("	    (list (cond ((numberp x) (wxxmlnumformat x))\n") +
	wxString("			((and (symbolp x) (get x \'wxxmlword)))\n") +
	wxString("			((and (symbolp x) (get x \'reversealias))\n") +
	wxString("			 (wxxml-stripdollar (get x \'reversealias)))\n") +
	wxString("			((stringp x)\n") +
	wxString("			 (setq tmp-x (wxxml-fix-string x))\n") +
	wxString("			 (if (and (boundp \'$stringdisp) $stringdisp)\n") +
	wxString("			     (setq tmp-x (format nil \"\\\"~a\\\"\" tmp-x)))\n") +
	wxString("			 (concatenate \'string \"<st>\" tmp-x \"</st>\"))\n") +
	wxString("			((arrayp x)\n") +
	wxString("			 (format nil \"<v>#{Lisp array [~{~a~^,~}]}</v>\"\n") +
	wxString("				 (array-dimensions x)))\n") +
	wxString("			((functionp x)\n") +
	wxString("			 (format nil \"<v>~a</v>\"\n") +
	wxString("				 (wxxml-fix-string\n") +
	wxString("				  (stripdollar\n") +
	wxString("				   (maybe-invert-string-case (format nil \"~A\" x))))))\n") +
	wxString("			((streamp x)\n") +
	wxString("			 (format nil \"<v>#{Stream [~A]</v>}\"\n") +
	wxString("				 (stream-element-type x)))\n") +
	wxString("			((member (type-of x) \'(GRAPH DIGRAPH))\n") +
	wxString("			 (format nil \"<v>~a</v>\" x))\n") +
	wxString("			((typep x \'structure-object)\n") +
	wxString("			 (let ((tmp-string (format nil \"~s\" x)))\n") +
	wxString("			   (format nil \"<st>~a</st>\" (wxxml-fix-string tmp-string))))\n") +
	wxString("			((hash-table-p x)\n") +
	wxString("			 (format nil \"<v>#{HashTable}</v>\"))\n") +
	wxString("			((and $wxsubscripts (subscriptp x)))\n") +
	wxString("			(t (wxxml-stripdollar x))))\n") +
	wxString("	    r))\n") +
	wxString("\n") +
	wxString("  ;; we could patch this so sin x rather than sin(x), but instead we made\n") +
	wxString("  ;; sin a prefix operator\n") +
	wxString("  (defun wxxml-function (x l r)\n") +
	wxString("    (setq l\n") +
	wxString("	  (let ((*var-tag* \'(\"<fnm>\" \"</fnm>\")))\n") +
	wxString("	    (wxxml (caar x) (append l \'(\"<fn>\"))\n") +
	wxString("		   nil \'mparen \'mparen))\n") +
	wxString("	  r (wxxml (cons \'(mprogn) (cdr x)) nil (append \'(\"</fn>\") r)\n") +
	wxString("		   \'mparen \'mparen))\n") +
	wxString("    (append l r))\n") +
	wxString("\n") +
	wxString("  (defun wxxml-defstruct (x l r)\n") +
	wxString("    (let ((L1 (cdr (get (caar x) \'defstruct-template)))\n") +
	wxString("	  (L2 (cdr x)))\n") +
	wxString("      (wxxml-function\n") +
	wxString("       (cons (car x)\n") +
	wxString("	     (mapcar #\'(lambda (e1 e2) (if (eq e1 e2) e1 `((mequal) ,e1 ,e2))) L1 L2))\n") +
	wxString("       l r)))\n") +
	wxString("\n") +
	wxString("  (defun wxxml-matchfix-dim (x l r)\n") +
	wxString("    (setq l (append l\n") +
	wxString("		    (list (wxxml-dissym-to-string (car (get (caar x) \'dissym)))))\n") +
	wxString("	  r (append (list (wxxml-dissym-to-string (cdr (get (caar x) \'dissym))))\n") +
	wxString("		    r)\n") +
	wxString("	  x (wxxml-list (cdr x) nil r \"<t>,</t>\"))\n") +
	wxString("    (append l x))\n") +
	wxString("\n") +
	wxString("  (defun wxxml-nary (x l r)\n") +
	wxString("    (let* ((op (caar x))\n") +
	wxString("	   (sym (cond ((member op \'(mtimes wxtimes) :test #\'eq)\n") +
	wxString("		       (if $stardisp\n") +
	wxString("			   \"<t>*</t>\"\n") +
	wxString("			 \"<h>*</h>\"))\n") +
	wxString("					;((wxxmlsym op))\n") +
	wxString("		      ((eq (get op \'dimension) \'dimension-nary)\n") +
	wxString("		       (wxxml-dissym-to-string (get op \'dissym)))))\n") +
	wxString("	   (y (cdr x))\n") +
	wxString("	   (ext-lop lop)\n") +
	wxString("	   (ext-rop rop))\n") +
	wxString("      (cond ((null y)\n") +
	wxString("	     (wxxml-function x l r)) ; this should not happen\n") +
	wxString("	    ((null (cdr y))\n") +
	wxString("	     (wxxml-function x l r)) ; this should not happen, too\n") +
	wxString("	    (t (do ((nl) (lop ext-lop op)\n") +
	wxString("		    (rop op (if (null (cdr y)) ext-rop op)))\n") +
	wxString("		   ((null (cdr y))\n") +
	wxString("		    (setq nl (nconc nl (wxxml (car y) l r lop rop))) nl)\n") +
	wxString("		   (setq nl (nconc nl (wxxml (car y) l (list sym) lop rop))\n") +
	wxString("			 y (cdr y)\n") +
	wxString("			 l nil))))))\n") +
	wxString("\n") +
	wxString("  (defun wxxml (x l r lop rop)\n") +
	wxString("    ;; x is the expression of interest; l is the list of strings to its\n") +
	wxString("    ;; left, r to its right. lop and rop are the operators on the left\n") +
	wxString("    ;; and right of x in the tree, and will determine if parens must\n") +
	wxString("    ;; be inserted\n") +
	wxString("    (setq x (nformat x))\n") +
	wxString("    (cond ((atom x) (wxxml-atom x l r))\n") +
	wxString("	  ((not (listp (car x)))\n") +
	wxString("	   (wxxml (cons \'(mlist simp) x) l r lop rop))\n") +
	wxString("	  ((or (<= (wxxml-lbp (caar x)) (wxxml-rbp lop))\n") +
	wxString("	       (> (wxxml-lbp rop) (wxxml-rbp (caar x))))\n") +
	wxString("	   (wxxml-paren x l r))\n") +
	wxString("	  ;; special check needed because macsyma notates arrays peculiarly\n") +
	wxString("	  ((member \'array (cdar x) :test #\'eq) (wxxml-array x l r))\n") +
	wxString("	  ;; dispatch for object-oriented wxxml-ifiying\n") +
	wxString("	  ((wxxml-get (caar x) \'wxxml) (funcall (get (caar x) \'wxxml) x l r))\n") +
	wxString("	  ((equal (wxxml-get (caar x) \'dimension) \'dimension-infix)\n") +
	wxString("	   (wxxml-infix x l r))\n") +
	wxString("	  ((equal (wxxml-get (caar x) \'dimension) \'dimension-match)\n") +
	wxString("	   (wxxml-matchfix-dim x l r))\n") +
	wxString("	  ((equal (wxxml-get (caar x) \'dimension) \'dimension-nary)\n") +
	wxString("	   (wxxml-nary x l r))\n") +
	wxString("	  ((equal (wxxml-get (caar x) \'dimension) \'dimension-postfix)\n") +
	wxString("	   (wxxml-postfix x l r))\n") +
	wxString("	  ((wxxml-get (caar x) \'defstruct-template)\n") +
	wxString("	   (wxxml-defstruct x l r))\n") +
	wxString("	  (t (wxxml-function x l r))))\n") +
	wxString("\n") +
	wxString("  (defun wxxml-paren (x l r)\n") +
	wxString("    (wxxml x (append l \'(\"<r><p>\")) (cons \"</p></r>\" r) \'mparen \'mparen))\n") +
	wxString("\n") +
	wxString("  ;; set up a list , separated by symbols (, * ...)  and then tack on the\n") +
	wxString("  ;; ending item (e.g. \"]\" or perhaps \")\"\n") +
	wxString("  (defun wxxml-list (x l r sym)\n") +
	wxString("    (if (null x) r\n") +
	wxString("      (do ((nl))\n") +
	wxString("	  ((null (cdr x))\n") +
	wxString("	   (setq nl (nconc nl (wxxml (car x)  l r \'mparen \'mparen)))\n") +
	wxString("	   nl)\n") +
	wxString("	  (setq nl (nconc nl (wxxml (car x)  l (list sym) \'mparen \'mparen))\n") +
	wxString("		x (cdr x)\n") +
	wxString("		l nil))))\n") +
	wxString("\n") +
	wxString(";;; Now we have functions which are called via property lists\n") +
	wxString("\n") +
	wxString("  (defun wxxml-prefix (x l r)\n") +
	wxString("    (wxxml (cadr x) (append l (wxxmlsym (caar x))) r (caar x) rop))\n") +
	wxString("\n") +
	wxString("  (defun wxxml-infix (x l r)\n") +
	wxString("    ;; check for 2 args\n") +
	wxString("    (if (or (null (cddr x)) (cdddr x)) (wna-err (caar x)))\n") +
	wxString("    (setq l (wxxml (cadr x) l nil lop (caar x)))\n") +
	wxString("    (wxxml (caddr x) (append l (wxxmlsym (caar x))) r (caar x) rop))\n") +
	wxString("\n") +
	wxString("  (defun wxxml-postfix (x l r)\n") +
	wxString("    (wxxml (cadr x) l (append (wxxmlsym (caar x)) r) lop (caar x)))\n") +
	wxString("\n") +
	wxString("  (defun wxxml-nofix (x l r) (wxxml (caar x) l r (caar x) rop))\n") +
	wxString("\n") +
	wxString("  (defun wxxml-matchfix (x l r)\n") +
	wxString("    (setq l (append l (car (wxxmlsym (caar x))))\n") +
	wxString("	  ;; car of wxxmlsym of a matchfix operator is the lead op\n") +
	wxString("	  r (append (cdr (wxxmlsym (caar x))) r)\n") +
	wxString("	  ;; cdr is the trailing op\n") +
	wxString("	  x (wxxml-list (cdr x) nil r \"<t>,</t>\"))\n") +
	wxString("    (append l x))\n") +
	wxString("\n") +
	wxString("\n") +
	wxString("  (defun wxxml-dissym-to-string (lst &aux pname)\n") +
	wxString("    (setq pname\n") +
	wxString("	  (wxxml-fix-string (format nil \"~{~a~}\" lst)))\n") +
	wxString("    (concatenate \'string \"<v>\" pname \"</v>\"))\n") +
	wxString("\n") +
	wxString("  (defun wxxmlsym (x)\n") +
	wxString("    (or (get x \'wxxmlsym)\n") +
	wxString("	(get x \'strsym)\n") +
	wxString("	(and (get x \'dissym)\n") +
	wxString("	     (list (wxxml-dissym-to-string (get x \'dissym))))\n") +
	wxString("	(list (stripdollar x))))\n") +
	wxString("\n") +
	wxString("  (defun wxxmlword (x)\n") +
	wxString("    (or (get x \'wxxmlword)\n") +
	wxString("	(stripdollar x)))\n") +
	wxString("\n") +
	wxString("  (defprop bigfloat wxxml-bigfloat wxxml)\n") +
	wxString("\n") +
	wxString("  ;;(defun mathml-bigfloat (x l r) (declare (ignore l r)) (fpformat x))\n") +
	wxString("  (defun wxxml-bigfloat (x l r)\n") +
	wxString("    (append l \'(\"<n>\") (fpformat x) \'(\"</n>\") r))\n") +
	wxString("\n") +
	wxString("  (defprop mprog  \"<fnm>block</fnm>\" wxxmlword)\n") +
	wxString("  (defprop $true  \"<t>true</t>\"  wxxmlword)\n") +
	wxString("  (defprop $false \"<t>false</t>\" wxxmlword)\n") +
	wxString("\n") +
	wxString("  (defprop mprogn wxxml-matchfix wxxml)\n") +
	wxString("  (defprop mprogn ((\"<r><p>\") \"</p></r>\") wxxmlsym)\n") +
	wxString("\n") +
	wxString("  (defprop mlist wxxml-matchfix wxxml)\n") +
	wxString("  (defprop mlist ((\"<r><t>[</t>\")\"<t>]</t></r>\") wxxmlsym)\n") +
	wxString("\n") +
	wxString("  (defprop $set wxxml-matchfix wxxml)\n") +
	wxString("  (defprop $set ((\"<r><t>{</t>\")\"<t>}</t></r>\") wxxmlsym)\n") +
	wxString("\n") +
	wxString("  (defprop mabs wxxml-matchfix wxxml)\n") +
	wxString("  (defprop mabs ((\"<r><a>\")\"</a></r>\") wxxmlsym)\n") +
	wxString("\n") +
	wxString("  (defprop $conjugate wxxml-matchfix wxxml)\n") +
	wxString("  (defprop $conjugate ((\"<r><cj>\")\"</cj></r>\") wxxmlsym)\n") +
	wxString("\n") +
	wxString("  (defprop %conjugate wxxml-matchfix wxxml)\n") +
	wxString("  (defprop %conjugate ((\"<r><cj>\")\"</cj></r>\") wxxmlsym)\n") +
	wxString("\n") +
	wxString("  (defprop mbox wxxml-mbox wxxml)\n") +
	wxString("  (defprop mlabox wxxml-mbox wxxml)\n") +
	wxString("\n") +
	wxString("  (defprop mbox 10. wxxml-rbp)\n") +
	wxString("  (defprop mbox 10. wxxml-lbp)\n") +
	wxString("\n") +
	wxString("  (defprop mlabbox 10. wxxml-rbp)\n") +
	wxString("  (defprop mlabbox 10. wxxml-lbp)\n") +
	wxString("\n") +
	wxString("  (defun wxxml-mbox (x l r)\n") +
	wxString("    (setq l (wxxml (cadr x) (append l \'(\"<r><hl>\")) nil \'mparen \'mparen)\n") +
	wxString("	  r (append \'(\"</hl></r>\") r))\n") +
	wxString("    (append l r))\n") +
	wxString("\n") +
	wxString("  (defprop mqapply wxxml-mqapply wxxml)\n") +
	wxString("\n") +
	wxString("  (defun wxxml-mqapply (x l r)\n") +
	wxString("    (setq l (wxxml (cadr x) (append l \'(\"<fn>\"))\n") +
	wxString("		   (list \"<p>\" ) lop \'mfunction)\n") +
	wxString("	  r (wxxml-list (cddr x) nil (cons \"</p></fn>\" r) \"<t>,</t>\"))\n") +
	wxString("    (append l r))\n") +
	wxString("\n") +
	wxString("\n") +
	wxString("  (defprop $zeta \"<g>zeta</g>\" wxxmlword)\n") +
	wxString("  (defprop %zeta \"<g>zeta</g>\" wxxmlword)\n") +
	wxString("\n") +
	wxString("  ;;\n") +
	wxString("  ;; Greek characters\n") +
	wxString("  ;;\n") +
	wxString("  (defprop $%alpha \"<g>%alpha</g>\" wxxmlword)\n") +
	wxString("  (defprop $alpha \"<g>alpha</g>\" wxxmlword)\n") +
	wxString("  (defprop $%beta \"<g>%beta</g>\" wxxmlword)\n") +
	wxString("  (defprop $beta \"<g>beta</g>\" wxxmlword)\n") +
	wxString("  (defprop $%gamma \"<g>%gamma</g>\" wxxmlword)\n") +
	wxString("  (defprop %gamma \"<g>gamma</g>\" wxxmlword)\n") +
	wxString("  (defprop $%delta \"<g>%delta</g>\" wxxmlword)\n") +
	wxString("  (defprop $delta \"<g>delta</g>\" wxxmlword)\n") +
	wxString("  (defprop $%epsilon \"<g>%epsilon</g>\" wxxmlword)\n") +
	wxString("  (defprop $epsilon \"<g>epsilon</g>\" wxxmlword)\n") +
	wxString("  (defprop $%zeta \"<g>%zeta</g>\" wxxmlword)\n") +
	wxString("  (defprop $%eta \"<g>%eta</g>\" wxxmlword)\n") +
	wxString("  (defprop $eta \"<g>eta</g>\" wxxmlword)\n") +
	wxString("  (defprop $%theta \"<g>%theta</g>\" wxxmlword)\n") +
	wxString("  (defprop $theta \"<g>theta</g>\" wxxmlword)\n") +
	wxString("  (defprop $%iota \"<g>%iota</g>\" wxxmlword)\n") +
	wxString("  (defprop $iota \"<g>iota</g>\" wxxmlword)\n") +
	wxString("  (defprop $%kappa \"<g>%kappa</g>\" wxxmlword)\n") +
	wxString("  (defprop $kappa \"<g>kappa</g>\" wxxmlword)\n") +
	wxString("  (defprop $%lambda \"<g>%lambda</g>\" wxxmlword)\n") +
	wxString("  (defprop $lambda \"<g>lambda</g>\" wxxmlword)\n") +
	wxString("  (defprop $%mu \"<g>%mu</g>\" wxxmlword)\n") +
	wxString("  (defprop $mu \"<g>mu</g>\" wxxmlword)\n") +
	wxString("  (defprop $%nu \"<g>%nu</g>\" wxxmlword)\n") +
	wxString("  (defprop $nu \"<g>nu</g>\" wxxmlword)\n") +
	wxString("  (defprop $%xi \"<g>%xi</g>\" wxxmlword)\n") +
	wxString("  (defprop $xi \"<g>xi</g>\" wxxmlword)\n") +
	wxString("  (defprop $%omicron \"<g>%omicron</g>\" wxxmlword)\n") +
	wxString("  (defprop $omicron \"<g>omicron</g>\" wxxmlword)\n") +
	wxString("  (defprop $%pi \"<s>%pi</s>\" wxxmlword)\n") +
	wxString("  (defprop $pi \"<g>pi</g>\" wxxmlword)\n") +
	wxString("  (defprop $%rho \"<g>%rho</g>\" wxxmlword)\n") +
	wxString("  (defprop $rho \"<g>rho</g>\" wxxmlword)\n") +
	wxString("  (defprop $%sigma \"<g>%sigma</g>\" wxxmlword)\n") +
	wxString("  (defprop $sigma \"<g>sigma</g>\" wxxmlword)\n") +
	wxString("  (defprop $%tau \"<g>%tau</g>\" wxxmlword)\n") +
	wxString("  (defprop $tau \"<g>tau</g>\" wxxmlword)\n") +
	wxString("  (defprop $%upsilon \"<g>%upsilon</g>\" wxxmlword)\n") +
	wxString("  (defprop $upsilon \"<g>upsilon</g>\" wxxmlword)\n") +
	wxString("  (defprop $%phi \"<g>%phi</g>\" wxxmlword)\n") +
	wxString("  (defprop $phi \"<g>phi</g>\" wxxmlword)\n") +
	wxString("  (defprop $%chi \"<g>%chi</g>\" wxxmlword)\n") +
	wxString("  (defprop $chi \"<g>chi</g>\" wxxmlword)\n") +
	wxString("  (defprop $%psi \"<g>%psi</g>\" wxxmlword)\n") +
	wxString("  (defprop $psi \"<g>psi</g>\" wxxmlword)\n") +
	wxString("  (defprop $%omega \"<g>%omega</g>\" wxxmlword)\n") +
	wxString("  (defprop $omega \"<g>omega</g>\" wxxmlword)\n") +
	wxString("  (defprop |$%Alpha| \"<g>%Alpha</g>\" wxxmlword)\n") +
	wxString("  (defprop |$Alpha| \"<g>Alpha</g>\" wxxmlword)\n") +
	wxString("  (defprop |$%Beta| \"<g>%Beta</g>\" wxxmlword)\n") +
	wxString("  (defprop |$Beta| \"<g>Beta</g>\" wxxmlword)\n") +
	wxString("  (defprop |$%Gamma| \"<g>%Gamma</g>\" wxxmlword)\n") +
	wxString("  (defprop |$Gamma| \"<g>Gamma</g>\" wxxmlword)\n") +
	wxString("  (defprop |$%Delta| \"<g>%Delta</g>\" wxxmlword)\n") +
	wxString("  (defprop |$Delta| \"<g>Delta</g>\" wxxmlword)\n") +
	wxString("  (defprop |$%Epsilon| \"<g>%Epsilon</g>\" wxxmlword)\n") +
	wxString("  (defprop |$Epsilon| \"<g>Epsilon</g>\" wxxmlword)\n") +
	wxString("  (defprop |$%Zeta| \"<g>%Zeta</g>\" wxxmlword)\n") +
	wxString("  (defprop |$Zeta| \"<g>Zeta</g>\" wxxmlword)\n") +
	wxString("  (defprop |$%Eta| \"<g>%Eta</g>\" wxxmlword)\n") +
	wxString("  (defprop |$Eta| \"<g>Eta</g>\" wxxmlword)\n") +
	wxString("  (defprop |$%Theta| \"<g>%Theta</g>\" wxxmlword)\n") +
	wxString("  (defprop |$Theta| \"<g>Theta</g>\" wxxmlword)\n") +
	wxString("  (defprop |$%Iota| \"<g>%Iota</g>\" wxxmlword)\n") +
	wxString("  (defprop |$Iota| \"<g>Iota</g>\" wxxmlword)\n") +
	wxString("  (defprop |$%Kappa| \"<g>%Kappa</g>\" wxxmlword)\n") +
	wxString("  (defprop |$Kappa| \"<g>Kappa</g>\" wxxmlword)\n") +
	wxString("  (defprop |$%Lambda| \"<g>%Lambda</g>\" wxxmlword)\n") +
	wxString("  (defprop |$Lambda| \"<g>Lambda</g>\" wxxmlword)\n") +
	wxString("  (defprop |$%Mu| \"<g>%Mu</g>\" wxxmlword)\n") +
	wxString("  (defprop |$Mu| \"<g>Mu</g>\" wxxmlword)\n") +
	wxString("  (defprop |$%Nu| \"<g>%Nu</g>\" wxxmlword)\n") +
	wxString("  (defprop |$Nu| \"<g>Nu</g>\" wxxmlword)\n") +
	wxString("  (defprop |$%Xi| \"<g>%Xi</g>\" wxxmlword)\n") +
	wxString("  (defprop |$Xi| \"<g>Xi</g>\" wxxmlword)\n") +
	wxString("  (defprop |$%Omicron| \"<g>%Omicron</g>\" wxxmlword)\n") +
	wxString("  (defprop |$Omicron| \"<g>Omicron</g>\" wxxmlword)\n") +
	wxString("  (defprop |$%Rho| \"<g>%Rho</g>\" wxxmlword)\n") +
	wxString("  (defprop |$Rho| \"<g>Rho</g>\" wxxmlword)\n") +
	wxString("  (defprop |$%Sigma| \"<g>%Sigma</g>\" wxxmlword)\n") +
	wxString("  (defprop |$Sigma| \"<g>Sigma</g>\" wxxmlword)\n") +
	wxString("  (defprop |$%Tau| \"<g>%Tau</g>\" wxxmlword)\n") +
	wxString("  (defprop |$Tau| \"<g>Tau</g>\" wxxmlword)\n") +
	wxString("  (defprop |$%Upsilon| \"<g>%Upsilon</g>\" wxxmlword)\n") +
	wxString("  (defprop |$Upsilon| \"<g>Upsilon</g>\" wxxmlword)\n") +
	wxString("  (defprop |$%Phi| \"<g>%Phi</g>\" wxxmlword)\n") +
	wxString("  (defprop |$Phi| \"<g>Phi</g>\" wxxmlword)\n") +
	wxString("  (defprop |$%Chi| \"<g>%Chi</g>\" wxxmlword)\n") +
	wxString("  (defprop |$Chi| \"<g>Chi</g>\" wxxmlword)\n") +
	wxString("  (defprop |$%Psi| \"<g>%Psi</g>\" wxxmlword)\n") +
	wxString("  (defprop |$Psi| \"<g>Psi</g>\" wxxmlword)\n") +
	wxString("  (defprop |$%Omega| \"<g>%Omega</g>\" wxxmlword)\n") +
	wxString("  (defprop |$Omega| \"<g>Omega</g>\" wxxmlword)\n") +
	wxString("  (defprop |$%Pi| \"<g>%Pi</g>\" wxxmlword)\n") +
	wxString("  (defprop |$Pi| \"<g>Pi</g>\" wxxmlword)\n") +
	wxString("\n") +
	wxString("  (defprop $%i \"<s>%i</s>\" wxxmlword)\n") +
	wxString("  (defprop $%e \"<s>%e</s>\" wxxmlword)\n") +
	wxString("  (defprop $inf \"<s>inf</s>\" wxxmlword)\n") +
	wxString("  (defprop $minf \"<t>-</t><s>inf</s>\" wxxmlword)\n") +
	wxString("\n") +
	wxString("  (defprop mreturn \"return\" wxxmlword)\n") +
	wxString("\n") +
	wxString("  (defprop mquote wxxml-prefix wxxml)\n") +
	wxString("  (defprop mquote (\"<t>\'</t>\") wxxmlsym)\n") +
	wxString("  (defprop mquote \"<t>\'</t>\" wxxmlword)\n") +
	wxString("  (defprop mquote 201. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defprop msetq wxxml-infix wxxml)\n") +
	wxString("  (defprop msetq (\"<t>:</t>\") wxxmlsym)\n") +
	wxString("  (defprop msetq \"<t>:</t>\" wxxmlword)\n") +
	wxString("  (defprop msetq 180. wxxml-rbp)\n") +
	wxString("  (defprop msetq 20. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defprop mset wxxml-infix wxxml)\n") +
	wxString("  (defprop mset (\"<t>::</t>\") wxxmlsym)\n") +
	wxString("  (defprop mset \"<t>::</t>\" wxxmlword)\n") +
	wxString("  (defprop mset 180. wxxml-lbp)\n") +
	wxString("  (defprop mset 20. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defprop mdefine wxxml-infix wxxml)\n") +
	wxString("  (defprop mdefine (\"<t>:=</t>\") wxxmlsym)\n") +
	wxString("  (defprop mdefine \"<t>:=</t>\" wxxmlword)\n") +
	wxString("  (defprop mdefine 180. wxxml-lbp)\n") +
	wxString("  (defprop mdefine 20. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defprop mdefmacro wxxml-infix wxxml)\n") +
	wxString("  (defprop mdefmacro (\"<t>::=</t>\") wxxmlsym)\n") +
	wxString("  (defprop mdefmacro \"<t>::=</t>\" wxxmlword)\n") +
	wxString("  (defprop mdefmacro 180. wxxml-lbp)\n") +
	wxString("  (defprop mdefmacro 20. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defprop marrow wxxml-infix wxxml)\n") +
	wxString("  (defprop marrow (\"<t>-></t>\") wxxmlsym)\n") +
	wxString("  (defprop marrow \"<t>-></t>\" wxxmlword)\n") +
	wxString("  (defprop marrow 25 wxxml-lbp)\n") +
	wxString("  (defprop marrow 25 wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defprop mfactorial wxxml-postfix wxxml)\n") +
	wxString("  (defprop mfactorial (\"<t>!</t>\") wxxmlsym)\n") +
	wxString("  (defprop mfactorial \"<t>!</t>\" wxxmlword)\n") +
	wxString("  (defprop mfactorial 160. wxxml-lbp)\n") +
	wxString("\n") +
	wxString("  (defprop mexpt wxxml-mexpt wxxml)\n") +
	wxString("  (defprop mexpt 140. wxxml-lbp)\n") +
	wxString("  (defprop mexpt 139. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defprop %sum 90. wxxml-rbp)\n") +
	wxString("  (defprop %product 95. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  ;; insert left-angle-brackets for mncexpt. a^<t> is how a^^n looks.\n") +
	wxString("\n") +
	wxString("  (defun wxxml-mexpt (x l r)\n") +
	wxString("    (cond ((atom (cadr x))\n") +
	wxString("	   (wxxml-mexpt-simple x l r))\n") +
	wxString("	  ((member \'array (caadr x))\n") +
	wxString("	   (wxxml-mexpt-array x l r))\n") +
	wxString("	  (t\n") +
	wxString("	   (wxxml-mexpt-simple x l r))))\n") +
	wxString("\n") +
	wxString("  (defun wxxml-mexpt-array (x l r)\n") +
	wxString("    (let* ((nc (eq (caar x) \'mncexpt))\n") +
	wxString("	   f (xarr (cadr x))\n") +
	wxString("	   (xexp (nformat (caddr x))))\n") +
	wxString("      ;; the index part\n") +
	wxString("      (if (eq \'mqapply (caar xarr))\n") +
	wxString("	  (setq f (cadr xarr)\n") +
	wxString("		xarr (cdr xarr)\n") +
	wxString("		l (wxxml f (append l (list \"<ie><p>\")) (list \"</p>\")\n") +
	wxString("			 \'mparen \'mparen))\n") +
	wxString("	(setq f (caar xarr)\n") +
	wxString("	      l (wxxml f (append l (if nc\n") +
	wxString("				       (list \"<ie mat=\\\"true\\\"><r>\")\n") +
	wxString("				     (list \"<ie><r>\")))\n") +
	wxString("		       (list \"</r>\") lop \'mfunction)))\n") +
	wxString("      (setq  l (append l (wxxml-list (cdr xarr) (list \"<r>\")\n") +
	wxString("				     (list \"</r>\") \"<v>,</v>\")))\n") +
	wxString("      ;; The exponent part\n") +
	wxString("      (setq r (if (mmminusp xexp)\n") +
	wxString("		  ;; the change in base-line makes parens unnecessary\n") +
	wxString("		  (wxxml (cadr xexp) \'(\"<r><v>-</v>\")\n") +
	wxString("			 (cons \"</r></ie>\" r) \'mparen \'mparen)\n") +
	wxString("		(if (and (integerp xexp) (< xexp 10))\n") +
	wxString("		    (wxxml xexp nil\n") +
	wxString("			   (cons \"</ie>\" r) \'mparen \'mparen)\n") +
	wxString("		  (wxxml xexp (list \"<r>\")\n") +
	wxString("			 (cons \"</r></ie>\" r) \'mparen \'mparen)\n") +
	wxString("		  )))\n") +
	wxString("      (append l r)))\n") +
	wxString("\n") +
	wxString("  (defun wxxml-mexpt-simple (x l r)\n") +
	wxString("    (let((nc (eq (caar x) \'mncexpt)))\n") +
	wxString("      (setq l (wxxml (cadr x) (append l (if nc\n") +
	wxString("					    \'(\"<e mat=\\\"true\\\"><r>\")\n") +
	wxString("					  \'(\"<e><r>\")))\n") +
	wxString("		     nil lop (caar x))\n") +
	wxString("	    r (if (mmminusp (setq x (nformat (caddr x))))\n") +
	wxString("		  ;; the change in base-line makes parens unnecessary\n") +
	wxString("		  (wxxml (cadr x) \'(\"</r><r><v>-</v>\")\n") +
	wxString("			 (cons \"</r></e>\" r) \'mminus \'mminus)\n") +
	wxString("		(if (and (integerp x) (< x 10))\n") +
	wxString("		    (wxxml x (list \"</r>\")\n") +
	wxString("			   (cons \"</e>\" r) \'mparen \'mparen)\n") +
	wxString("		  (wxxml x (list \"</r><r>\")\n") +
	wxString("			 (cons \"</r></e>\" r) \'mparen \'mparen)\n") +
	wxString("		  )))\n") +
	wxString("      (append l r)))\n") +
	wxString("\n") +
	wxString("  (defprop mncexpt wxxml-mexpt wxxml)\n") +
	wxString("\n") +
	wxString("  (defprop mncexpt 135. wxxml-lbp)\n") +
	wxString("  (defprop mncexpt 134. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defprop mnctimes wxxml-nary wxxml)\n") +
	wxString("  (defprop mnctimes \"<t>.</t>\" wxxmlsym)\n") +
	wxString("  (defprop mnctimes \"<t>.</t>\" wxxmlword)\n") +
	wxString("  (defprop mnctimes 110. wxxml-lbp)\n") +
	wxString("  (defprop mnctimes 109. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defprop mtimes wxxml-nary wxxml)\n") +
	wxString("  (defprop mtimes \"<h>*</h>\" wxxmlsym)\n") +
	wxString("  (defprop mtimes \"<t>*</t>\" wxxmlword)\n") +
	wxString("  (defprop mtimes 120. wxxml-lbp)\n") +
	wxString("  (defprop mtimes 120. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defprop wxtimes wxxml-nary wxxml)\n") +
	wxString("  (defprop wxtimes \"<h>*</h>\" wxxmlsym)\n") +
	wxString("  (defprop wxtimes \"<t>*</t>\" wxxmlword)\n") +
	wxString("  (defprop wxtimes 120. wxxml-lbp)\n") +
	wxString("  (defprop wxtimes 120. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defprop %sqrt wxxml-sqrt wxxml)\n") +
	wxString("\n") +
	wxString("  (defun wxxml-sqrt (x l r)\n") +
	wxString("    (wxxml (cadr x) (append l  \'(\"<q>\"))\n") +
	wxString("	   (append \'(\"</q>\") r) \'mparen \'mparen))\n") +
	wxString("\n") +
	wxString("  (defprop mquotient wxxml-mquotient wxxml)\n") +
	wxString("  (defprop mquotient (\"<t>/</t>\") wxxmlsym)\n") +
	wxString("  (defprop mquotient \"<t>/</t>\" wxxmlword)\n") +
	wxString("  (defprop mquotient 122. wxxml-lbp) ;;dunno about this\n") +
	wxString("  (defprop mquotient 123. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defun wxxml-mquotient (x l r)\n") +
	wxString("    (if (or (null (cddr x)) (cdddr x)) (wna-err (caar x)))\n") +
	wxString("    (setq l (wxxml (cadr x) (append l \'(\"<f><r>\")) nil \'mparen \'mparen)\n") +
	wxString("	  r (wxxml (caddr x) (list \"</r><r>\")\n") +
	wxString("		   (append \'(\"</r></f>\")r) \'mparen \'mparen))\n") +
	wxString("    (append l r))\n") +
	wxString("\n") +
	wxString("  (defprop $matrix wxxml-matrix-test wxxml)\n") +
	wxString("\n") +
	wxString("  (defun wxxml-matrix-test (x l r)\n") +
	wxString("    (if (every #\'$listp (cdr x))\n") +
	wxString("	(wxxml-matrix x l r)\n") +
	wxString("      (wxxml-function x l r)))\n") +
	wxString("\n") +
	wxString("  (defun wxxml-matrix(x l r) ;;matrix looks like ((mmatrix)((mlist) a b) ...)\n") +
	wxString("    (cond ((null (cdr x))\n") +
	wxString("	   (append l `(\"<fn><fnm>matrix</fnm><p/></fn>\") r))\n") +
	wxString("	  ((and (null (cddr x))\n") +
	wxString("		(null (cdadr x)))\n") +
	wxString("	   (append l `(\"<fn><fnm>matrix</fnm><p><t>[</t><t>]</t></p></fn>\") r))\n") +
	wxString("	  (t\n") +
	wxString("	   (append l (cond\n") +
	wxString("		      ((find \'inference (car x))\n") +
	wxString("		       (list \"<tb inference=\\\"true\\\">\"))\n") +
	wxString("		      ((find \'special (car x))\n") +
	wxString("		       (list (format nil \"<tb special=\\\"true\\\" rownames=~s colnames=~s>\"\n") +
	wxString("				     (if (find \'rownames (car x)) \"true\" \"false\")\n") +
	wxString("				     (if (find \'colnames (car x)) \"true\" \"false\"))))\n") +
	wxString("		      ((string= $lmxchar #\\()\n") +
	wxString("		       (list \"<tb roundedParens=\\\"true\\\">\"))\n") +
	wxString("		      (t\n") +
	wxString("		       (list \"<tb>\")))\n") +
	wxString("		   (mapcan #\'(lambda (y)\n") +
	wxString("			       (cond ((null (cdr y))\n") +
	wxString("				      (list \"<mtr><mtd><mspace/></mtd></mtr>\"))\n") +
	wxString("				     (t\n") +
	wxString("				      (wxxml-list (cdr y)\n") +
	wxString("						  (list \"<mtr><mtd>\")\n") +
	wxString("						  (list \"</mtd></mtr>\")\n") +
	wxString("						  \"</mtd><mtd>\"))))\n") +
	wxString("			   (cdr x))\n") +
	wxString("		   `(\"</tb>\") r))))\n") +
	wxString("\n") +
	wxString("  ;; macsyma sum or prod is over integer range, not  low <= index <= high\n") +
	wxString("  ;; wxxml is lots more flexible .. but\n") +
	wxString("\n") +
	wxString("  (defprop %sum wxxml-sum wxxml)\n") +
	wxString("  (defprop %lsum wxxml-lsum wxxml)\n") +
	wxString("  (defprop %product wxxml-sum wxxml)\n") +
	wxString("  (defprop $sum wxxml-sum wxxml)\n") +
	wxString("  (defprop $lsum wxxml-lsum wxxml)\n") +
	wxString("  (defprop $product wxxml-sum wxxml)\n") +
	wxString("\n") +
	wxString("  ;; easily extended to union, intersect, otherops\n") +
	wxString("\n") +
	wxString("  (defun wxxml-lsum(x l r)\n") +
	wxString("    (let ((op \"<sm type=\\\"lsum\\\"><r>\")\n") +
	wxString("	  ;; gotta be one of those above\n") +
	wxString("	  (s1 (wxxml (cadr x) nil nil \'mparen rop));; summand\n") +
	wxString("	  (index ;; \"index = lowerlimit\"\n") +
	wxString("	   (wxxml `((min simp) , (caddr x), (cadddr x))\n") +
	wxString("		  nil nil \'mparen \'mparen)))\n") +
	wxString("      (append l `(,op ,@index\n") +
	wxString("		      \"</r><r><mn/></r><r>\"\n") +
	wxString("		      ,@s1 \"</r></sm>\") r)))\n") +
	wxString("\n") +
	wxString("  (defun wxxml-sum(x l r)\n") +
	wxString("    (let ((op (if (or (eq (caar x) \'%sum)\n") +
	wxString("		      (eq (caar x) \'$sum))\n") +
	wxString("		  \"<sm><r>\"\n") +
	wxString("		\"<sm type=\\\"prod\\\"><r>\"))\n") +
	wxString("	  (s1 (wxxml (cadr x) nil nil \'mparen rop));; summand\n") +
	wxString("	  (index ;; \"index = lowerlimit\"\n") +
	wxString("	   (wxxml `((mequal simp) ,(caddr x) ,(cadddr x))\n") +
	wxString("		  nil nil \'mparen \'mparen))\n") +
	wxString("	  (toplim (wxxml (car (cddddr x)) nil nil \'mparen \'mparen)))\n") +
	wxString("      (append l `( ,op ,@index \"</r><r>\" ,@toplim\n") +
	wxString("		       \"</r><r>\"\n") +
	wxString("		       ,@s1 \"</r></sm>\") r)))\n") +
	wxString("\n") +
	wxString("  (defprop %integrate wxxml-int wxxml)\n") +
	wxString("  (defprop $integrate wxxml-int wxxml)\n") +
	wxString("\n") +
	wxString("  (defun wxxml-int (x l r)\n") +
	wxString("    (let ((s1 (wxxml (cadr x) nil nil \'mparen \'mparen));;integrand delims / & d\n") +
	wxString("	  (var (wxxml (caddr x) nil nil \'mparen rop))) ;; variable\n") +
	wxString("      (cond ((= (length x) 3)\n") +
	wxString("	     (append l `(\"<in def=\\\"false\\\"><r>\"\n") +
	wxString("			 ,@s1\n") +
	wxString("			 \"</r><r><s>d</s>\"\n") +
	wxString("			 ,@var\n") +
	wxString("			 \"</r></in>\") r))\n") +
	wxString("	    (t ;; presumably length 5\n") +
	wxString("	     (let ((low (wxxml (nth 3 x) nil nil \'mparen \'mparen))\n") +
	wxString("		   ;; 1st item is 0\n") +
	wxString("		   (hi (wxxml (nth 4 x) nil nil \'mparen \'mparen)))\n") +
	wxString("	       (append l `(\"<in><r>\"\n") +
	wxString("			   ,@low\n") +
	wxString("			   \"</r><r>\"\n") +
	wxString("			   ,@hi\n") +
	wxString("			   \"</r><r>\"\n") +
	wxString("			   ,@s1\n") +
	wxString("			   \"</r><r><s>d</s>\"\n") +
	wxString("			   ,@var \"</r></in>\") r))))))\n") +
	wxString("\n") +
	wxString("  (defprop %limit wxxml-limit wxxml)\n") +
	wxString("\n") +
	wxString("  (defprop mrarr wxxml-infix wxxml)\n") +
	wxString("  (defprop mrarr (\"<t>-></t>\") wxxmlsym)\n") +
	wxString("  (defprop mrarr 80. wxxml-lbp)\n") +
	wxString("  (defprop mrarr 80. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defun wxxml-limit (x l r) ;; ignoring direction, last optional arg to limit\n") +
	wxString("    (let ((s1 (wxxml (second x) nil nil \'mparen rop));; limitfunction\n") +
	wxString("	  (subfun ;; the thing underneath \"limit\"\n") +
	wxString("	   (wxxml `((mrarr simp) ,(third x)\n") +
	wxString("		    ,(fourth x)) nil nil \'mparen \'mparen)))\n") +
	wxString("      (case (fifth x)\n") +
	wxString("	    ($plus\n") +
	wxString("	     (append l `(\"<lm><fnm>lim</fnm><r>\"\n") +
	wxString("			 ,@subfun \"<v>+</v></r><r>\"\n") +
	wxString("			 ,@s1 \"</r></lm>\") r))\n") +
	wxString("	    ($minus\n") +
	wxString("	     (append l `(\"<lm><fnm>lim</fnm><r>\"\n") +
	wxString("			 ,@subfun \"<t>-</t></r><r>\"\n") +
	wxString("			 ,@s1 \"</r></lm>\") r))\n") +
	wxString("	    (otherwise\n") +
	wxString("	     (append l `(\"<lm><fnm>lim</fnm><r>\"\n") +
	wxString("			 ,@subfun \"</r><r>\"\n") +
	wxString("			 ,@s1 \"</r></lm>\") r)))))\n") +
	wxString("\n") +
	wxString("  (defprop %at wxxml-at wxxml)\n") +
	wxString("  ;; e.g.  at(diff(f(x)),x=a)\n") +
	wxString("  (defun wxxml-at (x l r)\n") +
	wxString("    (let ((s1 (wxxml (cadr x) nil nil lop rop))\n") +
	wxString("	  (sub (wxxml (caddr x) nil nil \'mparen \'mparen)))\n") +
	wxString("      (append l \'(\"<at><r>\") s1\n") +
	wxString("	      \'(\"</r><r>\") sub \'(\"</r></at>\") r)))\n") +
	wxString("\n") +
	wxString("  ;;binomial coefficients\n") +
	wxString("\n") +
	wxString("  (defprop %binomial wxxml-choose wxxml)\n") +
	wxString("\n") +
	wxString("\n") +
	wxString("  (defun wxxml-choose (x l r)\n") +
	wxString("    `(,@l\n") +
	wxString("      \"<p print=\\\"no\\\"><f line=\\\"no\\\"><r>\"\n") +
	wxString("      ,@(wxxml (cadr x) nil nil \'mparen \'mparen)\n") +
	wxString("      \"</r><r>\"\n") +
	wxString("      ,@(wxxml (caddr x) nil nil \'mparen \'mparen)\n") +
	wxString("      \"</r></f></p>\"\n") +
	wxString("      ,@r))\n") +
	wxString("\n") +
	wxString("\n") +
	wxString("  (defprop rat wxxml-rat wxxml)\n") +
	wxString("  (defprop rat 120. wxxml-lbp)\n") +
	wxString("  (defprop rat 121. wxxml-rbp)\n") +
	wxString("  (defun wxxml-rat(x l r) (wxxml-mquotient x l r))\n") +
	wxString("\n") +
	wxString("  (defprop mplus wxxml-mplus wxxml)\n") +
	wxString("  (defprop mplus 100. wxxml-lbp)\n") +
	wxString("  (defprop mplus 100. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defun wxxml-mplus (x l r)\n") +
	wxString("    (cond ((member \'trunc (car x) :test #\'eq)\n") +
	wxString("	   (setq r (cons \"<v>+</v><t>...</t>\" r))))\n") +
	wxString("    (cond ((null (cddr x))\n") +
	wxString("	   (if (null (cdr x))\n") +
	wxString("	       (wxxml-function x l r)\n") +
	wxString("	     (wxxml (cadr x) l r \'mplus rop)))\n") +
	wxString("	  (t (setq l (wxxml (cadr x) l nil lop \'mplus)\n") +
	wxString("		   x (cddr x))\n") +
	wxString("	     (do ((nl l)  (dissym))\n") +
	wxString("		 ((null (cdr x))\n") +
	wxString("		  (if (mmminusp (car x)) (setq l (cadar x) dissym\n") +
	wxString("					       (list \"<v>-</v>\"))\n") +
	wxString("		    (setq l (car x) dissym (list \"<v>+</v>\")))\n") +
	wxString("		  (setq r (wxxml l dissym r \'mplus rop))\n") +
	wxString("		  (append nl r))\n") +
	wxString("		 (if (mmminusp (car x)) (setq l (cadar x) dissym\n") +
	wxString("					      (list \"<v>-</v>\"))\n") +
	wxString("		   (setq l (car x) dissym (list \"<v>+</v>\")))\n") +
	wxString("		 (setq nl (append nl (wxxml l dissym nil \'mplus \'mplus))\n") +
	wxString("		       x (cdr x))))))\n") +
	wxString("\n") +
	wxString("  (defprop mminus wxxml-prefix wxxml)\n") +
	wxString("  (defprop mminus (\"<v>-</v>\") wxxmlsym)\n") +
	wxString("  (defprop mminus \"<v>-</v>\" wxxmlword)\n") +
	wxString("  (defprop mminus 101. wxxml-rbp)\n") +
	wxString("  (defprop mminus 101. wxxml-lbp)\n") +
	wxString("\n") +
	wxString("  (defprop $~ wxxml-infix wxxml)\n") +
	wxString("  (defprop $~ (\"<t>~</t>\") wxxmlsym)\n") +
	wxString("  (defprop $~ \"<t>~</t>\" wxxmlword)\n") +
	wxString("  (defprop $~ 134. wxxml-lbp)\n") +
	wxString("  (defprop $~ 133. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defprop min wxxml-infix wxxml)\n") +
	wxString("  (defprop min (\"<fnm>in</fnm>\") wxxmlsym)\n") +
	wxString("  (defprop min \"<fnm>in</fnm>\" wxxmlword)\n") +
	wxString("  (defprop min 80. wxxml-lbp)\n") +
	wxString("  (defprop min 80. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defprop mequal wxxml-infix wxxml)\n") +
	wxString("  (defprop mequal (\"<v>=</v>\") wxxmlsym)\n") +
	wxString("  (defprop mequal \"<v>=</v>\" wxxmlword)\n") +
	wxString("  (defprop mequal 80. wxxml-lbp)\n") +
	wxString("  (defprop mequal 80. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defprop mnotequal wxxml-infix wxxml)\n") +
	wxString("  (defprop mnotequal (\"<t>#</t>\") wxxmlsym)\n") +
	wxString("  (defprop mnotequal 80. wxxml-lbp)\n") +
	wxString("  (defprop mnotequal 80. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defprop mgreaterp wxxml-infix wxxml)\n") +
	wxString("  (defprop mgreaterp (\"<t>&gt;</t>\") wxxmlsym)\n") +
	wxString("  (defprop mgreaterp \"<t>&gt;</t>\" wxxmlword)\n") +
	wxString("  (defprop mgreaterp 80. wxxml-lbp)\n") +
	wxString("  (defprop mgreaterp 80. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defprop mgeqp wxxml-infix wxxml)\n") +
	wxString("  (defprop mgeqp (\"<t>&gt;=</t>\") wxxmlsym)\n") +
	wxString("  (defprop mgeqp \"<t>&gt;=</t>\" wxxmlword)\n") +
	wxString("  (defprop mgeqp 80. wxxml-lbp)\n") +
	wxString("  (defprop mgeqp 80. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defprop mlessp wxxml-infix wxxml)\n") +
	wxString("  (defprop mlessp (\"<t>&lt;</t>\") wxxmlsym)\n") +
	wxString("  (defprop mlessp \"<t>&lt;</t>\" wxxmlword)\n") +
	wxString("  (defprop mlessp 80. wxxml-lbp)\n") +
	wxString("  (defprop mlessp 80. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defprop mleqp wxxml-infix wxxml)\n") +
	wxString("  (defprop mleqp (\"<t>&lt;=</t>\") wxxmlsym)\n") +
	wxString("  (defprop mleqp \"<t>&lt;=</t>\" wxxmlword)\n") +
	wxString("  (defprop mleqp 80. wxxml-lbp)\n") +
	wxString("  (defprop mleqp 80. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defprop mnot wxxml-prefix wxxml)\n") +
	wxString("  (defprop mnot (\"<fnm altCopy=\\\"not \\\">not</fnm>\") wxxmlsym)\n") +
	wxString("  (defprop mnot \"<fnm>not</fnm>\" wxxmlword)\n") +
	wxString("  (defprop mnot 70. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defprop mand wxxml-nary wxxml)\n") +
	wxString("  (defprop mand \"<mspace/><fnm>and</fnm><mspace/>\" wxxmlsym)\n") +
	wxString("  (defprop mand \"<fnm>and</fnm>\" wxxmlword)\n") +
	wxString("  (defprop mand 60. wxxml-lbp)\n") +
	wxString("  (defprop mand 60. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defprop mor wxxml-nary wxxml)\n") +
	wxString("  (defprop mor \"<mspace/><fnm>or</fnm><mspace/>\" wxxmlsym)\n") +
	wxString("  (defprop mor \"<fnm>or</fnm>\" wxxmlword)\n") +
	wxString("  (defprop mor 50. wxxml-lbp)\n") +
	wxString("  (defprop mor 50. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("\n") +
	wxString("  (defprop mcond wxxml-mcond wxxml)\n") +
	wxString("  (defprop mcond 25. wxxml-lbp)\n") +
	wxString("  (defprop mcond 25. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defprop %derivative wxxml-derivative wxxml)\n") +
	wxString("  (defprop %derivative 120. wxxml-lbp)\n") +
	wxString("  (defprop %derivative 119. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defprop $diff wxxml-derivative wxxml)\n") +
	wxString("  (defprop $diff 120. wxxml-lbp)\n") +
	wxString("  (defprop $diff 119. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defun wxxml-derivative (x l r)\n") +
	wxString("    (if (and $derivabbrev\n") +
	wxString("	     (every #\'integerp (odds (cddr x) 0))\n") +
	wxString("	     (every #\'atom (odds (cddr x) 1)))\n") +
	wxString("	(append l (wxxml-d-abbrev x) r)\n") +
	wxString("      (wxxml (wxxml-d x) (append l \'(\"<d>\"))\n") +
	wxString("	     (append \'(\"</d>\") r) \'mparen \'mparen)))\n") +
	wxString("\n") +
	wxString("  (defun $derivabbrev (a)\n") +
	wxString("    (if a\n") +
	wxString("	(progn\n") +
	wxString("	  (defprop %derivative 130. wxxml-lbp)\n") +
	wxString("	  (defprop %derivative 129. wxxml-rbp)\n") +
	wxString("	  (setq $derivabbrev t))\n") +
	wxString("      (progn\n") +
	wxString("	(defprop %derivative 120. wxxml-lbp)\n") +
	wxString("	(defprop %derivative 119. wxxml-rbp)\n") +
	wxString("	(setq $derivabbrev nil))))\n") +
	wxString("\n") +
	wxString("  (defun wxxml-d-abbrev-subscript (l_vars l_ords &aux var_xml)\n") +
	wxString("    (let ((sub ()))\n") +
	wxString("      (loop while l_vars do\n") +
	wxString("	    (setq var_xml (car (wxxml (car l_vars) nil nil \'mparen \'mparen)))\n") +
	wxString("	    (loop for i from 1 to (car l_ords) do\n") +
	wxString("		  (setq sub (cons var_xml sub)))\n") +
	wxString("	    (setq l_vars (cdr l_vars)\n") +
	wxString("		  l_ords (cdr l_ords)))\n") +
	wxString("      (reverse sub)))\n") +
	wxString("\n") +
	wxString("  (defun wxxml-d-abbrev (x)\n") +
	wxString("    (let*\n") +
	wxString("	((difflist (cddr x))\n") +
	wxString("	 (ords (odds  difflist 0))\n") +
	wxString("	 (ords (cond ((null ords) \'(1))\n") +
	wxString("		     (t ords)))\n") +
	wxString("	 (vars (odds difflist 1))\n") +
	wxString("	 (fun (wxxml (cadr x) nil nil \'mparen \'mparen)))\n") +
	wxString("      (append \'(\"<i d=\\\"1\\\"><r>\") fun \'(\"</r>\")\n") +
	wxString("	      \'(\"<r>\") (wxxml-d-abbrev-subscript vars ords) \'(\"</r></i>\"))))\n") +
	wxString("\n") +
	wxString("  (defun wxxml-d (x)\n") +
	wxString("    ;; format the macsyma derivative form so it looks\n") +
	wxString("    ;; sort of like a quotient times the deriva-dand.\n") +
	wxString("    (let*\n") +
	wxString("	(($simp t)\n") +
	wxString("	 (arg (cadr x)) ;; the function being differentiated\n") +
	wxString("	 (difflist (cddr x)) ;; list of derivs e.g. (x 1 y 2)\n") +
	wxString("	 (ords (odds difflist 0)) ;; e.g. (1 2)\n") +
	wxString("	 (ords (cond ((null ords) \'(1))\n") +
	wxString("		     (t ords)))\n") +
	wxString("	 (vars (odds difflist 1)) ;; e.g. (x y)\n") +
	wxString("	 (dsym \'((wxxmltag simp) \"d\" \"s\"))\n") +
	wxString("	 (numer `((mexpt) ,dsym ((mplus) ,@ords))) ; d^n numerator\n") +
	wxString("	 (denom (cons \'(mtimes)\n") +
	wxString("		      (mapcan #\'(lambda(b e)\n") +
	wxString("				  `(,dsym ,(simplifya `((mexpt) ,b ,e) nil)))\n") +
	wxString("			      vars ords))))\n") +
	wxString("      `((wxtimes)\n") +
	wxString("	((mquotient) ,(simplifya numer nil) ,denom)\n") +
	wxString("	,arg)))\n") +
	wxString("\n") +
	wxString("  (defun wxxml-mcond (x l r)\n") +
	wxString("    (let ((res ()))\n") +
	wxString("      (setq res (wxxml (cadr x) \'(\"<fnm>if</fnm><mspace/>\")\n") +
	wxString("		       \'(\"<mspace/><fnm>then</fnm><mspace/>\") \'mparen \'mparen))\n") +
	wxString("      (setq res (append res (wxxml (caddr x) nil\n") +
	wxString("				   \'(\"<mspace/>\") \'mparen \'mparen)))\n") +
	wxString("      (let ((args (cdddr x)))\n") +
	wxString("	(loop while (>= (length args) 2) do\n") +
	wxString("	      (cond\n") +
	wxString("	       ((and (= (length args) 2) (eql (car args) t))\n") +
	wxString("		(unless (or (eql (cadr args) \'$false) (null (cadr args)))\n") +
	wxString("		  (setq res (wxxml (cadr args)\n") +
	wxString("				   (append res \'(\"<fnm>else</fnm><mspace/>\"))\n") +
	wxString("				   nil \'mparen \'mparen))))\n") +
	wxString("	       (t\n") +
	wxString("		(setq res (wxxml (car args)\n") +
	wxString("				 (append res \'(\"<fnm>elseif</fnm><mspace/>\"))\n") +
	wxString("				 (wxxml (cadr args)\n") +
	wxString("					\'(\"<mspace/><fnm>then</fnm><mspace/>\")\n") +
	wxString("					\'(\"<mspace/>\") \'mparen \'mparen)\n") +
	wxString("				 \'mparen \'mparen))))\n") +
	wxString("	      (setq args (cddr args)))\n") +
	wxString("	(append l res r))))\n") +
	wxString("\n") +
	wxString("  (defprop mdo wxxml-mdo wxxml)\n") +
	wxString("  (defprop mdo 30. wxxml-lbp)\n") +
	wxString("  (defprop mdo 30. wxxml-rbp)\n") +
	wxString("  (defprop mdoin wxxml-mdoin wxxml)\n") +
	wxString("  (defprop mdoin 30. wxxml-rbp)\n") +
	wxString("\n") +
	wxString("  (defun wxxml-lbp (x)\n") +
	wxString("    (cond ((wxxml-get x \'wxxml-lbp))\n") +
	wxString("	  (t(lbp x))))\n") +
	wxString("\n") +
	wxString("  (defun wxxml-rbp (x)\n") +
	wxString("    (cond ((wxxml-get x \'wxxml-rbp))\n") +
	wxString("	  (t(lbp x))))\n") +
	wxString("\n") +
	wxString("  ;; these aren\'t quite right\n") +
	wxString("\n") +
	wxString("  (defun wxxml-mdo (x l r)\n") +
	wxString("    (wxxml-list (wxxmlmdo x) l r \"<mspace/>\"))\n") +
	wxString("\n") +
	wxString("  (defun wxxml-mdoin (x l r)\n") +
	wxString("    (wxxml-list (wxxmlmdoin x) l r \"<mspace/>\"))\n") +
	wxString("\n") +
	wxString("  (defun wxxmlmdo (x)\n") +
	wxString("    (nconc (cond ((second x) (list (make-tag \"for\" \"fnm\") (second x))))\n") +
	wxString("	   (cond ((equal 1 (third x)) nil)\n") +
	wxString("		 ((third x)  (list (make-tag \"from\" \"fnm\") (third x))))\n") +
	wxString("	   (cond ((equal 1 (fourth x)) nil)\n") +
	wxString("		 ((fourth x)\n") +
	wxString("		  (list (make-tag \"step\" \"fnm\")  (fourth x)))\n") +
	wxString("		 ((fifth x)\n") +
	wxString("		  (list (make-tag \"next\" \"fnm\") (fifth x))))\n") +
	wxString("	   (cond ((sixth x)\n") +
	wxString("		  (list (make-tag \"thru\" \"fnm\") (sixth x))))\n") +
	wxString("	   (cond ((null (seventh x)) nil)\n") +
	wxString("		 ((eq \'mnot (caar (seventh x)))\n") +
	wxString("		  (list (make-tag \"while\" \"fnm\") (cadr (seventh x))))\n") +
	wxString("		 (t (list (make-tag \"unless\" \"fnm\") (seventh x))))\n") +
	wxString("	   (list (make-tag \"do\" \"fnm\") (eighth x))))\n") +
	wxString("\n") +
	wxString("  (defun wxxmlmdoin (x)\n") +
	wxString("    (nconc (list (make-tag \"for\" \"fnm\") (second x)\n") +
	wxString("		 (make-tag \"in\" \"fnm\") (third x))\n") +
	wxString("	   (cond ((sixth x)\n") +
	wxString("		  (list (make-tag \"thru\" \"fnm\") (sixth x))))\n") +
	wxString("	   (cond ((null (seventh x)) nil)\n") +
	wxString("		 ((eq \'mnot (caar (seventh x)))\n") +
	wxString("		  (list (make-tag \"while\" \"fnm\") (cadr (seventh x))))\n") +
	wxString("		 (t (list (make-tag \"unless\" \"fnm\") (seventh x))))\n") +
	wxString("	   (list (make-tag \"do\" \"fnm\") (eighth x))))\n") +
	wxString("\n") +
	wxString("\n") +
	wxString("  (defun wxxml-matchfix-np (x l r)\n") +
	wxString("    (setq l (append l (car (wxxmlsym (caar x))))\n") +
	wxString("	  ;; car of wxxmlsym of a matchfix operator is the lead op\n") +
	wxString("	  r (append (cdr (wxxmlsym (caar x))) r)\n") +
	wxString("	  ;; cdr is the trailing op\n") +
	wxString("	  x (wxxml-list (cdr x) nil r \"\"))\n") +
	wxString("    (append l x))\n") +
	wxString("\n") +
	wxString("  (defprop text-string wxxml-matchfix-np wxxml)\n") +
	wxString("  (defprop text-string ((\"<t>\")\"</t>\") wxxmlsym)\n") +
	wxString("\n") +
	wxString("  (defprop mtext wxxml-matchfix-np wxxml)\n") +
	wxString("  (defprop mtext ((\"\")\"\") wxxmlsym)\n") +
	wxString("\n") +
	wxString("  (defvar *wxxml-mratp* nil)\n") +
	wxString("\n") +
	wxString("  (defun wxxml-mlable (x l r)\n") +
	wxString("    (wxxml (caddr x)\n") +
	wxString("	   (append l\n") +
	wxString("		   (if (cadr x)\n") +
	wxString("		       (list\n") +
	wxString("			(format nil \"<lbl>(~A)~A </lbl>\"\n") +
	wxString("				(stripdollar (maybe-invert-string-case (symbol-name (cadr x))))\n") +
	wxString("				*wxxml-mratp*))\n") +
	wxString("		     nil))\n") +
	wxString("	   r \'mparen \'mparen))\n") +
	wxString("\n") +
	wxString("  (defprop mlable wxxml-mlable wxxml)\n") +
	wxString("  (defprop mlabel wxxml-mlable wxxml)\n") +
	wxString("\n") +
	wxString("  (defun wxxml-spaceout (x l r)\n") +
	wxString("    (append l (list \" \" (make-string (cadr x) :initial-element #\\.) \"\") r))\n") +
	wxString("\n") +
	wxString("  (defprop spaceout wxxml-spaceout wxxml)\n") +
	wxString("\n") +
	wxString("  (defun mydispla (x)\n") +
	wxString("    (let ((*print-circle* nil)\n") +
	wxString("	  (*wxxml-mratp* (format nil \"~{~a~}\" (cdr (checkrat x)))))\n") +
	wxString("      (mapc #\'princ\n") +
	wxString("	    (wxxml x \'(\"<mth>\") \'(\"</mth>\") \'mparen \'mparen))))\n") +
	wxString("\n") +
	wxString("  (setf *alt-display2d* \'mydispla)\n") +
	wxString("\n") +
	wxString("  (defun $set_display (tp)\n") +
	wxString("    (cond\n") +
	wxString("     ((eq tp \'$none)\n") +
	wxString("      (setq $display2d nil))\n") +
	wxString("     ((eq tp \'$ascii)\n") +
	wxString("      (setq $display2d t)\n") +
	wxString("      (setf *alt-display2d* nil))\n") +
	wxString("     ((eq tp \'$xml)\n") +
	wxString("      (setq $display2d t)\n") +
	wxString("      (setf *alt-display2d* \'mydispla))\n") +
	wxString("     (t\n") +
	wxString("      (format t \"Unknown display type\")\n") +
	wxString("      (setq tp \'$unknown)))\n") +
	wxString("    tp)\n") +
	wxString("\n") +
	wxString("  ;;\n") +
	wxString("  ;; inference_result from the stats package\n") +
	wxString("  ;;\n") +
	wxString("\n") +
	wxString("  (defun wxxml-inference (x l r)\n") +
	wxString("    (let ((name (cadr x))\n") +
	wxString("	  (values (caddr x))\n") +
	wxString("	  (dis (cadddr x))\n") +
	wxString("	  (m ()))\n") +
	wxString("      (labels\n") +
	wxString("       ((build-eq (e)\n") +
	wxString("		  `((mequal simp) ,(cadr e) ,(caddr e))))\n") +
	wxString("       (dolist (i (cdr dis))\n") +
	wxString("	 (setq m (append m `(((mlist simp) ,(build-eq (nth i values)))))))\n") +
	wxString("       (setq m (cons `((mlist simp) ,name) m))\n") +
	wxString("       (setq m (cons \'($matrix simp inference) m))\n") +
	wxString("       (wxxml m l r \'mparen \'mparen))))\n") +
	wxString("\n") +
	wxString("  (defprop $inference_result wxxml-inference wxxml)\n") +
	wxString("\n") +
	wxString("  (defun wxxml-amatrix (x l r)\n") +
	wxString("    (let* ((nr ($@-function x \'$nr))\n") +
	wxString("	   (nc ($@-function x \'$nc))\n") +
	wxString("	   (M (simplifya ($genmatrix\n") +
	wxString("			  `((lambda) ((mlist) i j) (mfuncall \'$get_element ,x i j))\n") +
	wxString("			  nr nc)\n") +
	wxString("			 t)))\n") +
	wxString("      (wxxml-matrix M l r)))\n") +
	wxString("\n") +
	wxString("  (defprop $amatrix wxxml-amatrix wxxml)\n") +
	wxString("\n") +
	wxString("  ;;\n") +
	wxString("  ;; orthopoly functions\n") +
	wxString("  ;;\n") +
	wxString("\n") +
	wxString("  (defun wxxml-pochhammer (x l r)\n") +
	wxString("    (let ((n (cadr x))\n") +
	wxString("	  (k (caddr x)))\n") +
	wxString("      (append l\n") +
	wxString("	      (list (format nil \"<i altCopy=\\\"~{~a~}\\\"><p>\" (mstring x)))\n") +
	wxString("	      (wxxml n nil nil \'mparen \'mparen)\n") +
	wxString("	      (list \"</p><r>\")\n") +
	wxString("	      (wxxml k nil nil \'mparen \'mparen)\n") +
	wxString("	      (list \"</r></i>\")\n") +
	wxString("	      r)))\n") +
	wxString("\n") +
	wxString("  (defprop $pochhammer wxxml-pochhammer wxxml)\n") +
	wxString("\n") +
	wxString("  (defun wxxml-orthopoly (x l r)\n") +
	wxString("    (let* ((fun-name (caar x))\n") +
	wxString("	   (disp-name (get fun-name \'wxxml-orthopoly-disp))\n") +
	wxString("	   (args (cdr x)))\n") +
	wxString("      (append l\n") +
	wxString("	      (list (format nil \"<fn altCopy=\\\"~{~a~}\\\">\" (mstring x)))\n") +
	wxString("	      (if (nth 2 disp-name)\n") +
	wxString("		  (list (format nil \"<ie><fnm>~a</fnm><r>\" (car disp-name)))\n") +
	wxString("		(list (format nil \"<i><fnm>~a</fnm><r>\" (car disp-name))))\n") +
	wxString("	      (wxxml (nth (nth 1 disp-name) args) nil nil \'mparen \'mparen)\n") +
	wxString("	      (when (nth 2 disp-name)\n") +
	wxString("		(append (list \"</r><r>\")\n") +
	wxString("			(when (nth 3 disp-name) (list \"<p>\"))\n") +
	wxString("			(wxxml-list (or (nth 5 disp-name)\n") +
	wxString("					(mapcar (lambda (i) (nth i args)) (nth 2 disp-name)))\n") +
	wxString("				    nil nil \",\")\n") +
	wxString("			(when (nth 3 disp-name) (list \"</p>\"))\n") +
	wxString("			(list \"</r>\")))\n") +
	wxString("	      (if (nth 2 disp-name)\n") +
	wxString("		  (list \"</ie>\")\n") +
	wxString("		(list \"</r></i>\"))\n") +
	wxString("	      (list \"<p>\")\n") +
	wxString("	      (wxxml-list (mapcar (lambda (i) (nth i args)) (nth 4 disp-name)) nil nil \",\")\n") +
	wxString("	      (list \"</p></fn>\")\n") +
	wxString("	      r)))\n") +
	wxString("\n") +
	wxString("  (dolist (ortho-pair\n") +
	wxString("	   \'(($laguerre \"L\" 0 nil nil (1))\n") +
	wxString("	     (%laguerre \"L\" 0 nil nil (1))\n") +
	wxString("	     ($legendre_p \"P\" 0 nil nil (1))\n") +
	wxString("	     (%legendre_p \"P\" 0 nil nil (1))\n") +
	wxString("	     ($legendre_q \"Q\" 0 nil nil (1))\n") +
	wxString("	     (%legendre_q \"Q\" 0 nil nil (1))\n") +
	wxString("	     ($chebyshev_t \"T\" 0 nil nil (1))\n") +
	wxString("	     (%chebyshev_t \"T\" 0 nil nil (1))\n") +
	wxString("	     ($chebyshev_u \"U\" 0 nil nil (1))\n") +
	wxString("	     (%chebyshev_u \"U\" 0 nil nil (1))\n") +
	wxString("	     ($hermite \"H\" 0 nil nil (1))\n") +
	wxString("	     (%hermite \"H\" 0 nil nil (1))\n") +
	wxString("	     ($spherical_bessel_j \"J\" 0 nil nil (1))\n") +
	wxString("	     (%spherical_bessel_j \"J\" 0 nil nil (1))\n") +
	wxString("	     ($spherical_bessel_y \"Y\" 0 nil nil (1))\n") +
	wxString("	     (%spherical_bessel_y \"Y\" 0 nil nil (1))\n") +
	wxString("	     ($assoc_legendre_p \"P\" 0 (1) nil (2))\n") +
	wxString("	     (%assoc_legendre_p \"P\" 0 (1) nil (2))\n") +
	wxString("	     ($assoc_legendre_q \"Q\" 0 (1) nil (2))\n") +
	wxString("	     (%assoc_legendre_q \"Q\" 0 (1) nil (2))\n") +
	wxString("	     ($jacobi_p \"P\" 0 (1 2) t (3))\n") +
	wxString("	     (%jacobi_p \"P\" 0 (1 2) t (3))\n") +
	wxString("	     ($gen_laguerre \"L\" 0 (1) t (2))\n") +
	wxString("	     (%gen_laguerre \"L\" 0 (1) t (2))\n") +
	wxString("	     ($spherical_harmonic \"Y\" 0 (1) nil (2 3))\n") +
	wxString("	     (%spherical_harmonic \"Y\" 0 (1) nil (2 3))\n") +
	wxString("	     ($ultraspherical \"C\" 0 (1) t (2))\n") +
	wxString("	     (%ultraspherical \"C\" 0 (1) t (2))\n") +
	wxString("	     ($spherical_hankel1 \"H\" 0 t t (1) (1))\n") +
	wxString("	     (%spherical_hankel1 \"H\" 0 t t (1) (1))\n") +
	wxString("	     ($spherical_hankel2 \"H\" 0 t t (1) (2))\n") +
	wxString("	     (%spherical_hankel2 \"H\" 0 t t (1) (2))))\n") +
	wxString("    (setf (get (car ortho-pair) \'wxxml) \'wxxml-orthopoly)\n") +
	wxString("    (setf (get (car ortho-pair) \'wxxml-orthopoly-disp) (cdr ortho-pair)))\n") +
	wxString("\n") +
	wxString(";;;\n") +
	wxString(";;; This is the display support only - copy/paste will not work\n") +
	wxString(";;;\n") +
	wxString("\n") +
	wxString("  (defmvar $pdiff_uses_prime_for_derivatives nil)\n") +
	wxString("  (defmvar $pdiff_prime_limit 3)\n") +
	wxString("  (defmvar $pdiff_uses_named_subscripts_for_derivatives nil)\n") +
	wxString("  (defmvar $pdiff_diff_var_names (list \'(mlist) \'|$x| \'|$y| \'|$z|))\n") +
	wxString("\n") +
	wxString("  (setf (get \'%pderivop \'wxxml) \'wxxml-pderivop)\n") +
	wxString("  (setf (get \'$pderivop \'wxxml) \'wxxml-pderivop)\n") +
	wxString("\n") +
	wxString("  (defun wxxml-pderivop (x l r)\n") +
	wxString("    (cond ((and $pdiff_uses_prime_for_derivatives (eq 3 (length x)))\n") +
	wxString("	   (let* ((n (car (last x)))\n") +
	wxString("		  (p))\n") +
	wxString("\n") +
	wxString("	     (cond ((<= n $pdiff_prime_limit)\n") +
	wxString("		    (setq p (make-list n :initial-element \"\'\")))\n") +
	wxString("		   (t\n") +
	wxString("		    (setq p (list \"(\" n \")\"))))\n") +
	wxString("	     (append (append l \'(\"<r>\"))\n") +
	wxString("		     (let ((*var-tag* (list \"<fnm>\" \"</fnm>\"))) (wxxml (cadr x) nil nil lop rop))\n") +
	wxString("		     p\n") +
	wxString("		     (list \"</r>\")  r)))\n") +
	wxString("\n") +
	wxString("	  ((and $pdiff_uses_named_subscripts_for_derivatives\n") +
	wxString("		(< (apply #\'+ (cddr x)) $pdiff_prime_limit))\n") +
	wxString("	   (let ((n (cddr x))\n") +
	wxString("		 (v (mapcar #\'stripdollar (cdr $pdiff_diff_var_names)))\n") +
	wxString("		 (p))\n") +
	wxString("	     (cond ((> (length n) (length v))\n") +
	wxString("		    (merror \"Not enough elements in pdiff_diff_var_names to display the expression\")))\n") +
	wxString("	     (dotimes (i (length n))\n") +
	wxString("	       (setq p (append p (make-list (nth i n)\n") +
	wxString("					    :initial-element (nth i v)))))\n") +
	wxString("	     (append (append l \'(\"<i><r>\"))\n") +
	wxString("		     (wxxml (cadr x) nil nil lop rop)\n") +
	wxString("		     (list \"</r><r>\") p (list \"</r></i>\") r)))\n") +
	wxString("	  (t\n") +
	wxString("	   (append (append l \'(\"<i><r>\"))\n") +
	wxString("		   (wxxml (cadr x) nil nil lop rop)\n") +
	wxString("		   (list \"</r><r>(\")\n") +
	wxString("		   (wxxml-list (cddr x) nil nil \",\")\n") +
	wxString("		   (list \")</r></i>\") r))))\n") +
	wxString("\n") +
	wxString("  ;;\n") +
	wxString("  ;; Plotting support\n") +
	wxString("  ;;\n") +
	wxString("\n") +
	wxString("  (defprop wxxmltag wxxml-tag wxxml)\n") +
	wxString("\n") +
	wxString("  (defun wxxml-tag (x l r)\n") +
	wxString("    (let ((name (cadr x))\n") +
	wxString("	  (tag (caddr x))\n") +
	wxString("	  (prop (cadddr x)))\n") +
	wxString("      (if prop\n") +
	wxString("	  (append l (list (format nil \"<~a ~a>~a</~a>\" tag prop name tag)) r)\n") +
	wxString("	(append l (list (format nil \"<~a>~a</~a>\" tag name tag)) r))))\n") +
	wxString("\n") +
	wxString("\n") +
	wxString("  (defvar *image-counter* 0)\n") +
	wxString("\n") +
	wxString("  ;; A suitable name for a .gnuplot file\n") +
	wxString("  (defun wxplot-gnuplotfilename ()\n") +
	wxString("    (incf *wx-plot-num*)\n") +
	wxString("    (format nil \"maxout_~d_~d.gnuplot\" (getpid) *wx-plot-num*))\n") +
	wxString("\n") +
	wxString("  ;; A suitable name for a .data file\n") +
	wxString("  (defun wxplot-datafilename ()\n") +
	wxString("    (incf *wx-plot-num*)\n") +
	wxString("    (format nil \"maxout_~d_~d.data\" (getpid) *wx-plot-num*))\n") +
	wxString("\n") +
	wxString("  (defun wxplot-filename (&optional (suff t))\n") +
	wxString("    (incf *image-counter*)\n") +
	wxString("    (plot-temp-file (if suff\n") +
	wxString("			(format nil \"maxout_~d_~d.png\" (getpid) *image-counter*)\n") +
	wxString("		      (format nil \"maxout_~d_~d\" (getpid) *image-counter*))))\n") +
	wxString("\n") +
	wxString("  ;; The \"solid\" has to be changed to \"dashed\" as soon as plot() starts\n") +
	wxString("  ;; supporting other line styles than \"solid\" or \"dots\".\n") +
	wxString("  (defun $wxplot_preamble ()\n") +
	wxString("    (let ((frmt\n") +
	wxString("	   (cond\n") +
	wxString("	    ($wxplot_old_gnuplot \"set terminal png picsize ~d ~d; set zeroaxis;\")\n") +
	wxString("	    ($wxplot_pngcairo \"set terminal pngcairo solid background \\\"white\\\" enhanced font \\\"arial,10\\\" fontscale 1.0 size ~d,~d; set zeroaxis;\")\n") +
	wxString("	    (t \"set terminal png size ~d,~d; set zeroaxis;\"))))\n") +
	wxString("      (format nil frmt\n") +
	wxString("	      ($first $wxplot_size)\n") +
	wxString("	      ($second $wxplot_size))))\n") +
	wxString("\n") +
	wxString("  (defun $int_range (lo &optional hi (st 1))\n") +
	wxString("    (unless (integerp lo)\n") +
	wxString("      ($error \"int_range: first argument is not an integer.\"))\n") +
	wxString("    (unless (or (null hi) (integerp hi))\n") +
	wxString("      ($error \"int_range: second argument is not an integer.\"))\n") +
	wxString("    (when (null hi)\n") +
	wxString("      (setq hi lo)\n") +
	wxString("      (setq lo 1))\n") +
	wxString("    (cons \'(mlist simp) (loop :for i :from lo :to hi :by st :collect i)))\n") +
	wxString("\n") +
	wxString("  (defvar *default-framerate* 2)\n") +
	wxString("  (defvar $wxanimate_framerate *default-framerate*)\n") +
	wxString("  (defun slide-tag (images)\n") +
	wxString("    (if (eql *default-framerate* $wxanimate_framerate)\n") +
	wxString("	($ldisp (list \'(wxxmltag simp) (format nil \"~{~a;~}\" images) \"slide\" (if (eql $wxanimate_autoplay \'t) \"running=\\\"true\\\" del=\\\"true\\\"\" \"running=\\\"false\\\" del=\\\"true\\\"\")))\n") +
	wxString("      ($ldisp (list \'(wxxmltag simp) (format nil \"~{~a;~}\" images) \"slide\" (if (eql $wxanimate_autoplay \'t) (format nil \"fr=\\\"~a\\\" running=\\\"true\\\" del=\\\"true\\\"\" $wxanimate_framerate)  (format nil \"fr=\\\"~a\\\" running=\\\"false\\\" del=\\\"true\\\"\" $wxanimate_framerate))\n") +
	wxString("                    ))))\n") +
	wxString("\n") +
	wxString("  (defun wxanimate (scene)\n") +
	wxString("    (let* ((scene (cdr scene))\n") +
	wxString("	   (a (car scene))\n") +
	wxString("	   (a-range (meval (cadr scene)))\n") +
	wxString("	   (expr (caddr scene))\n") +
	wxString("	   (args (cdddr scene))\n") +
	wxString("	   (frameno 1)\n") +
	wxString("	   (images ()))\n") +
	wxString("      (when (integerp a-range)\n") +
	wxString("	(setq a-range (cons \'(mlist simp) (loop for i from 1 to a-range collect i))))\n") +
	wxString("      (dolist (aval (reverse (cdr a-range)))\n") +
	wxString("	($wxstatusbar (format nil \"Preparing Frame #~d\" frameno))\n") +
	wxString("	(setf frameno (+ 1 frameno))\n") +
	wxString("	(let ((preamble ($wxplot_preamble))\n") +
	wxString("	      (system-preamble (get-plot-option-string \'$gnuplot_preamble 2))\n") +
	wxString("	      (filename (wxplot-filename))\n") +
	wxString("	      (expr (maxima-substitute aval a expr)))\n") +
	wxString("	  (when (string= system-preamble \"false\")\n") +
	wxString("	    (setq system-preamble \"\"))\n") +
	wxString("	  (setq preamble (format nil \"~a; ~a\" preamble system-preamble))\n") +
	wxString("	  (dolist (arg args)\n") +
	wxString("	    (if (and (listp arg) (eql (cadr arg) \'$gnuplot_preamble))\n") +
	wxString("		(setq preamble (format nil \"~a; ~a\"\n") +
	wxString("				       preamble (meval (maxima-substitute aval a (caddr arg)))))))\n") +
	wxString("	  (apply #\'$plot2d `(,(meval expr) ,@(mapcar #\'meval args)\n") +
	wxString("			     ((mlist simp) $plot_format $gnuplot)\n") +
	wxString("			     ((mlist simp) $gnuplot_term ,(if $wxplot_pngcairo \'$pngcairo \'$png))\n") +
	wxString("			     ((mlist simp) $gnuplot_preamble ,preamble)\n") +
	wxString("			     ((mlist simp) $gnuplot_out_file ,filename)))\n") +
	wxString("	  (setq images (cons filename images))))\n") +
	wxString("      (when images\n") +
	wxString("	(slide-tag images)))\n") +
	wxString("    \"\")\n") +
	wxString("\n") +
	wxString("  (defmspec $with_slider (scene)\n") +
	wxString("    (wxanimate scene))\n") +
	wxString("\n") +
	wxString("  (defmspec $wxanimate (scene)\n") +
	wxString("    (wxanimate scene))\n") +
	wxString("\n") +
	wxString("  (defvar *windows-OS* (string= *autoconf-win32* \"true\"))\n") +
	wxString("\n") +
	wxString("  (defun get-file-name-opt (scene)\n") +
	wxString("    (let (opts filename)\n") +
	wxString("      (loop for opt in scene do\n") +
	wxString("	    (if (and (not (atom opt))\n") +
	wxString("		     (eq (caar opt) \'mequal)\n") +
	wxString("		     (eq (cadr opt) \'$file_name))\n") +
	wxString("		(setq filename (caddr opt))\n") +
	wxString("	      (setq opts (cons opt opts))))\n") +
	wxString("      (values (reverse opts) filename)))\n") +
	wxString("\n") +
	wxString("  (defun get-pic-size-opt ()\n") +
	wxString("    (cond\n") +
	wxString("     ((eq ($get \'$draw \'$version) 1)\n") +
	wxString("      `(((mequal simp) $pic_width ,($first $wxplot_size))\n") +
	wxString("	((mequal simp) $pic_height ,($second $wxplot_size))))\n") +
	wxString("     (t\n") +
	wxString("      `(((mequal simp) $dimensions ,$wxplot_size)))))\n") +
	wxString("\n") +
	wxString("  (defun wxanimate-draw (scenes scene-head)\n") +
	wxString("    (unless ($get \'$draw \'$version) ($load \"draw\"))\n") +
	wxString("    (multiple-value-bind (scene file-name) (get-file-name-opt (cdr scenes))\n") +
	wxString("			 (let* ((a (meval (car scene)))\n") +
	wxString("				(a-range (meval (cadr scene)))\n") +
	wxString("				(*windows-OS* t)\n") +
	wxString("				(args (cddr scene))\n") +
	wxString("				(frameno 1)\n") +
	wxString("				(images ()))\n") +
	wxString("			   (when (integerp a-range)\n") +
	wxString("			     (setq a-range (cons \'(mlist simp) (loop for i from 1 to a-range collect i))))\n") +
	wxString("			   (if file-name\n") +
	wxString("			       ;; If file_name is set, draw the animation into gif using gnuplot\n") +
	wxString("			       (let (imgs)\n") +
	wxString("				 (dolist (aval (reverse (cdr a-range)))\n") +
	wxString("				   (setq imgs (cons\n") +
	wxString("					       (cons scene-head\n") +
	wxString("						     (mapcar #\'(lambda (arg) (meval (maxima-substitute aval a arg)))\n") +
	wxString("							     args))\n") +
	wxString("					       imgs)))\n") +
	wxString("				 ($apply \'$draw\n") +
	wxString("					 (append\n") +
	wxString("					  `((mlist simp)\n") +
	wxString("					    ((mequal simp) $terminal $animated_gif)\n") +
	wxString("					    ((mequal simp) $file_name ,file-name))\n") +
	wxString("					  (get-pic-size-opt)\n") +
	wxString("					  imgs))\n") +
	wxString("				 \"\")\n") +
	wxString("			     ;; If file_name is not set, show the animation in wxMaxima\n") +
	wxString("			     (progn\n") +
	wxString("			       (dolist (aval (reverse (cdr a-range)))\n") +
	wxString("				 ($wxstatusbar (format nil \"Preparing Frame #~d\" frameno))\n") +
	wxString("				 (setf frameno (+ 1 frameno))\n") +
	wxString("				 (let* ((filename (wxplot-filename nil))\n") +
	wxString("					(args (cons scene-head\n") +
	wxString("						    (mapcar #\'(lambda (arg) (meval (maxima-substitute aval a arg)))\n") +
	wxString("							    args))))\n") +
	wxString("				   (setq images (cons (format nil \"~a.png\" filename) images))\n") +
	wxString("				   ($apply \'$draw\n") +
	wxString("					   (append\n") +
	wxString("					    `((mlist simp)\n") +
	wxString("					      ((mequal simp) $terminal ,(if $wxplot_pngcairo \'$pngcairo \'$png))\n") +
	wxString("					      ((mequal simp) $file_name ,filename))\n") +
	wxString("					    (get-pic-size-opt)\n") +
	wxString("					    (list args)))))\n") +
	wxString("			       (when images\n") +
	wxString("				 (slide-tag images))))\n") +
	wxString("			   \"\")))\n") +
	wxString("\n") +
	wxString("  (defmspec $wxanimate_draw (scene)\n") +
	wxString("    (wxanimate-draw scene \'($gr2d)))\n") +
	wxString("\n") +
	wxString("  (defmspec $with_slider_draw (scene)\n") +
	wxString("    (wxanimate-draw scene \'($gr2d)))\n") +
	wxString("\n") +
	wxString("  (defmspec $with_slider_draw2d (scene)\n") +
	wxString("    (wxanimate-draw scene \'($gr2d)))\n") +
	wxString("\n") +
	wxString("  (defmspec $with_slider_draw3d (scene)\n") +
	wxString("    (wxanimate-draw scene \'($gr3d)))\n") +
	wxString("\n") +
	wxString("  (defmspec $wxanimate_draw3d (scene)\n") +
	wxString("    (wxanimate-draw scene \'($gr3d)))\n") +
	wxString("\n") +
	wxString("  (defun $wxplot2d (&rest args)\n") +
	wxString("    (let ((preamble ($wxplot_preamble))\n") +
	wxString("	  (system-preamble (get-plot-option-string \'$gnuplot_preamble 2))\n") +
	wxString("	  (filename (wxplot-filename)))\n") +
	wxString("      (when (string= system-preamble \"false\")\n") +
	wxString("	(setq system-preamble \"\"))\n") +
	wxString("      (setq preamble (format nil \"~a; ~a\" preamble system-preamble))\n") +
	wxString("      (dolist (arg args)\n") +
	wxString("	(if (and (listp arg) (eql (cadr arg) \'$gnuplot_preamble))\n") +
	wxString("	    (setq preamble (format nil \"~a; ~a\" preamble (caddr arg)))))\n") +
	wxString("      (apply #\'$plot2d `(,@args\n") +
	wxString("			 ((mlist simp) $plot_format $gnuplot)\n") +
	wxString("			 ((mlist simp) $gnuplot_term ,(if $wxplot_pngcairo \'$pngcairo \'$png))\n") +
	wxString("			 ((mlist simp) $gnuplot_preamble ,preamble)\n") +
	wxString("			 ((mlist simp) $gnuplot_out_file ,filename)))\n") +
	wxString("      ($ldisp `((wxxmltag simp) ,filename \"img\")))\n") +
	wxString("    \"\")\n") +
	wxString("\n") +
	wxString("  (defun $wxplot3d (&rest args)\n") +
	wxString("    (let ((preamble ($wxplot_preamble))\n") +
	wxString("	  (system-preamble (get-plot-option-string \'$gnuplot_preamble 2))\n") +
	wxString("	  (filename (wxplot-filename)))\n") +
	wxString("      (when (string= system-preamble \"false\")\n") +
	wxString("	(setq system-preamble \"\"))\n") +
	wxString("      (setq preamble (format nil \"~a; ~a\" preamble system-preamble))\n") +
	wxString("      (dolist (arg args)\n") +
	wxString("	(if (and (listp arg) (eql (cadr arg) \'$gnuplot_preamble))\n") +
	wxString("	    (setq preamble (format nil \"~a; ~a\"\n") +
	wxString("				   preamble (caddr arg)))))\n") +
	wxString("      (apply #\'$plot3d `(,@args\n") +
	wxString("			 ((mlist simp) $plot_format $gnuplot)\n") +
	wxString("			 ((mlist simp) $gnuplot_term ,(if $wxplot_pngcairo \'$pngcairo \'$png))\n") +
	wxString("			 ((mlist simp) $gnuplot_preamble ,preamble)\n") +
	wxString("			 ((mlist simp) $gnuplot_out_file ,filename)))\n") +
	wxString("      ($ldisp `((wxxmltag simp) ,filename \"img\")))\n") +
	wxString("    \"\")\n") +
	wxString("\n") +
	wxString("\n") +
	wxString("  (defun $wxdraw2d (&rest args)\n") +
	wxString("    (apply #\'$wxdraw\n") +
	wxString("	   (list (cons \'($gr2d) args))))\n") +
	wxString("\n") +
	wxString("  (defun $wxdraw3d (&rest args)\n") +
	wxString("    (apply #\'$wxdraw\n") +
	wxString("	   (list (cons \'($gr3d) args))))\n") +
	wxString("\n") +
	wxString("  (defvar $display_graphics t)\n") +
	wxString("\n") +
	wxString("  (defun option-sublist (lst)\n") +
	wxString("    (cons \'(mlist simp)\n") +
	wxString("	  (loop for l in lst\n") +
	wxString("		when (and (listp l) (= ($length l) 2))\n") +
	wxString("		collect l)))\n") +
	wxString("\n") +
	wxString("  (defun $wxdraw (&rest args)\n") +
	wxString("    (unless ($get \'$draw \'$version) ($load \"draw\"))\n") +
	wxString("    (let* ((file_name_spec ($assoc \'$file_name\n") +
	wxString("				   (option-sublist (append (cdar args)\n") +
	wxString("							   (cdr args)))))\n") +
	wxString("	   (gnuplotfilename (wxplot-gnuplotfilename))\n") +
	wxString("	   (datafilename (wxplot-datafilename))\n") +
	wxString("	   (filename (or file_name_spec (wxplot-filename nil)))\n") +
	wxString("	   (*windows-OS* t)\n") +
	wxString("	   res)\n") +
	wxString("      (setq res ($apply \'$draw\n") +
	wxString("			(append\n") +
	wxString("			 \'((mlist simp))\n") +
	wxString("			 args\n") +
	wxString("			 `(((mequal simp) $terminal ,(if $wxplot_pngcairo \'$pngcairo \'$png))\n") +
	wxString("			   ((mequal simp) $gnuplot_file_name ,gnuplotfilename)\n") +
	wxString("			   ((mequal simp) $data_file_name ,datafilename)\n") +
	wxString("			   ((mequal simp) $file_name ,filename))\n") +
	wxString("			 (cond\n") +
	wxString("			  ((eq ($get \'$draw \'$version) 1)\n") +
	wxString("			   `(((mequal simp) $pic_width ,($first $wxplot_size))\n") +
	wxString("			     ((mequal simp) $pic_height ,($second $wxplot_size))))\n") +
	wxString("			  (t\n") +
	wxString("			   `(((mequal simp) $dimensions ,$wxplot_size)))))))\n") +
	wxString("      (if $display_graphics\n") +
	wxString("	  (progn\n") +
	wxString("	    ($ldisp `((wxxmltag simp) ,(format nil \"~a.png\" filename) \"img\"\n") +
	wxString("		      ,(if file_name_spec\n") +
	wxString("			   (format nil \"del=\\\"no\\\" gnuplotsource=\\\"~a/~a\\\" gnuplotdata=\\\"~a/~a\\\"\" $maxima_tempdir gnuplotfilename $maxima_tempdir datafilename)\n") +
	wxString("			 (format nil \"del=\\\"yes\\\" gnuplotsource=\\\"~a/~a\\\" gnuplotdata=\\\"~a/~a\\\"\" $maxima_tempdir gnuplotfilename $maxima_tempdir datafilename)\n") +
	wxString("			 )\n") +
	wxString("		      ))\n") +
	wxString("	    (setq res \"\"))\n") +
	wxString("	(setf res `((wxxmltag simp) ,(format nil \"~a.png\" filename) \"img\")))\n") +
	wxString("      res))\n") +
	wxString("\n") +
	wxString("  (defmspec $wxdraw_list (args)\n") +
	wxString("    (unless ($get \'$draw \'$version) ($load \"draw\"))\n") +
	wxString("    (let (($display_graphics nil))\n") +
	wxString("      ($ldisp (cons \'(mlist simp) (mapcar #\'meval (cdr args)))))\n") +
	wxString("    \'$done)\n") +
	wxString("\n") +
	wxString("  (defun $wximplicit_plot (&rest args)\n") +
	wxString("    (let ((preamble ($wxplot_preamble))\n") +
	wxString("	  (system-preamble (get-plot-option-string \'$gnuplot_preamble 2))\n") +
	wxString("	  (filename (wxplot-filename)))\n") +
	wxString("      (when (string= system-preamble \"false\")\n") +
	wxString("	(setq system-preamble \"\"))\n") +
	wxString("      (setq preamble (format nil \"~a; ~a\" preamble system-preamble))\n") +
	wxString("      (dolist (arg args)\n") +
	wxString("	(if (and (listp arg) (eql (cadr arg) \'$gnuplot_preamble))\n") +
	wxString("	    (setq preamble (format nil \"~a; ~a\"\n") +
	wxString("				   preamble (caddr arg)))))\n") +
	wxString("      ($apply \'$implicit_plot `((mlist simp) ,@args\n") +
	wxString("				((mlist simp) $plot_format $gnuplot)\n") +
	wxString("				((mlist simp) $gnuplot_term ,(if $wxplot_pngcairo \'$pngcairo \'$png))\n") +
	wxString("				((mlist simp) $gnuplot_preamble ,preamble)\n") +
	wxString("				((mlist simp) $gnuplot_out_file ,filename)))\n") +
	wxString("      ($ldisp `((wxxmltag simp) ,filename \"img\")))\n") +
	wxString("    \"\")\n") +
	wxString("\n") +
	wxString("\n") +
	wxString("  (defun $wxcontour_plot (&rest args)\n") +
	wxString("    (let ((preamble ($wxplot_preamble))\n") +
	wxString("	  ($plot_options $plot_options)\n") +
	wxString("	  (system-preamble (get-plot-option-string \'$gnuplot_preamble 2))\n") +
	wxString("	  (filename (wxplot-filename)))\n") +
	wxString("      (when (string= system-preamble \"false\")\n") +
	wxString("	(setq system-preamble \"\"))\n") +
	wxString("      (setq preamble (format nil \"~a; ~a\" preamble system-preamble))\n") +
	wxString("      (dolist (arg args)\n") +
	wxString("	(if (and (listp arg) (eql (cadr arg) \'$gnuplot_preamble))\n") +
	wxString("	    (setq preamble (format nil \"~a; ~a\" preamble (caddr arg)))))\n") +
	wxString("      (apply #\'$contour_plot `(,@args\n") +
	wxString("			       ((mlist simp) $gnuplot_term ,(if $wxplot_pngcairo \'$pngcairo \'$png))\n") +
	wxString("			       ((mlist simp) $plot_format $gnuplot)\n") +
	wxString("			       ((mlist simp) $gnuplot_preamble ,preamble)\n") +
	wxString("			       ((mlist simp) $gnuplot_out_file ,filename)))\n") +
	wxString("\n") +
	wxString("      ($ldisp `((wxxmltag simp) ,filename \"img\")))\n") +
	wxString("    \"\")\n") +
	wxString("\n") +
	wxString("  (defun $show_image (file)\n") +
	wxString("    ($ldisp `((wxxmltag simp) ,file \"img\" \"del=\\\"no\\\"\")))\n") +
	wxString("\n") +
	wxString("  ;;\n") +
	wxString("  ;; Port of Barton Willis\'s texput function.\n") +
	wxString("  ;;\n") +
	wxString("\n") +
	wxString("  (defun $wxxmlput (e s &optional tx lbp rbp)\n") +
	wxString("\n") +
	wxString("    (when (stringp e)\n") +
	wxString("      (setf e (define-symbol e)))\n") +
	wxString("\n") +
	wxString("    (cond (($listp s)\n") +
	wxString("	   (setq s (margs s)))\n") +
	wxString("	  ((stringp s)\n") +
	wxString("	   (setq s (list s)))\n") +
	wxString("	  ((atom s)\n") +
	wxString("	   (setq s (list (wxxml-stripdollar ($sconcat s))))))\n") +
	wxString("\n") +
	wxString("    (when (or (null lbp) (not (integerp lbp)))\n") +
	wxString("      (setq lbp 180))\n") +
	wxString("    (when (or (null rbp) (not (integerp rbp)))\n") +
	wxString("      (setq rbp 180))\n") +
	wxString("    (cond ((null tx)\n") +
	wxString("	   (if (stringp (nth 0 s))\n") +
	wxString("	       (putprop e (nth 0 s) \'wxxmlword)\n") +
	wxString("             (let ((fun-name (gensym))\n") +
	wxString("                   (fun-body\n") +
	wxString("                    `(append l\n") +
	wxString("                             (list\n") +
	wxString("                              (let ((f-x (mfuncall \',s x)))\n") +
	wxString("                                (if (stringp f-x)\n") +
	wxString("                                    f-x\n") +
	wxString("				  (merror \"wxxml: function ~s did not return a string.~%\"\n") +
	wxString("					  ($sconcat \',(nth 0 s))))))\n") +
	wxString("                             r)))\n") +
	wxString("               (setf (symbol-function fun-name) (coerce `(lambda (x l r) ,fun-body) \'function))\n") +
	wxString("               (setf (get e \'wxxml) fun-name))))\n") +
	wxString("	  ((eq tx \'$matchfix)\n") +
	wxString("	   (putprop e \'wxxml-matchfix \'wxxml)\n") +
	wxString("	   (cond ((< (length s) 2)\n") +
	wxString("		  (merror\n") +
	wxString("		   \"Improper 2nd argument to `wxxmlput\' for matchfix operator.\"))\n") +
	wxString("		 ((eq (length s) 2)\n") +
	wxString("		  (putprop e (list (list (nth 0 s)) (nth 1 s)) \'wxxmlsym))\n") +
	wxString("		 (t\n") +
	wxString("		  (putprop\n") +
	wxString("		   e (list (list (nth 0 s)) (nth 1 s) (nth 2 s)) \'wxxmlsym))))\n") +
	wxString("	  ((eq tx \'$prefix)\n") +
	wxString("	   (putprop e \'wxxml-prefix \'wxxml)\n") +
	wxString("	   (putprop e s \'wxxmlsym)\n") +
	wxString("	   (putprop e lbp \'wxxml-lbp)\n") +
	wxString("	   (putprop e rbp \'wxxml-rbp))\n") +
	wxString("	  ((eq tx \'$infix)\n") +
	wxString("	   (putprop e \'wxxml-infix \'wxxml)\n") +
	wxString("	   (putprop e  s \'wxxmlsym)\n") +
	wxString("	   (putprop e lbp \'wxxml-lbp)\n") +
	wxString("	   (putprop e rbp \'wxxml-rbp))\n") +
	wxString("	  ((eq tx \'$postfix)\n") +
	wxString("	   (putprop e \'wxxml-postfix \'wxxml)\n") +
	wxString("	   (putprop e  s \'wxxmlsym)\n") +
	wxString("	   (putprop e lbp \'wxxml-lbp))\n") +
	wxString("	  (t (merror \"Improper arguments to `wxxmlput\'.\"))))\n") +
	wxString("\n") +
	wxString(";;;;;;;;;;;;;\n") +
	wxString("  ;; Auto-loaded functions\n") +
	wxString(";;;;\n") +
	wxString("\n") +
	wxString("  (setf (get \'$lbfgs \'autoload) \"lbfgs\")\n") +
	wxString("  (setf (get \'$lcm \'autoload) \"functs\")\n") +
	wxString("\n") +
	wxString(";;;;;;;;;;;;;\n") +
	wxString("  ;; Statistics functions\n") +
	wxString(";;;;\n") +
	wxString("\n") +
	wxString("  (defvar $draw_compound t)\n") +
	wxString("\n") +
	wxString("  (defmacro create-statistics-wrapper (fun wxfun)\n") +
	wxString("    `(defun ,wxfun (&rest args)\n") +
	wxString("       (let (($draw_compound nil) res)\n") +
	wxString("	 (declare (special $draw_compound))\n") +
	wxString("	 (setq res ($apply \',fun (cons \'(mlist simp) args)))\n") +
	wxString("	 ($apply \'$wxdraw2d res))))\n") +
	wxString("\n") +
	wxString("  (create-statistics-wrapper $histogram $wxhistogram)\n") +
	wxString("  (create-statistics-wrapper $scatterplot $wxscatterplot)\n") +
	wxString("  (create-statistics-wrapper $barsplot $wxbarsplot)\n") +
	wxString("  (create-statistics-wrapper $piechart $wxpiechart)\n") +
	wxString("  (create-statistics-wrapper $boxplot $wxboxplot)\n") +
	wxString("\n") +
	wxString("  (dolist (fun \'($histogram\n") +
	wxString("		 $scatterplot\n") +
	wxString("		 $barsplot\n") +
	wxString("		 $piechart\n") +
	wxString("		 $boxplot))\n") +
	wxString("    (setf (get fun \'autoload) \"descriptive\"))\n") +
	wxString("\n") +
	wxString("  (dolist (fun \'($mean\n") +
	wxString("		 $median\n") +
	wxString("		 $var\n") +
	wxString("		 $std\n") +
	wxString("		 $test_mean\n") +
	wxString("		 $test_means_difference\n") +
	wxString("		 $test_normality\n") +
	wxString("		 $simple_linear_regression\n") +
	wxString("		 $subsample))\n") +
	wxString("    (setf (get fun \'autoload) \"stats\"))\n") +
	wxString("\n") +
	wxString("  (setf (get \'$lsquares_estimates \'autoload) \"lsquares\")\n") +
	wxString("\n") +
	wxString("  (setf (get \'$to_poly_solve \'autoload) \"to_poly_solve\")\n") +
	wxString("\n") +
	wxString(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n") +
	wxString("  ;;\n") +
	wxString("  ;; Redefine load so that it prints the list of functions\n") +
	wxString("  ;; used for autocompletion.\n") +
	wxString("  ;;\n") +
	wxString(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n") +
	wxString("  (defun symbol-to-xml (s)\n") +
	wxString("    (wxxml-fix-string (format nil \"~a\" (maybe-invert-string-case (symbol-name (stripdollar s))))))\n") +
	wxString("\n") +
	wxString("  (defun print_unit (unit)\n") +
	wxString("    (format nil \"<unit>~a</unit>\" (symbol-to-xml unit)))\n") +
	wxString("\n") +
	wxString("  (defun $print_function (fun)\n") +
	wxString("    (let ((fun-name (symbol-to-xml (caar fun)))\n") +
	wxString("	  (*print-circle* nil)\n") +
	wxString("	  (args (mapcar (lambda (u)\n") +
	wxString("			  (cond ((atom u) (symbol-to-xml u))\n") +
	wxString("				((eq (caar u) \'mlist)\n") +
	wxString("				 ($concat \"[\" (symbol-to-xml\n") +
	wxString("					       (if (atom (cadr u)) (cadr u) (cadadr u))) \"]\"))\n") +
	wxString("				(t (symbol-to-xml (cadr u)))))\n") +
	wxString("			(cdr fun))))\n") +
	wxString("      (format nil \"<function>~a</function><template>~a(~{&lt;~a&gt;~^, ~})</template>\" fun-name fun-name args)))\n") +
	wxString("\n") +
	wxString("  (defun print_value (val)\n") +
	wxString("    (format nil \"<value>~a</value>\" (symbol-to-xml val)))\n") +
	wxString("\n") +
	wxString("  (defun $add_function_template (&rest functs)\n") +
	wxString("    (let ((*print-circle* nil))\n") +
	wxString("      (format t \"<wxxml-symbols>~{~a~^$~}</wxxml-symbols>\" (mapcar #\'$print_function functs))\n") +
	wxString("      (cons \'(mlist simp) functs)))\n") +
	wxString("\n") +
	wxString("\n") +
	wxString("  ;; A function that determines all symbols for autocompletion\n") +
	wxString("  (defun wxPrint_autoompletesymbols ()\n") +
	wxString("    (format t \"<wxxml-symbols>\")\n") +
	wxString("    ;; Function names and rules\n") +
	wxString("    (format t \"~{~a~^$~}\"\n") +
	wxString("	    (append (mapcar #\'$print_function (cdr ($append $functions $macros)))\n") +
	wxString("		    (mapcar #\'print_value (cdr ($append $values $rules)))))\n") +
	wxString("    ;; Idea from Robert Dodier:\n") +
	wxString("    ;; Variables defined with mdef don\'t appear in $values nor do they in $myoptions\n") +
	wxString("    ;; but they appear in *variable-initial-values*\n") +
	wxString("    (maphash (lambda (key val)\n") +
	wxString("	       (declare (ignore val))\n") +
	wxString("	       (if (eq (char (format nil \"~a\" key) 0) #\\$ )\n") +
	wxString("		   (format t \"~a\" (print_value key))))\n") +
	wxString("	     *variable-initial-values*)\n") +
	wxString("\n") +
	wxString("    ;; ezunits publishes all known units in a function.\n") +
	wxString("    ;; Currently the following lines produce a warning, though, that\n") +
	wxString("    ;; $known_units is undefined.\n") +
	wxString("    ;;	(if (boundp \'$known_units)\n") +
	wxString("    ;;	    (no-warning\n") +
	wxString("    ;;	     (format t \"~{~a~^$~}\"\n") +
	wxString("    ;;		     (mapcar #\'print_unit (cdr ($known_units))))))\n") +
	wxString("    (format t \"</wxxml-symbols>\"))\n") +
	wxString("\n") +
	wxString(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n") +
	wxString(";;;\n") +
	wxString(";;; Communication between wxMaxima and wxMaxima about variables and directories\n") +
	wxString(";;;\n") +
	wxString("\n") +
	wxString(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n") +
	wxString(";;;\n") +
	wxString(";;; Communicate the contents of variables to wxMaxima\n") +
	wxString("  (defun wx-print-variable (var)\n") +
	wxString("    (format t \"<variable>\")\n") +
	wxString("    (format t \"<name>~a</name>\" (symbol-to-xml var))\n") +
	wxString("    (if (boundp var)\n") +
	wxString("	(format t \"<value>~a</value>\" (wxxml-fix-string(eval var))))\n") +
	wxString("    (format t \"</variable>\"))\n") +
	wxString("\n") +
	wxString("  (defun wx-print-variables ()\n") +
	wxString("    (format t \"<variables>\")\n") +
	wxString("    (wx-print-variable \'$maxima_userdir)\n") +
	wxString("    (wx-print-variable \'$maxima_tempdir)\n") +
	wxString("    (wx-print-variable \'*maxima-htmldir*)\n") +
	wxString("					;  (wx-print-variable \'*maxima-topdir*)\n") +
	wxString("    (wx-print-variable \'$gnuplot_command)\n") +
	wxString("    (wx-print-variable \'*maxima-demodir*)\n") +
	wxString("    (wx-print-variable \'*maxima-sharedir*)\n") +
	wxString("    (wx-print-variable \'*autoconf-version*)\n") +
	wxString("    (wx-print-variable \'*autoconf-host*)\n") +
	wxString("    (format t \"<variable><name>*lisp-name*</name><value>~a</value></variable>\"\n") +
	wxString("	    #+sbcl (ensure-readably-printable-string (lisp-implementation-type))\n") +
	wxString("	    #-sbcl (lisp-implementation-type))\n") +
	wxString("    (format t \"<variable><name>*lisp-version*</name><value>~a</value></variable>\"\n") +
	wxString("	    #+sbcl (ensure-readably-printable-string (lisp-implementation-version))\n") +
	wxString("	    #-sbcl (lisp-implementation-version))\n") +
	wxString("    (format t \"</variables>\")\n") +
	wxString("    )\n") +
	wxString("\n") +
	wxString(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n") +
	wxString("  ;; A function that allows wxMaxima to set maxima\'s current directory to that\n") +
	wxString("  ;; of the worksheet.\n") +
	wxString("  (defun wx-cd (dir)\n") +
	wxString("    (handler-case\n") +
	wxString("      (progn\n") +
	wxString("        (let ((dir (cond ((pathnamep dir) dir)\n") +
	wxString("	  	       ((stringp dir)\n") +
	wxString("	  		(make-pathname :directory (pathname-directory dir)\n") +
	wxString("	    			       :host (pathname-host dir)\n") +
	wxString("	  			       :device (pathname-device dir)))\n") +
	wxString("	  	       (t (error \"cd(dir): dir must be a string or pathname.\")))))\n") +
	wxString("	  #+  allegro (excl:chdir dir)\n") +
	wxString("	  #+clisp (ext:cd dir)\n") +
	wxString("	  #+cmu (setf (ext:default-directory) dir)\n") +
	wxString("	  #+cormanlisp (ccl:set-current-directory dir)\n") +
	wxString("	  #+gcl (si::chdir dir)\n") +
	wxString("	  #+lispworks (hcl:change-directory dir)\n") +
	wxString("	  #+lucid (lcl:working-directory dir)\n") +
	wxString("	  #+sbcl (sb-posix:chdir dir)\n") +
	wxString("	  #+sbcl (setf *default-pathname-defaults* (sb-ext:native-pathname (format nil \"~A~A\" (sb-posix:getcwd) \"/\")))\n") +
	wxString("	  #+ccl (ccl:cwd dir)\n") +
	wxString("	  #+ecl (si::chdir dir)\n") +
	wxString("         ;;; Officially gcl supports (si:chdir dir), too. But the version\n") +
	wxString("         ;;; shipped with debian and ubuntu (at least in Feb 2017) doesn\'t.\n") +
	wxString("	  #+gcl (xchdir dir)\n") +
	wxString("	  #+gcl (setf *default-pathname-defaults* dir)\n") +
	wxString("\n") +
	wxString("	  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl ccl ecl) (format t\n") +
	wxString("           \"Info: wxMathml.cpp: Changing the working dir during a maxima session isn\'t implemented for this lisp.\")\n") +
	wxString("  	  (namestring dir)\n") +
	wxString("	  (wx-print-variables)))\n") +
	wxString("      (error (c)") +
	wxString("        (format t \"Warning: Cannot change maxima\'s working directory to the dir of the worksheet:~%~&~%\")\n") +
	wxString("          (values 0 c))))\n") +
	wxString("\n") +
	wxString(";;;;;;;;;;;;;;;;;;;;;\n") +
	wxString("  ;; table_form implementation\n") +
	wxString("\n") +
	wxString("  (defun make-zeros (n)\n") +
	wxString("    (cons \'(mlist simp) (loop for i from 1 to n collect \"\")))\n") +
	wxString("  (defun take-first (l n)\n") +
	wxString("    (if (= n 0) nil (cons (first l) (take-first (rest l) (- n 1)))))\n") +
	wxString("\n") +
	wxString("  (defun $table_form (mat &rest opts)\n") +
	wxString("    (when (mapatom mat)\n") +
	wxString("      ($error \"table_form: the argument should not be an atom.\"))\n") +
	wxString("    (setq mat ($args mat))\n") +
	wxString("    (unless (every #\'$listp (cdr mat))\n") +
	wxString("      (setq mat (cons \'(mlist simp)\n") +
	wxString("		      (mapcar (lambda (e) (list \'(mlist simp) e))\n") +
	wxString("			      (cdr mat)))))\n") +
	wxString("    (setq opts (cons \'(mlist simp) opts))\n") +
	wxString("    (let ((row-names ($assoc \'$row_names opts))\n") +
	wxString("	  (col-names ($assoc \'$column_names opts))\n") +
	wxString("	  (m (apply #\'max (mapcar \'$length (cdr mat))))\n") +
	wxString("	  (n (length (cdr mat)))\n") +
	wxString("	  (mtrx \'(special)))\n") +
	wxString("      (when ($assoc \'$transpose opts)\n") +
	wxString("	(rotatef m n))\n") +
	wxString("      (when (eq row-names \'$auto)\n") +
	wxString("	(setq row-names (cons \'(mlist simp) (loop for i from 1 to n collect i))))\n") +
	wxString("      (when (eq col-names \'$auto)\n") +
	wxString("	(setq col-names (cons \'(mlist simp) (loop for i from 1 to m collect i))))\n") +
	wxString("      (when row-names\n") +
	wxString("	(setq row-names ($append row-names (make-zeros (- n ($length row-names)))))\n") +
	wxString("	(setq row-names (cons \'(mlist simp) (take-first (cdr row-names) n))))\n") +
	wxString("      (when col-names\n") +
	wxString("	(setq col-names ($append col-names (make-zeros (- m ($length col-names)))))\n") +
	wxString("	(setq col-names (cons \'(mlist simp) (take-first (cdr col-names) m))))\n") +
	wxString("      (when (and row-names col-names)\n") +
	wxString("	(setq col-names ($cons \"\" col-names)))\n") +
	wxString("      (setq mat (cons \'(mlist simp) (mapcar\n") +
	wxString("				     (lambda (r) ($append r (make-zeros (- m ($length r)))))\n") +
	wxString("				     (cdr mat))))\n") +
	wxString("      (setq mat ($apply \'$matrix mat))\n") +
	wxString("      (when ($assoc \'$transpose opts)\n") +
	wxString("	(setq mat ($transpose mat)))\n") +
	wxString("      (when row-names\n") +
	wxString("	(setq mat (cons \'($matrix simp)\n") +
	wxString("			(mapcar #\'$append (cdr ($transpose row-names)) (cdr mat))))\n") +
	wxString("	(setq mtrx (cons \'rownames mtrx)))\n") +
	wxString("      (when col-names\n") +
	wxString("	(setq mat (cons \'(matrix simp)\n") +
	wxString("			(cons col-names (cdr mat))))\n") +
	wxString("	(setq mtrx (cons \'colnames mtrx)))\n") +
	wxString("      ($ldisp (cons (append \'($matrix simp) mtrx) (cdr mat)))\n") +
	wxString("      \'$done))\n") +
	wxString("\n") +
	wxString("  (putprop \'$table_form t \'evfun)\n") +
	wxString("\n") +
	wxString("  ;; Load the initial functions (from mac-init.mac)\n") +
	wxString("  (let ((*print-circle* nil))\n") +
	wxString("    (format t \"<wxxml-symbols>~{~a~^$~}</wxxml-symbols>\"\n") +
	wxString("	    (mapcar #\'$print_function (cdr ($append $functions $macros)))))\n") +
	wxString("\n") +
	wxString("  (no-warning\n") +
	wxString("   (defun mredef-check (fnname)\n") +
	wxString("     (declare (ignore fnname))\n") +
	wxString("     t))\n") +
	wxString("\n") +
	wxString(";;; A function that loads bitmaps from files as a slideshow.\n") +
	wxString(";;; Todo: Replace this function by at least half-way-optimized LISP code.\n") +
	wxString("  (progn\n") +
	wxString("    (defprop $wxanimate_from_imgfiles t translated)\n") +
	wxString("    (add2lnc \'$wxanimate_from_imgfiles $props)\n") +
	wxString("    (defmtrfun ($wxanimate_from_imgfiles $any mdefine t nil)\n") +
	wxString("      ($x)\n") +
	wxString("      (declare (special $x))\n") +
	wxString("      (progn\n") +
	wxString("	(simplify (mfunction-call $printf t \'\"<mth><slide\"))\n") +
	wxString("	(cond\n") +
	wxString("	 ((is-boole-check (trd-msymeval $wxanimate_autoplay \'$wxanimate_autoplay))\n") +
	wxString("	  (simplify (mfunction-call $printf t \'\" running=\\\"false\\\"\"))))\n") +
	wxString("	(cond\n") +
	wxString("	 ((like\n") +
	wxString("	   (simplify\n") +
	wxString("	    `((mfactorial)\n") +
	wxString("	      ,(trd-msymeval $wxanimate_framerate \'$wxanimate_framerate)))\n") +
	wxString("	   \'$wxanimate_framerate)\n") +
	wxString("	  (simplify\n") +
	wxString("	   (mfunction-call $printf t \'\" fr=\\\"~d\\\"\"\n") +
	wxString("			   (trd-msymeval $wxanimate_framerate\n") +
	wxString("					 \'$wxanimate_framerate)))))\n") +
	wxString("	(simplify (mfunction-call $printf t \'\">\"))\n") +
	wxString("	(do (($i)\n") +
	wxString("	     (mdo (cdr $x) (cdr mdo)))\n") +
	wxString("	    ((null mdo) \'$done)\n") +
	wxString("	    (declare (special $i))\n") +
	wxString("	    (setq $i (car mdo))\n") +
	wxString("	    (simplify (mfunction-call $printf t \'\"~a;\" $i)))\n") +
	wxString("	(simplify (mfunction-call $printf t \'\"</slide></mth>\")))\n") +
	wxString("      ))\n") +
	wxString("\n") +
	wxString("\n") +
	wxString("  (when ($file_search \"wxmaxima-init\")\n") +
	wxString("    ($load \"wxmaxima-init\"))\n") +
	wxString("\n") +
	wxString(";;;\n") +
	wxString(";;; Now that we have loaded the init file we can rewrite of the function load\n") +
	wxString(";;; (maxima/src/mload.lisp) to displays functions and variable names after\n") +
	wxString(";;; loading a Maxima package so the autocomplete functionality knows which\n") +
	wxString(";;; function and variable names exists.\n") +
	wxString(";;;\n") +
	wxString(";;; We also inform wxMaxima about the name of the package and about the fact\n") +
	wxString(";;; if it was loaded by another package.\n") +
	wxString(";;;\n") +
	wxString("  (no-warning\n") +
	wxString("   (defun $load (filename)\n") +
	wxString("     (let ((searched-for\n") +
	wxString("	    ($file_search1 filename\n") +
	wxString("			   \'((mlist) $file_search_maxima $file_search_lisp  )))\n") +
	wxString("	   type)\n") +
	wxString("       (setq type ($file_type searched-for))\n") +
	wxString("       (case type\n") +
	wxString("	     (($maxima)\n") +
	wxString("	      (format t \"<variables>\")\n") +
	wxString("	      (format t \"<variable><name>*wx-load-file-name*</name><value>~a</value></variable>\"\n") +
	wxString("		      (wxxml-fix-string filename))\n") +
	wxString("	      (format t \"<variable><name>*wx-load-file-start*</name><value>1</value></variable>\")\n") +
	wxString("	      (format t \"</variables>\")\n") +
	wxString("	      ($batchload searched-for)\n") +
	wxString("	      (format t \"<variables>\")\n") +
	wxString("	      (format t \"<variable><name>*wx-load-file-start*</name><value>0</value></variable>\")\n") +
	wxString("	      (format t \"</variables>\")\n") +
	wxString("	      (wxPrint_autoompletesymbols))\n") +
	wxString("	     (($lisp $object)\n") +
	wxString("	      ;; do something about handling errors\n") +
	wxString("	      ;; during loading. Foobar fail act errors.\n") +
	wxString("	      (format t \"<variables>\")\n") +
	wxString("	      (format t \"<variable><name>*wx-load-file-name*</name><value>~a</value></variable>\"\n") +
	wxString("		      (wxxml-fix-string filename))\n") +
	wxString("	      (format t \"<variable><name>*wx-load-file-start*</name><value>1</value></variable>\")\n") +
	wxString("	      (format t \"</variables>\")\n") +
	wxString("	      (no-warning\n") +
	wxString("	       (load-and-tell searched-for))\n") +
	wxString("	      (format t \"<variables>\")\n") +
	wxString("	      (format t \"<variable><name>*wx-load-file-start*</name><value>0</value></variable>\")\n") +
	wxString("	      (format t \"</variables>\")\n") +
	wxString("	      (wxPrint_autoompletesymbols))\n") +
	wxString("	     (t\n") +
	wxString("	      (merror \"Maxima bug: Unknown file type ~M\" type)))\n") +
	wxString("       searched-for)))\n") +
	wxString("  (force-output)\n") +
	wxString("  (format t \"</suppressOutput>\")\n") +
	wxString("  ;; Publish all new global variables maxima might contain to wxMaxima\'s\n") +
	wxString("  ;; autocompletion feature.\n") +
	wxString("  (wxPrint_autoompletesymbols)\n") +
	wxString("  (wx-print-variables)\n") +
	wxString("  (force-output)\n") +
        wxString(")\n\0");
    }

wxString wxMathML::GetCmd()
{
  wxString cmd;
  
  wxStringTokenizer lines(m_wxMathML,wxT("\n"));
  while(lines.HasMoreTokens())
  {
    wxString line = lines.GetNextToken();
    wxString lineWithoutComments;

    bool stringIs = false;
    wxChar lastChar=wxT(' ');
    wxString::iterator ch = line.begin();
    while (ch < line.end())
    {
      // Remove formatting spaces
      if(((lastChar == ' ') && (*ch == ' ')) && (!stringIs))
	  ch++;
      else
	{
	  // Handle backslashes that might escape double quotes
	  if (*ch == wxT('\\'))
	    {
	      lineWithoutComments += *ch;
	      lastChar = *ch;
	      ch++;
	    }
	  else
	    {
	      // Handle strings
	      if (*ch == wxT('\"'))
		stringIs = !stringIs;

	      // Handle comments
	      if ((*ch == wxT(';')) && (!stringIs))
		break;
	    }
	  lineWithoutComments += *ch;
	  lastChar = *ch;
	  ch++;
	}
    }
    cmd += lineWithoutComments + " ";
  }
  return wxT(":lisp-quiet ") + cmd + "\n";
}
