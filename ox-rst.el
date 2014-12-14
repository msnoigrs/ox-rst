;;; ox-rst.el --- Export reStructuredText using org-mode.

;; Copyright (C) 2013  IGARASHI Masanao

;; Author: IGARASHI Masanao <syoux2@gmail.com>
;; Keywords: org, rst, reST, reStructuredText
;; Version: 0.1

;;; Commentary:
;; This library implements an reStructuredText back-end for
;; Org generic exporter.

;;; Code:

(eval-when-compile (require 'cl))
(require 'ox)
(require 'ox-publish)
(require 'ox-ascii)


;;; Define Back-End
(org-export-define-backend 'rst
  '((bold . org-rst-bold)
    (center-block . org-rst-center-block)
    (clock . org-rst-clock)
    (code . org-rst-code)
    (comment . org-rst-comment)
    (comment-block . org-rst-comment-block)
    (drawer . org-rst-drawer)
    (dynamic-block . org-rst-dynamic-block)
    (entity . org-rst-entity)
    (example-block . org-rst-example-block)
    (export-block . org-rst-export-block)
    (export-snippet . org-rst-export-snippet)
    (fixed-width . org-rst-fixed-width)
    (footnote-reference . org-rst-footnote-reference)
    (headline . org-rst-headline)
    (horizontal-rule . org-rst-horizontal-rule)
    (inline-src-block . org-rst-inline-src-block)
    (inlinetask . org-rst-inlinetask)
    (inner-template . org-rst-inner-template)
    (italic . org-rst-italic)
    (item . org-rst-item)
    (keyword . org-rst-keyword)
    (latex-environment . org-rst-latex-environment)
    (latex-fragment . org-rst-latex-fragment)
    (line-break . org-rst-line-break)
    (link . org-rst-link)
    (node-property . org-rst-node-property)
    (paragraph . org-rst-paragraph)
    (plain-list . org-rst-plain-list)
    (plain-text . org-rst-plain-text)
    (planning . org-rst-planning)
    (property-drawer . org-rst-property-drawer)
    (quote-block . org-rst-quote-block)
    (radio-target . org-rst-radio-target)
    (section . org-rst-section)
    (special-block . org-rst-special-block)
    (src-block . org-rst-src-block)
    (statistics-cookie . org-rst-statistics-cookie)
    (strike-through . org-rst-strike-through)
    (subscript . org-rst-subscript)
    (superscript . org-rst-superscript)
    (table . org-rst-table)
    (table-cell . org-rst-table-cell)
    (table-row . org-rst-table-row)
    (target . org-rst-target)
    (template . org-rst-template)
    (timestamp . org-rst-timestamp)
    (underline . org-rst-underline)
    (verbatim . org-rst-verbatim)
    (verse-block . org-rst-verse-block))
  :export-block '("RST" "REST" "RESTRUCTUREDTEXT")
  :menu-entry
  '(?r "Export to reStructuredText"
	   ((?R "As reStructuredText buffer" org-rst-export-as-rst)
		(?r "As reStructuredText file" org-rst-export-to-rst)))
  :options-alist
    '((:rst-link-org-as-html nil nil org-rst-link-org-files-as-html)
	  (:rst-link-use-abs-url nil "rst-link-use-abs-url" org-rst-link-use-abs-url)
      (:rst-inline-images nil nil org-rst-inline-images)
      (:rst-inline-image-rules nil nil org-rst-inline-image-rules)
      (:rst-link-org-files-as-html nil nil org-rst-link-org-files-as-html)
	  (:rst-link-home "RST_LINK_HOME" nil org-rst-link-home))
  :filters-alist '((:filter-headline . org-rst-filter-headline-blank-lines)
				   (:filter-parse-tree org-rst-separate-elements
									   org-rst-filter-paragraph-spacing)
				   (:filter-section . org-rst-filter-headline-blank-lines)))


;;; Internal Variables



;;; User Configurable Variables

(defgroup org-export-rst nil
  "Options for exporting Org mode files to reStructuredText."
  :tag "Org RST"
  :group 'org-export)


(defcustom org-rst-link-org-files-as-html t
  "Non-nil means make file links to `file.org' point to `file.html'.
When `org-mode' is exporting an `org-mode' file to HTML, links to
non-html files are directly put into a href tag in HTML.
However, links to other Org-mode files (recognized by the
extension `.org.) should become links to the corresponding html
file, assuming that the linked `org-mode' file will also be
converted to HTML.
When nil, the links still point to the plain `.org' file."
  :group 'org-export-rst
  :type 'boolean)


(defcustom org-rst-link-home ""
  "Where should the \"HOME\" link of exported HTML pages lead?"
  :group 'org-export-rst
  :type '(string :tag "File or URL"))


(defcustom org-rst-link-use-abs-url nil
  "Should we prepend relative links with RST_LINK_HOME?"
  :group 'org-export-rst
  :type 'boolean)

;;;; Links :: Inline images

(defcustom org-rst-inline-images t
  "Non-nil means inline images into exported reStructuredText.
This is done using an image directive or an figure directive.
When nil, an anchor with reference is used to link to the image."
  :group 'org-export-rst
  :type 'boolean)

(defcustom org-rst-inline-image-rules
  '(("file" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\|svgz\\|swf\\)\\'")
    ("fuzzy" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\|svgz\\|swf\\)\\'")
    ("http" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\|svgz\\|swf\\)\\'")
    ("https" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\|svgz\\|swf\\)\\'"))
  "Rules characterizing image files that can be inlined into reStructuredText.
A rule consists in an association whose key is the type of link
to consider, and value is a regexp that will be matched against
link's path."
  :group 'org-export-rst
  :type '(alist :key-type (string :tag "Type")
		:value-type (regexp :tag "Path")))


(defcustom org-rst-text-markup-alist '((bold . "**%s**")
									   (code . verb)
									   (italic . "*%s*")
									   (verbatim . verb)
									   (comment . ".. %s\n"))
  "Alist of reStructredText expressions to convert text markup.

The key must be a symbol among `bold', `code', `italic',
`comment' and `verbatim'.  The value is a formatting string to
wrap fontified text with.

Value can also be set to the following symbols: `verb'.

If no association can be found for a given markup, text will be
returned as-is."
  :group 'org-export-rst
  :type 'alist
  :options '(bold code italic verbatim comment))


(defcustom org-rst-quote-margin 4
  "Width of margin used for quoting text, in characters.
This margin is applied on left side of the text."
  :group 'org-export-rst
  :type 'integer)


(defcustom org-rst-headline-spacing '(1 . 1)
  "Number of blank lines inserted around headlines.

This variable can be set to a cons cell.  In that case, its car
represents the number of blank lines present before headline
contents whereas its cdr reflects the number of blank lines after
contents.

A nil value replicates the number of blank lines found in the
original Org buffer at the same place."
  :group 'org-export-rst
  :type '(choice
	  (const :tag "Replicate original spacing" nil)
	  (cons :tag "Set an uniform spacing"
		(integer :tag "Number of blank lines before contents")
		(integer :tag "Number of blank lines after contents"))))


(defcustom org-rst-paragraph-spacing 'auto
  "Number of white lines between paragraphs.
If the value is an integer, add this number of blank lines
between contiguous paragraphs.  If is it the symbol `auto', keep
the same number of blank lines as in the original document."
  :group 'org-export-rst
  :type '(choice
	  (integer :tag "Number of blank lines")
	  (const :tag "Preserve original spacing" auto)))


;;;; Drawers

(defcustom org-rst-format-drawer-function nil
  "Function called to format a drawer in reStructuredText code.

The function must accept two parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.

The function should return the string to be exported.

For example, the variable could be set to the following function
in order to mimic default behaviour:

\(defun org-rst-format-drawer-default \(name contents\)
  \"Format a drawer element for reStructuredText export.\"
  contents\)"
  :group 'org-export-rst
  :type 'function)


;;;; Inlinetasks

(defcustom org-rst-format-inlinetask-function nil
  "Function called to format an inlinetask in reStructuredText code.

The function must accept six parameters:
  TODO      the todo keyword, as a string
  TODO-TYPE the todo type, a symbol among `todo', `done' and nil.
  PRIORITY  the inlinetask priority, as a string
  NAME      the inlinetask name, as a string.
  TAGS      the inlinetask tags, as a list of strings.
  CONTENTS  the contents of the inlinetask, as a string.

The function should return the string to be exported."
  :group 'org-export-rst
  :type 'function)


;;;; Src blocks

(defcustom org-rst-pygments t
  "Non-nil means export source code using the pygments package."
  :group 'org-export-rst
  :type '(choice
	  (const :tag "Use pygments" t)
	  (const :tag "Export verbatim" nil)))


(defcustom org-rst-pygments-langs
  '((emacs-lisp "scheme") (lisp "scheme") (clojure "clojure")
    (c "c") (cc "cpp")
    (fortran "fortran")
    (perl "perl") (cperl "perl") (python "python") (ruby "ruby")
    (html "html") (xml "xml")
    (tex "tex") (latex "latex")
    (shell-script "bash")
    (gnuplot "gnuplot")
    (ocaml "ocaml") (caml "ocaml")
    (sql "sql") (sqlite "sqlite3"))
  "Alist mapping languages to their listing language counterpart.
The key is a symbol, the major mode symbol without the \"-mode\".
The value is the string that should be inserted as the language
parameter for the listings package.  If the mode name and the
listings name are the same, the language does not need an entry
in this list - but it does not hurt if it is present."
  :group 'org-export-rst
  :type '(repeat
	  (list
	   (symbol :tag "Major mode       ")
	   (string :tag "Pygments language"))))



;;; Internal Functions

(defun org-rst--indent-string (s width)
  "Indent string S by WIDTH white spaces.
Empty lines are not indented."
  (when (stringp s)
    (replace-regexp-in-string
     "\\(^\\)\\(?:.*\\S-\\)" (make-string width ? ) s nil nil 1)))


(defun org-rst--has-caption-p (element info)
  "Non-nil when ELEMENT has a caption affiliated keyword.
INFO is a plist used as a communication channel.  This function
is meant to be used as a predicate for `org-export-get-ordinal'."
  (org-element-property :caption element))


(defun org-rst--make-attribute-string (attributes)
  "Return a list of attributes, as a string.
ATTRIBUTES is a plist where values are either strings or nil. An
attributes with a nil value will be omitted from the result."
  (let (output)
    (dolist (item attributes (mapconcat 'identity (nreverse output) "\n"))
      (cond ((null item) (pop output))
            ((symbolp item) (push (substring (symbol-name item) 1) output))
            (t (let ((key (org-trim (car output)))
                     (value (replace-regexp-in-string "\"" "\\\""
							 (replace-regexp-in-string
							  "\\\\" "\\\\" (org-trim item)))))
                 (setcar output (format "    :%s: %s" key value))))))))


(defun org-rst--build-title
  (element info &optional underline notags toc)
  "Format ELEMENT title and return it.

ELEMENT is either an `headline' or `inlinetask' element.  INFO is
a plist used as a communication channel.

When optional argument UNDERLINE is non-nil, underline title,
without the tags, according to `org-rst-underline'
specifications.

If optional argument NOTAGS is non-nil, no tags will be added to
the title.

When optional argument TOC is non-nil, use optional title if
possible.  It doesn't apply to `inlinetask' elements."
  (let* ((headlinep (eq (org-element-type element) 'headline))
		 (numbers
		  ;; Numbering is specific to headlines.
		  (and headlinep (org-export-numbered-headline-p element info)
			   ;; All tests passed: build numbering string.
			   (concat
				(mapconcat
				 'number-to-string
				 (org-export-get-headline-number element info) ".")
				" ")))
		 (text
		  (org-trim
		   (org-export-data
			(if (and toc headlinep) (org-export-get-alt-title element info)
			  (org-element-property :title element))
			info)))
		 (todo
		  (and (plist-get info :with-todo-keywords)
			   (let ((todo (org-element-property :todo-keyword element)))
				 (and todo (concat (org-export-data todo info) " ")))))
		 (tags (and (not notags)
					(plist-get info :with-tags)
					(let ((tag-list (org-export-get-tags element info)))
					  (and tag-list
						   (format ":%s:"
								   (mapconcat 'identity tag-list ":"))))))
		 (priority
		  (and (plist-get info :with-priority)
			   (let ((char (org-element-property :priority element)))
				 (and char (format "(#%c) " char)))))
		 (first-part (concat numbers todo priority text)))
    (concat
     first-part
     ;; Align tags, if any.
     (when tags
       (format
		(format " %%%ds" (string-width tags))
		tags))
     ;; Maybe underline text, if ELEMENT type is `headline' and an
     ;; underline character has been defined.
     (when (and underline headlinep)
       (let ((under-char
			  (nth (1- (org-export-get-relative-level element info))
				   '(?- ?~ ?^ ?: ?' ?\ ?_))))
		 (and under-char
			  (concat "\n"
					  (make-string (string-width first-part) under-char))))))))


(defun org-rst--text-markup (text markup)
  "Format TEXT depending on MARKUP text markup.
See `org-rst-text-markup-alist' for details."
  (let ((fmt (cdr (assq markup org-rst-text-markup-alist))))
    (cond
     ;; No format string: Return raw text.
     ((not fmt) text)
     ;; Handle the `verb' special case: Protect some
     ;; special chars and use "\\" escape.
     ((eq 'verb fmt)
      (let ((start 0)
	    (rtn "")
	    char)
		(while (string-match "\\`*" text)
		  (setq char (match-string 0 text))
		  (if (> (match-beginning 0) 0)
			  (setq rtn (concat rtn (substring text 0 (match-beginning 0)))))
		  (setq text (substring text (1+ (match-beginning 0))))
		  (setq char (concat "\\" char)
				rtn (concat rtn char)))
		(setq text (concat rtn text)
			  fmt "``%s``")
		(format fmt text)))
     ;; Else use format string.
     (t (format fmt text)))))


(defun org-rst--describe-links (links info)
  "Return a string describing a list of links.

LINKS is a list of link type objects, as returned by
`org-rst--unique-links'.  INFO is a plist used as a communication
channel."
  (mapconcat
   (lambda (link)
     (let ((type (org-element-property :type link))
	   (anchor (let ((desc (org-element-contents link)))
		     (if desc (org-export-data desc info)
		       (org-element-property :raw-link link)))))
       (cond
	;; Coderefs, radio links and fuzzy links are ignored.
	((member type '("coderef" "radio" "fuzzy")) nil)
	;; Id and custom-id links: Headlines refer to their numbering.
	((member type '("custom-id" "id"))
	 (let ((dest (org-export-resolve-id-link link info)))
	   (concat
		(format
		 "`%s <%s>`_"
		 anchor
		 (if (not dest) "Unknown reference"
		   (format
			"See section %s"
			(mapconcat 'number-to-string
					   (org-export-get-headline-number dest info) "."))))
		   "\n\n")))
	;; Do not add a link that cannot be resolved and doesn't have
	;; any description: destination is already visible in the
	;; paragraph.
	((not (org-element-contents link)) nil)
	(t
	 (concat
	  (format "`%s <%s>`_" anchor (org-element-property :raw-link link))
	  "\n\n")))))
   links ""))


(defun org-rst--unique-links (element info)
  "Return a list of unique link references in ELEMENT.

ELEMENT is either a headline element or a section element.  INFO
is a plist used as a communication channel."
  (let* (seen
		 (unique-link-p
		  (function
		   ;; Return LINK if it wasn't referenced so far, or nil.
		   ;; Update SEEN links along the way.
		   (lambda (link)
			 (let ((footprint
                    ;; Normalize description in footprints.
					(cons (org-element-property :raw-link link)
                          (let ((contents (org-element-contents link)))
                            (and contents
                                 (replace-regexp-in-string
                                  "[ \r\t\n]+" " "
                                  (org-trim
                                   (org-element-interpret-data contents))))))))
			   ;; Ignore LINK if it hasn't been translated already.
			   ;; It can happen if it is located in an affiliated
			   ;; keyword that was ignored.
			   (when (and (org-string-nw-p
						   (gethash link (plist-get info :exported-data)))
						  (not (member footprint seen)))
				 (push footprint seen) link)))))
		 ;; If at a section, find parent headline, if any, in order to
		 ;; count links that might be in the title.
		 (headline
		  (if (eq (org-element-type element) 'headline) element
			(or (org-export-get-parent-headline element) element))))
    ;; Get all links in HEADLINE.
    (org-element-map headline 'link
      (lambda (l) (funcall unique-link-p l)) info nil nil t)))


(defun org-rst--checkbox (item info)
  "Return checkbox string for ITEM or nil.
INFO is a plist used as a communication channel."
  ;(let ((utf8p (eq (plist-get info :ascii-charset) 'utf-8)))
  ;  (case (org-element-property :checkbox item)
  ;    (on (if utf8p "☑ " "[X] "))
  ;    (off (if utf8p "☐ " "[ ] "))
  ;    (trans (if utf8p "☒ " "[-] ")))))
  (case (org-element-property :checkbox item)
	(on "☑ ")
	(off "☐ ")
	(trans "☒ ")))



;;; Template

(defun org-rst-template--document-title (info)
  "Return document title, as a string.
INFO is a plist used as a communication channel."
  (let* (;; Links in the title will not be resolved later, so we make
		 ;; sure their path is located right after them.
		 (title (org-export-data (plist-get info :title) info))
		 (author (and (plist-get info :with-author)
					  (let ((auth (plist-get info :author)))
						(and auth (org-export-data auth info)))))
		 (email (and (plist-get info :with-email)
					 (org-export-data (plist-get info :email) info)))
		 (date (and (plist-get info :with-date)
					(org-export-data (org-export-get-date info) info)))
		 (title
		  (if (string= title "")
			  (cond
			   ((and (org-string-nw-p date) (org-string-nw-p author))
				(concat
				 author
				 date
				 (when (org-string-nw-p email) email)))
			   ((and (org-string-nw-p date) (org-string-nw-p email))
				(concat
				 email
				 date
				 date))
			   ((org-string-nw-p date)
				date)
			   ((and (org-string-nw-p author) (org-string-nw-p email))
				(concat author email))
			   ((org-string-nw-p author) author)
			   ((org-string-nw-p email) email)) title))
		  (titleline (make-string (string-width title) ?=)))
	(concat
	 title "\n"
	 titleline "\n"
	 (when (org-string-nw-p author) (concat "\n    :Author: " author))
	 (when (org-string-nw-p email) (concat "\n    :Contact: " email))
	 (when (org-string-nw-p date) (concat "\n    :Date: " date))
	 "\n")))


(defun org-rst-template (contents info)
  "Return complete document string after reStructuredText conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; 1. Build title block.
   (concat (org-rst-template--document-title info)
		   ;; 2. Table of contents.
		   (let ((depth (plist-get info :with-toc)))
			 (when depth "\n.. contents::\n")))
   ;; 3. Document's body.
   contents
   ;; 4. Creator.  Ignore `comment' value as there are no comments in
   ;;    ASCII.  Justify it to the bottom right.
   (let ((creator-info (plist-get info :with-creator)))
	 (unless (or (not creator-info) (eq creator-info 'comment))
	   (concat
		"\n    :Creator: "
		(plist-get info :creator))))))



;;; Transcode Functions

;;;; Bold

(defun org-rst-bold (bold contents info)
  "Transcode BOLD from Org to reStructuredText.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (org-rst--text-markup contents 'bold))


;;;; Center Block

(defun org-rst-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to reStructuredText.
CONTENTS holds the contents of the center block.  INFO is a plist
holding contextual information."
  contents)


;;;; Clock

(defun org-rst-clock (clock contents info)
  "Transcode a CLOCK object from Org to reStructuredText.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (concat org-clock-string " "
		  (org-translate-time
		   (org-element-property :raw-value
								 (org-element-property :value clock)))
		  (let ((time (org-element-property :duration clock)))
			(and time
				 (concat " => "
						 (apply 'format
								"%2s:%02s"
								(org-split-string time ":")))))))


;;;; Code

(defun org-rst-code (code contents info)
  "Transcode a CODE object from Org to reStructuredText.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-rst--text-markup (org-element-property :value code) 'code))


;;; Comment
(defun org-rst-comment (comment contents info)
  "Transcode a COMMENT object from Org to reStructuredText.
CONTENTS is the text in the comment.  INFO is a plist holding
contextual information."
  (org-rst--text-markup (org-element-property :value comment) 'comment))


;;; Comment Block

(defun org-rst-comment-block (comment-block contents info)
  "Transcode a COMMENT-BLOCK object from Org to reStructuredText.
CONTENTS is the text within the block.  INFO is a plist holding
contextual information."
  (let ((value (org-remove-indentation
				(org-element-property :value comment-block))))
    (when value
	  (concat "..\n" (org-rst--indent-string value org-rst-quote-margin)))))


;;;; Drawer

(defun org-rst-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to reStructuredText.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((name (org-element-property :drawer-name drawer))
	 (output (if (functionp org-rst-format-drawer-function)
		     (funcall org-rst-format-drawer-function
			      name contents)
		   ;; If there's no user defined function: simply
		   ;; display contents of the drawer.
		   contents)))
    output))


;;;; Dynamic Block

(defun org-rst-dynamic-block (dynamic-block contents info)
  "Transcode a DYNAMIC-BLOCK element from Org to reStructuredText.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  contents)


;;;; Entity

(defun org-rst-entity (entity contents info)
  "Transcode an ENTITY object from Org to reStructuredText.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (org-element-property :utf-8 entity))


;;;; Example Block

(defun org-rst-example-block (example-block contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to reStructuredText.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let* ((example (org-remove-indentation
				   (org-element-property :value example-block)))
		 (label (org-element-property :name example-block))
		 (attributes
		  (org-export-read-attribute :attr_rst example-block))
		 (class (plist-get attributes :class)))
	(when example
	  (concat
	   "::\n"
	   (when class (format "    :class: %s\n" class))
	   (when label (format "    :name: %s\n"
						   (org-export-solidify-link-text label)))
	   "\n"
	   (org-rst--indent-string example org-rst-quote-margin)))))


;;;; Export Block

(defun org-rst-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to reStructuredText.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (member (org-element-property :type export-block) '("RST" "REST" "RESTRUCTUREDTEXT"))
    (org-element-property :value export-block)))


;;;; Export Snippet

(defun org-rst-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to reStructuredText.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'rst)
    (org-element-property :value export-snippet)))


;;;; Fixed Width

(defun org-rst-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to reStructuredText.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-rst--text-markup (org-element-property :value fixed-width) 'verbatim))


;;;; Footnote Definition

;(defun org-rst-footnote-definition (footnote-definition contents info)
;  "Transcode a FOOTNOTE-DEFINITION element from Org to reStructuredText.
;CONTENTS is nil.  INFO is a plist holding contextual information."
;  (replace-regexp-in-string
;   "^" ".. "
;   (org-remove-indentation
;	(org-element-property :value footnote-definition))))


;;;; Footnote Reference

(defun org-rst-footnote-reference (footnote-reference contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to reStructuredText.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format " [%s]_" (org-export-get-footnote-number footnote-reference info)))


;;;; Headline

(defun org-rst-headline (headline contents info)
  "Transcode a HEADLINE element from Org to reStructuredText.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  ;; Don't export footnote section, which will be handled at the end
  ;; of the template.
  (unless (org-element-property :footnote-section-p headline)
    (let* (;; Blank lines between headline and its contents.
		   ;; `org-rst-headline-spacing', when set, overwrites
		   ;; original buffer's spacing.
		   (pre-blanks
			(make-string
			 (if org-rst-headline-spacing (car org-rst-headline-spacing)
			   (org-element-property :pre-blank headline)) ?\n))
		   ;; Even if HEADLINE has no section, there might be some
		   ;; links in its title that we shouldn't forget to describe.
		   (links
			(unless (or (eq (caar (org-element-contents headline)) 'section))
			  (let ((title (org-element-property :title headline)))
				(when (consp title)
				  (org-rst--describe-links
				   (org-rst--unique-links title info) info))))))
	  (concat
	   (org-rst--build-title headline info 'underline)
	   "\n" pre-blanks
	   (concat (when (org-string-nw-p links) links) contents)))))


;;;; Horizontal Rule

(defun org-rst-horizontal-rule (horizontal-rule contents info)
  "Transcode an HORIZONTAL-RULE object from Org to reStructuredText.
CONTENTS is nil.  INFO is a plist holding contextual information."
  "\n------------\n")

;;;; Inline Src Block

(defun org-rst-inline-src-block (inline-src-block contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to reStructuredText.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (org-rst--text-markup
   (org-element-property :value inline-src-block) 'verbatim))


;;;; Inlinetask

(defun org-rst-inlinetask (inlinetask contents info)
  "Transcode an INLINETASK element from Org to reStructuredText.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((title (org-export-data (org-element-property :title inlinetask) info))
		(todo (and (plist-get info :with-todo-keywords)
				   (let ((todo (org-element-property :todo-keyword inlinetask)))
					 (and todo (org-export-data todo info)))))
		(todo-type (org-element-property :todo-type inlinetask))
		(tags (and (plist-get info :with-tags)
				   (org-export-get-tags inlinetask info)))
		(priority (and (plist-get info :with-priority)
					   (org-element-property :priority inlinetask))))
    ;; If `org-rst-format-inlinetask-function' is provided, call it
    ;; with appropriate arguments.
    (if (functionp org-rst-format-inlinetask-function)
		(funcall org-rst-format-inlinetask-function
				 todo todo-type priority title tags contents)
      ;; Otherwise, use a default template.
       (let ((full-title
			  (concat
			   (when todo (format "%s" todo))
			   (when priority (format "\#%c " priority))
			   title
			   (when tags (format ":%s:"
								  (mapconcat 'identity tags ":"))))))
		 (format (concat "%s\n\n"
						 "%s\n")
				 full-title contents)))))


;;;; Inner template

(defun org-rst-inner-template (contents info)
  "Return complete document string after reStructuredText conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (org-element-normalize-string
   (concat
	;; 1. Document's body.
	contents
	;; 2. Footnote definitions.
	(let ((definitions (org-export-collect-footnote-definitions
						(plist-get info :parse-tree) info))
		  ;; Insert full links right inside the footnote definition
		  ;; as they have no chance to be inserted later.
		  (org-rst-links-to-notes nil))
	  (when definitions
		(concat
		 "\n\n"
		 (mapconcat
		  (lambda (ref)
			(let ((id (format ".. [%s] " (car ref))))
			  ;; Distinguish between inline definitions and
			  ;; full-fledged definitions.
			  (org-trim
			   (let ((def (nth 2 ref)))
				 (if (eq (org-element-type def) 'org-data)
					 ;; Full-fledged definition: footnote ID is
					 ;; inserted inside the first parsed paragraph
					 ;; (FIRST), if any, to be sure filling will
					 ;; take it into consideration.
					 (let ((first (car (org-element-contents def))))
					   (if (not (eq (org-element-type first) 'paragraph))
						   (concat id "\n" (org-export-data def info))
						 (push id (nthcdr 2 first))
						 (org-export-data def info)))
				   ;; Fill paragraph once footnote ID is inserted
				   ;; in order to have a correct length for first
				   ;; line.
				   (concat id (org-export-data def info)))))))
		  definitions "\n\n")))))))


;;;; Italic

(defun org-rst-italic (italic contents info)
  "Transcode ITALIC from Org to reStructuredText.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (org-rst--text-markup contents 'italic))


;;;; Item

(defun org-rst-item (item contents info)
  "Transcode ITEM element into reStructuredText format.
CONTENTS is the item contents.  INFO is a plist used as
a communication channel."
  (let* ((checkbox (org-rst--checkbox item info))
		 (list-type (org-element-property :type (org-export-get-parent item)))
		 (tag (let
				  ((tag (org-element-property :tag item)))
				(and tag (concat (org-export-data tag info) checkbox))))
		 (bullet
		  ;; First parent of ITEM is always the plain-list.  Get
		  ;; `:type' property from it.
		  (org-list-bullet-string
		   (cond
			((eq list-type 'ordered)
			 ;; Return correct number for ITEM, paying attention to
			 ;; counters.
			 (let* ((struct (org-element-property :structure item))
					(bul (org-element-property :bullet item))
					(num (number-to-string
						  (car (last (org-list-get-item-number
									  (org-element-property :begin item)
									  struct
									  (org-list-prevs-alist struct)
									  (org-list-parents-alist struct)))))))
			   (replace-regexp-in-string "[0-9]+" num bul)))
			(tag "")
			(t "-"))))
		 (width (if tag 4 (string-width bullet)))
		 )
    (concat
     (if tag tag (concat bullet checkbox))
     (let ((contents (org-rst--indent-string contents width)))
       (if (and (not tag)
				(eq (org-element-type (car (org-element-contents item))) 'paragraph))
		   (org-trim contents)
		 (concat "\n" contents))))))


;;;; Keyword

(defun org-rst-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to reStructuredText.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
	(value (org-element-property :value keyword)))
    (cond
     ((string= key "RST") value)
     ((string= key "TOC") (downcase value)))))


;;;; Latex Environment

(defun org-rst-latex-environment (latex-environment contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to reStructuredText.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (when (plist-get info :with-latex)
    (org-remove-indentation (org-element-property :value latex-environment))))


;;;; Latex Fragment

(defun org-rst-latex-fragment (latex-fragment contents info)
  "Transcode a LATEX-FRAGMENT object from Org to reStructuredText.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (when (plist-get info :with-latex)
    (org-element-property :value latex-fragment)))


;;;; Line Break

(defun org-rst-line-break (line-break contents info)
  "Transcode a LINE-BREAK object from Org to reStructuredText.
CONTENTS is nil.  INFO is a plist holding contextual
  information."
  hard-newline)


;;;; Link

(defun org-rst-inline-image-p (link info)
  "Non-nil when LINK is meant to appear as an image.
INFO is a plist used as a communication channel.  LINK is an
inline image when it has no description and targets an image
file (see `org-rst-inline-image-rules' for more information), or
if its description is a single link targeting an image file."
  (if (not (org-element-contents link))
      (org-export-inline-image-p
       link (plist-get info :rst-inline-image-rules))
    (not
     (let ((link-count 0))
       (org-element-map (org-element-contents link)
		   (cons 'plain-text org-element-all-objects)
		 (lambda (obj)
		   (case (org-element-type obj)
			 (plain-text (org-string-nw-p obj))
			 (link (if (= link-count 1) t
					 (incf link-count)
					 (not (org-export-inline-image-p
						   obj (plist-get info :rst-inline-image-rules)))))
			 (otherwise t)))
         info t)))))


(defun my-org-export-inline-image-p (link &optional rules)
  (let ((case-fold-search t)
        (rules (or rules org-export-default-inline-image-rule)))
    (catch 'exit
      (mapc
       (lambda (rule)
         (if (string-match (cdr rule) link)
              (throw 'exit t)))
       rules)
      ;; Return nil if no rule matched.
      nil)))


(defun org-rst-link (link desc info)
  "Transcode a LINK object from Org to reStructuredText.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information."
  (let* ((home (when (plist-get info :html-link-home)
				 (org-trim (plist-get info :html-link-home))))
		 (use-abs-url (plist-get info :html-link-use-abs-url))
		 (link-org-files-as-html-maybe
		  (function
		   (lambda (raw-path info)
			 "Treat links to `file.org' as links to `file.html', if needed.
           See `org-rst-link-org-files-as-html'."
			 (cond
			  ((and (plist-get info :rst-link-org-files-as-html)
					(string= ".org"
							 (downcase (file-name-extension raw-path "."))))
			   (concat (file-name-sans-extension raw-path) "."
					   (plist-get info :html-extension)))
			  (t raw-path)))))
		 (type (org-element-property :type link))
		 (raw-path (org-element-property :path link))
		 ;; Ensure DESC really exists, or set it to nil.
		 (desc (and (not (string= desc "")) desc))
		 (path (cond
				((member type '("http" "https" "ftp" "mailto"))
				 (org-link-escape
				  (org-link-unescape
				   (concat type ":" raw-path)) org-link-escape-chars-browser))
				((string= type "file")
				 ;; Treat links to ".org" files as ".html", if needed.
				 (setq raw-path
					   (funcall link-org-files-as-html-maybe raw-path info))
				 (cond ((and home use-abs-url)
						(setq raw-path
							  (concat (file-name-as-directory home) raw-path)))))
				(t raw-path)))
		 ;; Extract attributes from parent's paragraph.  HACK: Only do
		 ;; this for the first link in parent (inner image link for
		 ;; inline images).  This is needed as long as attributes
		 ;; cannot be set on a per link basis.
		 (attributes-plist
		  (let* ((parent (org-export-get-parent-element link))
				 (link (let ((container (org-export-get-parent link)))
						 (if (and (eq (org-element-type container) 'link)
								  (org-rst-inline-image-p link info))
							 container
						   link))))
			(and (eq (org-element-map parent 'link 'identity info t) link)
				 (org-export-read-attribute :attr_rst parent))))
		 (attributes
		  (let ((attr (org-rst--make-attribute-string attributes-plist)))
			(if (org-string-nw-p attr) (concat "\n" attr "\n") "")))
		 protocol)
    (cond
     ;; Image file.
     ((and (plist-get info :rst-inline-images)
           (org-export-inline-image-p
            link (plist-get info :rst-inline-image-rules)))
	  (let ((ipath (if (not (file-name-absolute-p raw-path)) raw-path
					 (expand-file-name raw-path)))
			(caption (org-export-get-caption
					  (org-export-get-parent-element link))))
		(if caption (format ".. figure:: %s%s\n    %s\n" ipath attributes
							(org-export-data caption info))
		  (format ".. image:: %s%s\n" ipath	attributes))))
     ((and (plist-get info :rst-inline-images)
           desc
           (my-org-export-inline-image-p
            desc (plist-get info :rst-inline-image-rules)))
      (format ".. image:: %s\n    :target: %s%s" desc path attributes))
     ;; Radio link: Transcode target's contents and use them as link's
     ;; description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
		(when destination
			(format "`%s <%s>`_"
					(org-export-solidify-link-text path)
					(org-export-data (org-element-contents destination) info)))))
     ;; Links pointing to a headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
							 (org-export-resolve-fuzzy-link link info)
						   (org-export-resolve-id-link link info))))
		(case (org-element-type destination)
		  ;; Id link points to an external file.
		  (plain-text
		   (if desc (format "`%s <%s>`_" desc destination)
			 (format "`%s`_" destination)))
		  ;; Fuzzy link points nowhere.
		  ('nil
		   (let ((rawlink
				  (org-export-data (org-element-property :raw-link link) info)))
			 (if desc (format "`%s <%s>`_" desc rawlink)
			   (format "`%s`_" rawlink))))
		  ;; LINK points to a headline.  If headlines are numbered
		  ;; and the link has no description, display headline's
		  ;; number.  Otherwise, display description or headline's
		  ;; title.
		  (headline
		   (let ((label
				  (mapconcat
				   'number-to-string
				   (org-export-get-headline-number destination info)
				   ".")))
			 (if (and (plist-get info :section-numbers) (not desc))
				 (format "`%s`_" label)
			   (format "`%s <%s>`_" label
					   (or desc
						   (org-export-data
							(org-element-property :title destination) info))))))
          ;; Fuzzy link points to a target.  Do as above.
		  (otherwise
		   (let ((path (org-export-solidify-link-text path)))
			 (if (not desc) (format "`%s`_" path)
			   (format "`%s <%s>`_" desc path)))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (format (org-export-get-coderef-format path desc)
			  (org-export-resolve-coderef path info)))
     ;; Link type is handled by a special function.
     ;((functionp (setq protocol (nth 2 (assoc type org-link-protocols))))
     ; (funcall protocol (org-link-unescape path) desc 'latex))
     ;; External link with a description part.
     ((and path desc) (format "`%s <%s>`_" desc path))
     ;; External link without a description part.
     (path (format "`%s`_" path))
     ;; No path, only description.  Try to do something useful.
     (t (format "`%s`_" desc)))))


;;;; Node Property

(defun org-rst-node-property (node-property contents info)
  "Transcode a NODE-PROPERTY element from Org to reStructuredText.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "%s:%s"
          (org-element-property :key node-property)
          (let ((value (org-element-property :value node-property)))
            (if value (concat " " value) ""))))


;;;; Paragraph

(defun org-rst-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to reStructuredText.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  contents)


;;;; Plain List

(defun org-rst-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to reStructuredText.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  contents)


;;;; Plain Text

(defun org-rst-plain-text (text info)
  "Transcode a TEXT string from Org to reStructuredText.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (when (plist-get info :with-smart-quotes)
    (setq text (org-export-activate-smart-quotes text :utf-8 info)))
  ;; Protect `, *, _ and \
  (setq text (replace-regexp-in-string "[`*_\\]" "\\\\\\&" text))
  ;; Protect ..
  (setq text (replace-regexp-in-string "^[\s-]*\\.\\. " "\\\\.. " text))
  ;; Protect ^\d+.
  (setq text (replace-regexp-in-string "^\\(\\d\\)+\\." "\\1\\." text))
  ;; Handle break preservation, if required.
  (when (plist-get info :preserve-breaks)
    (setq text (replace-regexp-in-string "^" "| "  text)))
  ;; Return value.
  text)


;;;; Planning

(defun org-rst-planning (planning contents info)
  "Transcode a PLANNING element from Org to reStructuredText.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (mapconcat
   'identity
   (delq nil
	 (list (let ((closed (org-element-property :closed planning)))
		 (when closed
		   (concat org-closed-string " "
			   (org-translate-time
			    (org-element-property :raw-value closed)))))
	       (let ((deadline (org-element-property :deadline planning)))
		 (when deadline
		   (concat org-deadline-string " "
			   (org-translate-time
			    (org-element-property :raw-value deadline)))))
	       (let ((scheduled (org-element-property :scheduled planning)))
		 (when scheduled
		   (concat org-scheduled-string " "
			   (org-translate-time
			    (org-element-property :raw-value scheduled)))))))
   " "))


;;;; Property Drawer

(defun org-rst-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER element from Org to reStructuredText.
CONTENTS holds the contents of the drawer.  INFO is a plist
holding contextual information."
  (when (org-string-nw-p contents)
	(concat
	 "::\n\n"
	 (org-rst--indent-string contents org-rst-quote-margin))))


;;;; Quote Block

(defun org-rst-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to reStructuredText.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((attributes
		  (org-export-read-attribute :attr_rst quote-block))
		 (directive (plist-get attributes :directive))
		 (title (plist-get attributes :title))
		 (subtitle (plist-get attributes :subtitle))
		 (margin (plist-get attributes :margin))
		 (class (plist-get attributes :class))
		 (label (org-element-property :name quote-block)))
	(cond ((and margin contents)
		   (org-rst--indent-string contents (string-to-number margin)))
		  (directive
		   (concat
			(format ".. %s::" directive)
			(when title (format " %s" title))
			"\n"
			(when (and subtitle (string= "sidebar" directive))
			  (format "    :subtitle: %s\n" subtitle))
			(when (and class (not (string= "container" directive)))
			  (format "    :class: %s\n" class))
			(when label (format "    :name: %s\n"
								(org-export-solidify-link-text label)))
			"\n"
			(when contents
			  (org-rst--indent-string contents org-rst-quote-margin))))
		  (t
		   (concat
			(format "::\n")
			(when class (format "    :class: %s\n" class))
			(when label (format "    :name: %s\n"
								(org-export-solidify-link-text label)))
			"\n"
			(when contents
			  (org-rst--indent-string contents org-rst-quote-margin)))))))


;;;; Radio Target

(defun org-rst-radio-target (radio-target contents info)
  "Transcode a RADIO-TARGET object from Org to reStructuredText.
CONTENTS is the contents of the target.  INFO is a plist holding
contextual information."
  contents)


;;;; Section

(defun org-rst-section (section contents info)
  "Transcode a SECTION element from Org to reStructuredText.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  contents)


;;;; Special Block

(defun org-rst-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to reStructuredText.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  contents)


;;;; Src Block

(defun org-rst-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to reStructuredText.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (when (org-string-nw-p (org-element-property :value src-block))
    (let* ((lang (org-element-property :language src-block))
		   (caption (org-element-property :caption src-block))
		   (label (org-element-property :name src-block))
		   (value (org-remove-indentation
				   (org-element-property :value src-block)))
		   (num-start (case (org-element-property :number-lines src-block)
						(continued (org-export-get-loc src-block info))
						(new 1)))
		   (retain-labels (org-element-property :retain-labels src-block))
		   (attributes
			(org-export-read-attribute :attr_rst src-block))
		   (class (plist-get attributes :class)))
      (cond
       ;; Case 1.  No source fontification.
       ((not org-rst-pygments)
		(let ()
		  (concat
		   "::\n"
		   (when class (format "    :class: %s\n" class))
		   (when label (format "    :name: %s\n"
							   (org-export-solidify-link-text label)))
		   "\n"
		   (org-rst--indent-string value org-rst-quote-margin))))
	   ;; Case 2. pygments.
	   (t
		(let ((lst-lang
			   (or (cadr (assq (intern lang) org-rst-pygments-langs)) lang)))
		  (concat
		   (format ".. code-block:: %s\n" lst-lang)
		   (when num-start (format "    :number-lines: %s\n" num-start))
		   (when class (format "    :class: %s\n" class))
		   (when label (format "    :name: %s\n"
							   (org-export-solidify-link-text label)))
		   "\n"
		   (org-rst--indent-string value org-rst-quote-margin)
		  )))))))


;;;; Statistics Cookie

(defun org-rst-statistics-cookie (statistics-cookie contents info)
  "Transcode a STATISTICS-COOKIE object from Org to reStructuredText.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value statistics-cookie))


;;;; Strike-Through

(defun org-rst-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to reStructuredText.
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  contents)


;;;; Subscript

(defun org-rst-subscript (subscript contents info)
  "Transcode a SUBSCRIPT object from Org to reStructuredText.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "\\ :sub:`%s`\\ " contents))


;;;; Superscript

(defun org-rst-superscript (superscript contents info)
  "Transcode a SUPERSCRIPT object from Org to reStructuredText.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "\\ :sup:`%s`\\ " contents))


;;;; Table

(defun org-rst-table (table contents info)
  "Transcode a TABLE element from Org to reStructuredText.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (let* ((caption (org-export-get-caption table))
		 (attributes
		  (org-export-read-attribute :attr_rst table))
		 (class (plist-get attributes :class))
		 (label (org-element-property :name table)))
    (concat
     (if caption (format ".. table:: %s\n" (org-export-data caption info))
	   ".. table::\n")
	 (when class (format "    :class: %s\n" class))
	 (when label (format "    :name: %s\n"
						 (org-export-solidify-link-text label)))
	 "\n"
	 (org-rst--indent-string contents org-rst-quote-margin))))


;;;; Table Cell


(defun org-rst-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL object from Org to reStructuredText.
CONTENTS is the cell contents.  INFO is a plist used as
a communication channel."
  (let ((width (org-ascii--table-cell-width table-cell info)))
    ;; When contents are too large, truncate them.
    (unless (or org-ascii-table-widen-columns
                (<= (string-width contents) width))
      (setq contents (concat (substring contents 0 (- width 2)) "=>")))
    ;; Align contents correctly within the cell.
    (let* ((indent-tabs-mode nil)
	   (data
	    (when contents
	      (org-ascii--justify-string
	       contents width
	       (org-export-table-cell-alignment table-cell info)))))
      (setq contents (concat data
                             (make-string (- width (string-width data)) ? ))))
    ;; Return cell.
    (concat (format " %s " contents)
			(when (org-export-get-next-element table-cell info) "|"))))


;;;; Table Row

(defun org-rst-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to reStructuredText.
CONTENTS is the row contents.  INFO is a plist used as
a communication channel."
  (let ((borders (org-export-table-cell-borders
				  (org-element-map table-row 'table-cell 'identity info t)
				  info)))
	(if (not (and (memq 'bottom borders) (memq 'top borders)))
	  (let ((hline
			 (replace-regexp-in-string "|" "+"
									   (replace-regexp-in-string
										"[^|]" "-" contents))))
		(concat
		(when (and (memq 'top borders) (memq 'above borders))
		  (concat "+" hline "+\n"))
		"|" contents "|"
		(when (memq 'below borders)
		  (concat "\n+" hline "+"))))
	  nil
	  )))


;;;; Target

(defun org-rst-target (target contents info)
  "Transcode a TARGET object from Org to reStructuredText.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "`%s`"
	  (org-export-solidify-link-text (org-element-property :value target))))


;;;; Timestamp

(defun org-rst-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to reStructuredText.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (org-rst-plain-text (org-timestamp-translate timestamp) info))


;;;; Underline

(defun org-rst-underline (underline contents info)
  "Transcode UNDERLINE from Org to reStructuredText.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  contents)


;;;; Verbatim

(defun org-rst-verbatim (verbatim contents info)
  "Transcode a VERBATIM object from Org to reStructredText.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-rst--text-markup (org-element-property :value verbatim) 'verbatim))


;;;; Verse Block

(defun org-rst-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to reStructuredText.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  (concat
   (replace-regexp-in-string "^" "| " (if (> (string-width contents) 1)
                                          (substring contents 0 -1)
                                        contents)) "\n"))



;;; Filters

(defun org-rst-separate-elements (tree backend info)
  "Make sure elements are separated by at least one blank line.

TREE is the parse tree being exported.  BACKEND is the export
back-end used.  INFO is a plist used as a communication channel.

Assume BACKEND is `rst'."
  (org-element-map tree org-element-all-elements
    (lambda (elem)
      (unless (or (eq (org-element-type elem) 'org-data)
				  (eq (org-element-type elem) 'table-row))
		(org-element-put-property
		 elem :post-blank
		 (let ((post-blank (org-element-property :post-blank elem)))
		   (if (not post-blank) 1 (max 1 post-blank)))))))
  ;; Return updated tree.
  tree)

(defun org-rst-filter-headline-blank-lines (headline back-end info)
  "Filter controlling number of blank lines after a headline.

HEADLINE is a string representing a transcoded headline.
BACK-END is symbol specifying back-end used for export.  INFO is
plist containing the communication channel.

This function only applies to `rst' back-end.  See
`org-rst-headline-spacing' for information."
  (if (not org-rst-headline-spacing) headline
    (let ((blanks (make-string (1+ (cdr org-rst-headline-spacing)) ?\n)))
      (replace-regexp-in-string "\n\\(?:\n[ \t]*\\)*\\'" blanks headline))))

(defun org-rst-filter-paragraph-spacing (tree back-end info)
  "Filter controlling number of blank lines between paragraphs.

TREE is the parse tree.  BACK-END is the symbol specifying
back-end used for export.  INFO is a plist used as
a communication channel.

See `org-rst-paragraph-spacing' for information."
  (when (wholenump org-rst-paragraph-spacing)
    (org-element-map tree 'paragraph
      (lambda (p)
		(when (eq (org-element-type (org-export-get-next-element p info))
				  'paragraph)
		  (org-element-put-property
		   p :post-blank org-rst-paragraph-spacing)))))
  tree)

(defun org-rst-filter-comment-spacing (tree backend info)
  "Filter removing blank lines between comments.
TREE is the parse tree.  BACK-END is the symbol specifying
back-end used for export.  INFO is a plist used as
a communication channel."
  (org-element-map tree '(comment comment-block)
    (lambda (c)
      (when (memq (org-element-type (org-export-get-next-element c info))
				  '(comment comment-block))
		(org-element-put-property c :post-blank 0))))
  tree)



;;; End-user functions

;;;###autoload
(defun org-rst-export-as-rst
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a reStructuredText buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org RST Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'rst "*Org RST Export*"
    async subtreep visible-only body-only ext-plist (lambda () (rst-mode))))

;;;###autoload
(defun org-rst-convert-region-to-rst ()
  "Assume the current region has org-mode syntax, and convert it to
reStructuredText.
This can be used in any buffer.  For example, you can write an
itemized list in org-mode syntax in a Markdown buffer and use
this command to convert it."
  (interactive)
  (org-export-replace-region-by 'rst))


;;;###autoload
(defun org-rst-export-to-rst
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a reStructuredText file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".rst" subtreep)))
    (org-export-to-file 'rst outfile
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-rst-publish-to-rst (plist filename pub-dir)
  "Publish an org file to reStructuredText.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'rst filename ".rst" plist pub-dir))


;;; provide

(provide 'ox-rst)

;;; ox-rst.el ends here
