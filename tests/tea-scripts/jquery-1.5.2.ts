;; jQuery JavaScript Library v1.5.2
;; http://jquery.com/
;;
;; Copyright 2011, John Resig
;; Dual licensed under the MIT or GPL Version 2 licenses.
;; http://jquery.org/license
;;
;; Includes Sizzle.js
;; http://sizzlejs.com/
;; Copyright 2011, The Dojo Foundation
;; Released under the MIT, BSD, and GPL Licenses.
;;
;; Date: Thu Mar 31 15:28:23 2011 -0400

;; ported to TeaScript by Daniel King (at danking (dot ccs neu edu))

((lambda (window undefined)
   (let (; Use the correct document accordingly with window argument (sandbox)
         (document (get-field window document))
         ; Define a local copy of jQuery
         (jQuery ((lambda ()
                    ; The jQuery object is actually just the init constructor
                    ; 'enhanced'
                    (let ((jQuery (lambda (selector context)
                                    (new (send (get-field jQuery fn)
                                               init
                                               selector context rootjQuery))))
                          ; Map over jQuery in case of overwrite
                          (_jQuery (get-field window jQuery))
                          ; Map over the $ in case of overwrite
                          (_$ (get-field window $))
                          ; A central reference to the root jQuery(document)
                          (rootjQuery undefined)
                          ; A simple way to check for HTML strings or ID strings
                          ; (both of which we optimize for)
                          (quickExpr (RegExp "/^(?:[^<]*(<[\\w\\W]+>)[^>]*$|#([\\w\\-]+)$)/"))
                          ; Check if a string has a non-whitespace character in it
                          (rnotwhite (RegExp "/\\S/"))
                          ; Used for trimming whitespace
                          (trimLeft  (RegExp "/^\\s+/"))
                          (trimRight (RegExp "/\\s+$/"))
                          ; Check for digits
                          (rdigit    (RegExp "/\\d/"))
                          ; Match a standalone tag
                          (singleTag "/^<(\\w+)\\s*\\/?>(?:<\\/\\1>)?$/")
                          ; JSON RegExp
                          (rvalidchars "/^[\\],:{}\\s]*$/")
                          (rvalidescape "/\\\\(?:[\"\\\\\\/bfnrt]|u[0-9a-fA-F]{4})/g")
                          (rvalidtokens "/\"[^\"\\\\\\n\\r]*\"|true|false|null|-?\\d+(?:\\.\\d*)?(?:[eE][+\\-]?\\d+)?/g")
                          (rvalidbraces "/(?:^|:|,)(?:\\s*\\[)+/g")
                          ; Useragent RegExp
                          (rwebkit "/(webkit)[ \\/]([\\w.]+)/")
                          (ropera "/(opera)(?:.*version)?[ \\/]([\\w.]+)/")
                          (rmsie "/(msie) ([\\w.]+)/")
                          (rmozilla "/(mozilla)(?:.*? rv:([\\w.]+))?/")
                          ; Keep a UserAgent string for use with jQuery.browser
                          (userAgent (get-field navigator userAgent))
                          ; For matching the engine and the version of the browser
                          (browserMatch undefined)
                          ; The deferred used on DOM ready
                          (readyList undefined)
                          ; The ready event handler
                          (DOMContentLoaded undefined)
                          ; Save a reference to some core methods
                          (toString (get-field (get-field Object prototype)
                                               toString))
                          (hasOwn   (get-field (get-field Object prototype)
                                               hasOwnProperty))
                          (push     (get-field (get-field Array prototype)
                                               push))
                          (slice    (get-field (get-field Array prototype)
                                               slice))
                          (trim     (get-field (get-field String prototype)
                                               trim))
                          (indexOf  (get-field (get-field Array prototype)
                                               indexOf))
                          ; [[Class]] -> type pairs
                          (class2type (JSON "{}")))
                      (set-field jQuery prototype
                                 (JSON (constructor jQuery)
                                       (init (lambda (selector context rootJquery)
                                               ; Handle $(""), $(null), or $(undefined)
                                               (if (not selector)
                                                   )))))))))))))
