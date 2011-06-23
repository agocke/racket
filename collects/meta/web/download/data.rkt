#lang racket/base

(define -platform-names-
  `(;; source platforms
    ["win"  "Windows"]
    ["mac"  "Macintosh"]
    ["unix" "Unix"]
    ;; binary platforms
    ["i386-win32" "Windows x86"]
    ["x86_64-win32" "Windows x64"]
    ["(ppc|i386|x86_64)-osx-mac"
     ,(lambda (_ cpu)
        (format "Macintosh OS X (~a)"
                (if (equal? cpu "ppc") "PPC" (format "Intel ~a" cpu))))]
    ["(ppc|68k)-mac-classic" "Macintosh Classic (\\1)"]
    ["(ppc|i386)-darwin"
     ,(lambda (_ cpu)
        (format "Macintosh Darwin (~a)"
                (if (equal? cpu "ppc") "PPC" "Intel")))]
    ["i386-linux(-gcc2)?"                  "Linux i386"]
    ["i386-linux-fc([0-9]+)"               "Linux i386 (Fedora Core \\1)"]
    ["(i386|x86_64)-linux-f([0-9]+)"       "Linux \\1 (Fedora \\2)"]
    ["(i386|x86_64)-linux-debian"          "Linux \\1 (Debian Stable)"]
    ["(i386|x86_64)-linux-debian-([a-zA-Z0-9]+)" "Linux \\1 (Debian \\2)"]
    ["(i386|x86_64)-linux-ubuntu[0-9]+"    "Linux \\1 (Ubuntu \\2)"]
    ["(i386|x86_64)-linux-ubuntu-([a-z]+)" "Linux \\1 (Ubuntu \\2)"]
    ["(i386|x86_64)-linux-ubuntu.*"        "Linux \\1 (Ubuntu)"]
    ["(i386|x86_64)-freebsd"               "FreeBSD \\1"]
    ["sparc-solaris"                       "Sparc Solaris (SunOS)"]
    ["i386-kernel"                         "x86 Standalone Kernel"]
    ))

(define -file-type-names-
  '(["sh"  "Self-extracting shell script"]
    ["exe" "Windows Installer"]
    ["tgz" "Gzipped TAR Archive"]
    ["zip" "Zipped Archive"]
    ["dmg" "Disk Image"]
    ["plt" "Racket Package"]
    ["sit" "StuffIt Archive"]))

(define -mirrors-
  ;; This is a sequence of
  ;;   (location url reposnisble-name email [techincal-contact])
  '(["Main download (USA, Massachusetts, Northeastern University)"
     "http://download.racket-lang.org/installers/"
     "Eli Barzilay"
     "eli@barzilay.org"]
    ["USA, Illinois (Northwestern University)"
     "http://www.eecs.northwestern.edu/racket/"
     "Robby Findler"
     "robby@eecs.northwestern.edu"]
    ["USA, Utah (University of Utah)"
     "http://www.cs.utah.edu/plt/installers/"
     "Matthew Flatt"
     "mflatt@cs.utah.edu"]
    ["Canada, Ontario (University of Waterloo)"
     "http://mirror.csclub.uwaterloo.ca/racket/racket-installers/"
     "Systems Committee"
     "syscom@csclub.uwaterloo.ca"]
    ["Germany (Universität Tübingen)"
     "http://mirror.informatik.uni-tuebingen.de/mirror/racket/"
     "Marcus Crestani"
     "crestani@informatik.uni-tuebingen.de"]
    ["Belgium (Infogroep, Vrije Universiteit Brussel)"
     "ftp://infogroep.be/pub/racket/installers/"
     "Infogroep"
     "research@infogroep.be"]
    ["Turkey, Istanbul (Bilgi University)"
     "http://russell.cs.bilgi.edu.tr/racket-installers/"
     "Onur Gungor"
     "onurgu@cs.bilgi.edu.tr"]
    #;
    ["Austria (Vienna University of Technology)"
     "http://gd.tuwien.ac.at/languages/scheme/plt/"
     "Rudolf Ladner"
     "ladner@zid.tuwien.ac.at"]
    #; ; Scheme guy left
    ["France (Institut Pasteur)"
     "ftp://ftp.pasteur.fr/pub/computing/Scheme/plt-scheme/"
     "Marc Badouin"
     "babafou@pasteur.fr"
     "Pasteur Institute FTP ftpmain@pasteur.fr"]
    #; ; ftp down (permanently?)
    ["Mexico (Wish Computing)"
     "ftp://morpheus.wish.com.mx/pub/plt/"
     "Francisco Solsona"
     "solsona@acm.org"]
    ))

;; Used to sort packages when more then one is rendered on a page (delayed)
(define (-installer-orders-)
  `((,installer-package ,eq? (racket racket-textual))
    (,installer-binary? ,eq? (#t #f))
    (,installer-platform ,regexp-match?
     (#px"\\bwin(32)?\\b" #px"\\bmac\\b" #px"\\blinux\\b" #px""))
    (,installer-platform ,regexp-match?
     (#px"\\bi386\\b" #px"\\bx86_64\\b" #px"\\bppc\\b" #px""))))

;; ----------------------------------------------------------------------------

(provide (struct-out mirror) mirrors
         (struct-out release) (struct-out installer)
         all-installers current-release all-releases all-packages
         package->name platform->name suffix->name
         set-announcements-file!)

(require racket/list racket/file version/utils racket/runtime-path
         "release-info.rkt")

;; ----------------------------------------------------------------------------
;; Mirror information

(struct mirror (location url person email))

(define mirrors
  (map (lambda (m)
         (mirror (car m) (regexp-replace #rx"/?$" (cadr m) "/")
                 (caddr m) (cadddr m)))
       -mirrors-))

;; ----------------------------------------------------------------------------
;; Release information

(struct release (version date date-string announcement))

(define announcements #f)
(define (set-announcements-file! file)
  (set! announcements #t))

(define version->release
  (let ([t (make-hash)]
        [months '#("January" "February" "March" "April" "May" "June" "July"
                   "August" "September" "October" "November" "December")])
    (lambda (v)
      (hash-ref! t v
        (lambda ()
          (let* ([info   (get-version-tag-info v)]
                 [tagger (car info)]
                 [date   (cadr info)]
                 [announcement (caddr info)]
                 [year   (date-year date)]
                 [month  (vector-ref months (sub1 (date-month date)))])
            (release v date (format "~a ~a" month year) announcement)))))))

;; ----------------------------------------------------------------------------
;; Installer information

(define-runtime-path installers-data "installers.txt")

(struct installer
  (path     ; path to file from the installers directory
   file     ; just the file name
   release  ; the release that this installer comes from
   size     ; human-readable size string
   package  ; package kind symbol 'racket or 'racket-textual
   binary?  ; #t = binary distribution, #f = source distribution
   platform ; platform name string (generic for srcs, cpu-os for bins)
   suffix)) ; string

(define installer-rx
  (pregexp (string-append
            "^"
            "([0-9.]+[A-Z]+)"       ; size
            "\t"
            "("                     ; path
            "([0-9p.]+)"            ; version
            "/"
            "(racket(?:-textual)?)" ; package
            "/("                    ; file
            "\\4-\\3-"              ; <package>-<version>-
            "(bin|src)-"            ; binary/source
            "([^.]+)"               ; platform
            "\\."
            "([a-z]+)"              ; suffix
            "))$")))

(define (make-installer size path version package file type platform suffix)
  (installer path file (version->release version) size
             (string->symbol package) (equal? "bin" type) platform suffix))

(define (parse-installers in)
  (port-count-lines! in)
  (for/list ([line (in-lines in)] [num (in-naturals 1)])
    (apply make-installer
           (cdr (or (regexp-match installer-rx line)
                    (error 'installers "bad installer data line#~a: ~s"
                           num line))))))

(define (order->precedes order)
  (define =? (car order))
  (define l (cadr order))
  (define (num-of x)
    (let loop ([l l] [n 0])
      (cond [(null? l)
             (error 'precedes "could not find ~s in precedence list: ~s" x l)]
            [(=? (car l) x) n]
            [else (loop (cdr l) (add1 n))])))
  (lambda (x y) (< (num-of x) (num-of y))))

;; sorted by version (newest first), and then by -installer-orders-
(define all-installers
  (sort
   (call-with-input-file installers-data parse-installers)
   (let ([fns `([,(lambda (i)
                    (version->integer (release-version (installer-release i))))
                 . ,>]
                ,@(map (lambda (o) (cons (car o) (order->precedes (cdr o))))
                       (-installer-orders-)))])
     (lambda (i1 i2)
       (let loop ([fns fns])
         (if (null? fns)
           #f
           (let* ([get (caar fns)] [<? (cdar fns)] [x1 (get i1)] [x2 (get i2)])
             (or (<? x1 x2) (and (equal? x1 x2) (loop (cdr fns)))))))))))

(define all-releases ; still sorted from newest to oldest
  (remove-duplicates (map installer-release all-installers)))
(define all-packages ; also sorted
  (remove-duplicates (map installer-package all-installers)))
(define current-release (car all-releases))

(define package->name
  (let ([t (make-hasheq)])
    (lambda (package)
      (hash-ref! t package
        (lambda ()
          (string-titlecase
           (regexp-replace #rx"-" (symbol->string package) " ")))))))

(define platform-names
  (for/list ([pn (in-list -platform-names-)])
    (list (regexp (string-append "^" (car pn) "$")) (cadr pn))))

(define platform->name
  (let ([t (make-hash)])
    (lambda (platform)
      (hash-ref! t platform
        (lambda ()
          (or (for/or ([pn (in-list platform-names)])
                ;; find out if a regexp applied by checking if the result is
                ;; different (relies on regexp-replace returning the same
                ;; string when fails)
                (let ([new (regexp-replace (car pn) platform (cadr pn))])
                  (and (not (eq? new platform)) new)))
              (error 'platform->name "unrecognized platform: ~e"
                     platform)))))))

(define (suffix->name suffix)
  (cond [(assoc suffix -file-type-names-) => cadr]
        [else (error 'suffix->name "unrecognized suffix: ~e" suffix)]))
