#lang meta/web

(require "resources.rkt" "data.rkt" "installer-pages.rkt"
         (prefix-in pre: "../stubs/pre.rkt"))

(provide render-download-page)
(define (render-download-page [release current-release] [package 'racket])
  (define version (release-version release))
  @center-div{
    @h2{Download @(package->name package)
                 v@version (@(release-date-string release))}
    @div[id: "download_panel" style: "display: none;"]{
      Platform:
      @select[id: "platform_selector"
              onchange: "selection_changed();"
              onkeypress: "selection_changed();"]{
        @(for/list ([i (in-list all-installers)]
                    #:when (and (equal? release (installer-release i))
                                (equal? package (installer-package i))))
           (installer->page i 'render-option))}
      @input[type: 'submit value: "Download" onclick: "do_jump();"]
      @|br hr|
      @div[align: "center"]{
        @(let ([links (list ((release-page release) "Release Notes")
                            @license{License}
                            all-version-pages
                            @pre:installers{Nightly installers})])
           (small (add-between links @list{ @nbsp @bull @nbsp })))}
      @hr
      @div[id: "linux_explain"
           style: '("font-size: 75%; display: none; width: 28em;"
                    " margin-top: 1ex; text-align: center;")]{
        @b{Note about the Linux installers:} if you don't see an option for
        your particular platform, try other Linux installers, starting from
        similar ones.  Very often, a build on one Linux variant will work on
        others too.}}
    @downloader-script
    @noscript{
      Installers are available for the following platforms:
      @ul{@(for/list ([i (in-list all-installers)]
                      #:when (and (equal? release (installer-release i))
                                  (equal? package (installer-package i))))
             @li{@(installer->page i 'only-platform)})}}})

(define (release-page* rel)
  (define ver (release-version rel))
  (define title @list{v@ver Release Notes})
  @page[#:file (format "v~a.html" ver) #:title title #:part-of 'download]{
    @h2{Release Announcements for Version @ver}
    @pre{@release-announcement[rel]}
  })
(define release-page
  (let ([t (make-hash)])
    (lambda (rel) (hash-ref! t rel (lambda () (release-page* rel))))))

(define all-version-pages
  (let ()
    (define (make-page rel pkg)
      (define ver   (release-version rel))
      (define file  (format "~a-v~a.html" pkg ver))
      (define title @list{Download @(package->name pkg) v@ver})
      @page[#:file file #:title title #:part-of 'download]{
        @(render-download-page rel pkg)})
    (define style
      @style/inline[type: 'text/css]{
        .version-row {
          background-color: #ffffc0;
        }
        .version-row:hover {
          background-color: #e0e0a0;
        }
        .version-row a {
          text-decoration: none;
        }
        .version-row a:hover {
          background-color: #eeee22;
        }})
    @page[#:id 'all-versions #:title "All Versions" #:part-of 'download
          #:extra-headers style]{
      @table[align: 'center cellspacing: 0 cellpadding: 4 frame: 'box
             rules: 'groups]{
        @thead{
          @tr{@td{@nbsp @strong{Version & Release Notes}}
              @(map (lambda (p) @th[align: 'center]{@(package->name p)})
                    all-packages)}}
        @(let ([sep (tr style: "height: 4px; margin: 0; padding: 0;"
                        (td) (map (lambda (_) (td)) all-packages))])
           (define (cell rel pkg)
             @td[align: 'center]{
               @nbsp @(make-page rel pkg){[download]} @nbsp})
           @tbody{
             @sep
             @(map (lambda (r)
                     @list{
                       @tr[class: 'version-row]{
                         @td{@|nbsp nbsp| @strong{Version @release-version[r]},
                             @(release-page r){@release-date-string[r]} @nbsp}
                         @(map (lambda (p) (cell r p)) all-packages)}
                       @sep})
                   all-releases)})
        @tfoot{
          @tr[class: 'version-row]{@td[align: 'center colspan: 3]{
            @pre:installers}}}}}))

(define license
  @page[#:title "Software License" #:part-of 'download]{
    @p{Racket is distributed under the
       @a[href: "http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html#SEC1"]{
         GNU Lesser General Public License (LGPL)}.
       This means that you can link parts of Racket (such as racket or gracket)
       into proprietary applications, provided that you follow the specific
       rules stated in the LGPL.  You can also modify Racket software; if you
       distribute a modified version, you must distribute it under the terms of
       the LGPL, which in particular means that you must release the source
       code for the modified Racket software.}})

(define downloader-script
  @script/inline[type: 'text/javascript]{@||
    var do_jump, selection_changed;
    (function() {
    // show the download panel, since JS is obviously working
    document.getElementById("download_panel").style.display = "block";
    //
    var selector = document.getElementById("platform_selector");
    // jump to the selected item
    do_jump = function() {
      location.href = selector[selector.selectedIndex].value;
    }
    // returns an ordering for the platform names, an array of regexps
    // note that the entries are sorted in a good order, so return an order
    // that only brings the locally desired entries to the top
    function getPlatformOrder() {
      var p = navigator.platform;
      var l = function(str) { return p.indexOf(str) != -1@";" }
      var Win      = /Windows/,
          Mac      = /Macintosh/,
          MacIntel = /Macintosh.*Intel/,
          MacPPC   = /Macintosh.*PPC/,
          Linux    = /Linux/,
          Linux64  = /Linux.*x86_64/,
          Linux32  = /Linux.*i386/,
          Unix     = /Unix/,
          Solaris  = /Solaris/;
      if (p == null) return [];
      else if (l("SunOS")) return [Solaris, Unix];
      else if (l("Win"))   return [Win];
      else if (l("Mac"))   return [(l("Intel")?MacIntel:MacPPC), Mac, Unix];
      else if (l("Linux")) {
        // also show the linux explanation if it's a linux
        document.getElementById("linux_explain").style.display = "block";
        return [(l("_64")?Linux64:Linux32), Linux, Unix];
      } else return [];
    }
    // show the linux explanation on change too (do it with a timeout so it
    // changes even when the arrow keys are used to move the selection -- since
    // then onchange is called only on blur)
    linux_expl_s = document.getElementById("linux_explain").style;
    selection_changed_timer = false;
    selection_changed = function() {
      if (selection_changed_timer) clearTimeout(selection_changed_timer);
      selection_changed_timer = setTimeout(do_selection_changed, 250);
    }
    function do_selection_changed() {
      linux_expl_s.display =
        (selector[selector.selectedIndex].text.search(/Linux/) >= 0) ?
          "block" : "none";
    }
    //
    var opts = selector.options;
    var len = opts.length;
    // get the order and a make a sorting function
    var order = getPlatformOrder();
    function getOrder(str) {
      for (var i=0@";" i<order.length@";" i++)
        if (str.search(order[i]) >= 0) return i;
      return 999;
    }
    function isBetter(opt1,opt2) {
      // sort first by the order, then by how they were placed originally
      var ord1 = getOrder(opt1[0]), ord2 = getOrder(opt2[0]);
           if (ord1 < ord2)       return -1;
      else if (ord1 > ord2)       return +1;
      else if (opt1[2] < opt2[2]) return -1;
      else if (opt1[2] > opt2[2]) return +1;
      else                        return  0;
    }
    // sort the options, need to use a temporary array
    var tmps = new Array(len);
    for (var i=0@";" i<len@";" i++)
      tmps[i]=[opts[i].text,opts[i].value,i];
    tmps.sort(isBetter);
    for (var i=0@";" i<len@";" i++) {
      opts[i].text  = tmps[i][0];
      opts[i].value = tmps[i][1];
    }
    opts.selectedIndex = 0;
    })();
    @||})
