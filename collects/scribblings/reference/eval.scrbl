#lang scribble/doc
@(require "mz.ss")

@title[#:tag "eval"]{Evaluation and Compilation}

@defparam[current-eval proc (any/c . -> . any)]{

A parameter that determines the current @deftech{evaluation handler}.
The evaluation handler is a procedure that takes a top-level form and
evaluates it, returning the resulting values. The @tech{evaluation
handler} is called by @racket[eval], @racket[eval-syntax], the default
@tech{load handler}, and @racket[read-eval-print-loop] to evaluate a
top-level form. The handler should evaluate its argument in tail
position.

The @racket[_top-level-form] provided to the handler can be a
@tech{syntax object}, a compiled form, a compiled form wrapped as a
syntax object, or an arbitrary datum.

The default handler converts an arbitrary datum to a syntax object
using @racket[datum->syntax], and then enriches its @tech{lexical
information} in the same way as @racket[eval]. (If
@racket[_top-level-form] is a syntax object, then its @tech{lexical
information} is not enriched.)  The default evaluation handler
partially expands the form to splice the body of top-level
@racket[begin] forms into the top level (see
@racket[expand-to-top-form]), and then individually compiles and
evaluates each spliced form before continuing to expand, compile, and
evaluate later forms.}


@defproc[(eval [top-level-form any/c]
               [namespace namespace? (current-namespace)])
         any]{

@guidealso["namespaces"]

Calls the current @tech{evaluation handler} to evaluate
@racket[top-level-form]. The @tech{evaluation handler} is called in
tail position with respect to the @racket[eval] call, and
@racket[parameterize]d to set @racket[current-namespace] to
@racket[namespace].

If @racket[top-level-form] is a syntax object whose datum is not a
compiled form, then its @tech{lexical information} is enriched before
it is sent to the @tech{evaluation handler}:

@itemize[

 @item{If @racket[top-level-form] is a pair whose @racket[car] is a
       symbol or identifier, and if applying
       @racket[namespace-syntax-introduce] to the
       (@racket[datum->syntax]-converted) identifier produces an
       identifier bound to @racket[module] in a @tech{phase level}
       that corresponds to @racket[namespace]'s @tech{base phase},
       then only that identifier is enriched.}

 @item{For any other @racket[top-level-form],
       @racket[namespace-syntax-introduce] is applied to the entire
       syntax object.}

]

For interactive evaluation in the style of
@racket[read-eval-print-loop] and @racket[load], wrap each expression
with @racketidfont{#%top-interaction}, which is normally bound to
@racket[#%top-interaction], before passing it to @racket[eval].}


@defproc[(eval-syntax [stx syntax?]
                      [namespace namespace? (current-namespace)])
         any]{

Like @racket[eval], except that @racket[stx] must be a syntax object,
and its lexical context is not enriched before it is passed to the
@tech{evaluation handler}.}


@defparam[current-load proc (path? (or/c symbol? #f) . -> . any)]{

A parameter that determines the current @deftech{load handler} to load
top-level forms from a file. The @tech{load handler} is called by
@racket[load], @racket[load-relative], @racket[load/cd], and the
default @tech{compiled-load handler}.

A load handler takes two arguments: a path (see
@secref["pathutils"]) and an expected module name. The expected
module name is a symbol when the call is to load a module declaration
in response to a @racket[require] (in which case the file should
contain a module declaration), or @racket[#f] for any other load.
 
The default load handler reads forms from the file in
@racket[read-syntax] mode with line-counting enabled for the file
port, unless the path has a @racket[".zo"] suffix. It also
@racket[parameterize]s each read to set @racket[read-accept-compiled],
@racket[read-accept-reader], and @racket[read-accept-lang] to
@racket[#t]. In addition, if @racket[load-on-demand-enabled] is
@racket[#t], then @racket[read-on-demand-source] is effectively set to
the @tech{cleanse}d, absolute form of @racket[path] during the
@racket[read-syntax] call. After reading a single form, the form is
passed to the current @tech{evaluation handler}, wrapping the
evaluation in a continuation prompt (see
@racket[call-with-continuation-prompt]) for the default continuation
prompt tag with handler that propagates the abort to the continuation
of the @racket[load] call.

If the second argument to the load handler is a symbol, then:

@itemize[

 @item{The @racket[read-syntax] from the file is additionally
       @racket[parameterize]d as follows (to provide consistent reading
       of module source):

       @racketblock[
       (current-readtable #f)
       (read-case-sensitive #t)
       (read-square-bracket-as-paren #t)
       (read-curly-brace-as-paren #t)
       (read-accept-box #t)
       (read-accept-compiled #t)
       (read-accept-bar-quote #t)
       (read-accept-graph #t)
       (read-decimal-as-inexact #t)
       (read-accept-dot #t)
       (read-accept-infix-dot #t)
       (read-accept-quasiquote #t)
       (read-accept-reader #t)
       (read-accept-lang #t)
       ]}

 @item{If the read result is not a @racketidfont{module} form, or if a
       second @racket[read-syntax] does not produce an end-of-file,
       then the @exnraise[exn:fail] without evaluating the form that
       was read from the file. (In previous versions, the module
       declaration was checked to match the name given as the second
       argument to the load handler, but this check is no longer
       performed.)}

 @item{The @tech{lexical information} of the initial
       @racketidfont{module} identifier is enriched with a binding for
       @racket[module], so that the form corresponds to a module
       declaration independent of the current namespace's bindings.}

]

If the second argument to the load handler is @racket[#f], then each
expression read from the file is wrapped with
@racketidfont{#%top-interaction}, which is normally bound to
@racket[#%top-interaction], before passing it to the @tech{evaluation
handler}.

The return value from the default @tech{load handler} is the value of
the last form from the loaded file, or @|void-const| if the file
contains no forms. If the given path is a relative path, then it is
resolved using the value of @racket[current-directory].}


@defproc[(load [file path-string?]) any]{

@guidealso["namespaces"]

Calls the current @tech{load handler} in tail position. The call is
@racket[parameterized] to set @racket[current-load-relative-directory]
to the directory of @racket[file], which is resolved relative to
the value of @racket[current-directory].}


@defproc[(load-relative [file path-string?]) any]{

Like @racket[load/use-compiled], but when @racket[file] is a relative
path, it is resolved using the value of
@racket[current-load-relative-directory] instead of the value of
@racket[current-directory] if the former is not @racket[#f], otherwise
@racket[current-directory] is used.}


@defproc[(load/cd [file path-string?]) any]{

Like @racket[load], but @racket[load/cd] sets both
@racket[current-directory] and
@racket[current-load-relative-directory] before calling the @tech{load
handler}.}


@defparam[current-load-extension proc (path? (or/c symbol? #f) . -> . any)]{

A parameter that determines a @deftech{extension-load handler}, which is
called by @racket[load-extension] and the default @tech{compiled-load
handler}.

An @tech{extension-load handler} takes the same arguments as a
@tech{load handler}, but the file should be a platform-specific
@deftech{dynamic extension}, typically with the file suffix
@filepath{.so} (Unix), @filepath{.dll} (Windows), or @filepath{.dylib}
(Mac OS X).  The file is loaded using internal, OS-specific
primitives. See @other-manual['(lib
"scribblings/inside/inside.scrbl")] for more information on
@tech{dynamic extensions}.}


@defproc[(load-extension [file path-string?]) any]{

Sets @racket[current-load-relative-directory] like @racket[load], and
calls the @tech{extension-load handler} in tail position.}


@defproc[(load-relative-extension [file path-string?]) any]{

Like @racket[load-exension], but resolves @racket[file] using
@racket[current-load-relative-directory] like @racket[load-relative].}


@defparam[current-load/use-compiled proc (path? (or/c symbol? #f) . -> . any)]{

A parameter that determines the current @deftech{compiled-load
handler} to load from a file that may have a compiled form. The
@tech{compiled-load handler} is called by @racket[load/use-compiled].

The protocol for a @tech{compiled-load handler} is the same as for the
@tech{load handler} (see @racket[current-load]), except that a
@tech{compiled-load handler} is expected to set
@racket[current-load-relative-directory] itself. The default
@tech{compiled-load handler}, however, checks for a @filepath{.ss}
file when the given path ends with @filepath{.rkt}, no @filepath{.rkt}
file exists, and when the handler's second argument is a symbol. In
addition, the default @tech{compiled-load handler} checks for
@filepath{.zo} (bytecode) files and @filepath{.so} (native Unix),
@filepath{.dll} (native Windows), or @filepath{.dylib} (native Mac OS
X) files.

The check for a compiled file occurs whenever the given path
@racket[_file] ends with any extension (e.g., @filepath{.rkt} or
@filepath{.scrbl}), and the check consults the subdirectories
indicated by the @racket[use-compiled-file-paths] parameter relative
to @racket[_file].  The subdirectories are checked in order. A
@filepath{.zo} version of the file (whose name is formed by passing
@racket[_file] and @racket[#".zo"] to @racket[path-add-suffix]) is
loaded if it exists directly in one of the indicated subdirectories,
or a @filepath{.so}/@filepath{.dll}/@filepath{.dylib} version of the
file is loaded if it exists within a @filepath{native} subdirectory of
a @racket[use-compiled-file-paths] directory, in an even deeper
subdirectory as named by @racket[system-library-subpath]. A compiled
file is loaded only if its modification date is not older than the
date for @racket[_file]. If both @filepath{.zo} and
@filepath{.so}/@filepath{.dll}/@filepath{.dylib} files are available,
the @filepath{.so}/@filepath{.dll}/@filepath{.dylib} file is used.  If
@racket[_file] ends with @filepath{.rkt}, no such file exists, the
handler's second argument is a symbol, and a @filepath{.ss} file
exists, then @filepath{.zo} and
@filepath{.so}/@filepath{.dll}/@filepath{.dylib} files are used only
with names based on @racket[_file] with its suffixed replaced by
@filepath{.ss}.

While a @filepath{.zo}, @filepath{.so}, @filepath{.dll}, or
@filepath{.dylib} file is loaded, the current @racket[load-relative]
directory is set to the directory of the original @racket[_file].  If
the file to be loaded has the suffix @filepath{.ss} while the
requested file has the suffix @filepath{.rkt}, then the
@racket[current-module-declare-source] parameter is set to the full
path of the loaded file, otherwise the
@racket[current-module-declare-source] parameter is set to
@racket[#f].

If the original @racket[_file] is loaded or a @filepath{.zo} variant is
loaded, the @tech{load handler} is called to load the file. If any
other kind of file is loaded, the @tech{extension-load handler} is
called.}


@defproc[(load/use-compiled [file path-string?]) any]{

Calls the current @tech{compiled-load handler} in tail position.}


@defparam[current-load-relative-directory path 
          (or/c (and/c path-string? complete-path?) #f)]{

A parameter that is set by @racket[load], @racket[load-relative],
@racket[load-extension], @racket[load-relative-extension], and the
default @tech{compiled-load handler}, and used by
@racket[load-relative], @racket[load-relative-extension], and the
default @tech{compiled-load handler}.

When a new path or string is provided as the parameter's value, it is
immediately expanded (see @secref["pathutils"]) and converted to a
path. (The directory need not exist.)}


@defparam*[use-compiled-file-paths paths (listof path-string?) (listof path?)]{

A list of relative paths, which defaults to @racket[(list
(string->path "compiled"))]. It is used by the @tech{compiled-load
handler} (see @racket[current-load/use-compiled]).}


@defproc[(read-eval-print-loop) any]{

Starts a new @deftech{REPL} using the current input, output, and error
ports. The REPL wraps each expression to evaluate with
@racketidfont{#%top-interaction}, which is normally bound to
@racket[#%top-interaction], and it wraps each evaluation with a
continuation prompt using the default continuation prompt tag and
prompt handler (see @racket[call-with-continuation-prompt]). The REPL
also wraps the read and print operations with a prompt for the default
tag whose handler ignores abort arguments and continues the loop. The
@racket[read-eval-print-loop] procedure does not return until
@racket[eof] is read, at which point it returns @|void-const|.

The @racket[read-eval-print-loop] procedure can be configured through
the @racket[current-prompt-read], @racket[current-eval], and
@racket[current-print] parameters.}


@defparam[current-prompt-read proc (-> any)]{

A parameter that determines a @deftech{prompt read handler}, which is
a procedure that takes no arguments, displays a prompt string, and
returns a top-level form to evaluate. The prompt read handler is
called by @racket[read-eval-print-loop], and after printing a prompt,
the handler typically should call the @tech{read interaction handler}
(as determined by the @racket[current-read-interaction] parameter)
with the port produced by the @tech{interaction port handler}
(as determined by the @racket[current-get-interaction-input-port] parameter).

The default prompt read handler prints @litchar{> } and returns the
result of

@racketblock[
(let ([in ((current-get-interaction-input-port))])
  ((current-read-interaction) (object-name in) in))
]}


@defparam[current-get-interaction-input-port proc (-> input-port?)]{

A parameter that determines the @deftech{interaction port handler},
which returns a port to use for @racket[read-eval-print-loop] inputs.

The default interaction port handler returns the current input port.
In addition, if that port is the initial current input port,
the initial current output and error ports are flushed.

The @racketmodname[racket/gui/base] library adjusts this parameter's
value by extending the current value. The extension wraps the result
port so that GUI events can be handled when reading from the port
blocks.}


@defparam[current-read-interaction proc (any/c input-port? -> any)]{

A parameter that determines the current @deftech{read interaction
handler}, which is procedure that takes an arbitrary value and an
input port and returns an expression read from the input port. 

The default read interaction handler accepts @racket[_src] and
@racket[_in] and returns

@racketblock[
(parameterize ([read-accept-reader #t]
               [read-accept-lang #f])
  (read-syntax _src _in))
]}


@defparam[current-print proc (any/c -> any)]{

A parameter that determines the @deftech{print handler} that is called
 by @racket[read-eval-print-loop] to print the result of an evaluation
 (and the result is ignored).

The default @tech{print handler} @racket[print]s the value to the
 current output port (as determined by the
 @racket[current-output-port] parameter) and then outputs a newline,
 except that it prints nothing when the value is @|void-const|.}


@defparam[current-compile proc (any/c boolean? . -> . compiled-expression?)]{

A parameter that determines the current @deftech{compilation handler}.
The @tech{compilation handler} is a procedure that takes a top-level form and
returns a compiled form; see see @secref["compilation-model"] for
more information on compilation.

The @tech{compilation handler} is called by @racket[compile], and
indirectly by the default @tech{evaluation handler} and the default
@tech{load handler}.

The handler's second argument is @racket[#t] if the compiled form will
be used only for immediate evaluation, or @racket[#f] if the compiled
form may be saved for later use; the default compilation handler is
optimized for the special case of immediate evaluation.

When a compiled form is written to an output port, the written form
starts with @litchar{#~}. See @secref["print-compiled"] for more
information.}


@defproc[(compile [top-level-form any/c]) compiled-expression?]{

Like @racket[eval], but calls the current @tech{compilation handler} in
tail position with @racket[top-level-form].}


@defproc[(compile-syntax [stx syntax?]) compiled-expression?]{

Like @racket[eval-syntax], but calls the current @tech{compilation
handler} in tail position with @racket[stx].}


@defproc[(compiled-expression? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a compiled form, @racket[#f]
otherwise.}


@defboolparam[compile-enforce-module-constants on?]{

A parameter that determines how a module declaration is compiled. 

When constants are enforced, and when the macro-expanded body of a
module contains no @racket[set!] assignment to a particular variable
defined within the module, then the variable is marked as constant
when the definition is evaluated. Afterward, the variable's value
cannot be assigned or undefined through @racket[module->namespace],
and it cannot be defined by redeclaring the module.

Enforcing constants allows the compiler to inline some variable
values, and it allows the native-code just-in-time compiler to
generate code that skips certain run-time checks.}


@defboolparam[compile-allow-set!-undefined allow?]{

A parameter that determines how a @racket[set!] expression is compiled
when it mutates a global variable. If the value of this parameter is a
true value, @racket[set!] expressions for global variables are
compiled so that the global variable is set even if it was not
previously defined.  Otherwise, @racket[set!] expressions for global
variables are compiled to raise the
@racket[exn:fail:contract:variable] exception if the global variable
is not defined at the time the @racket[set!] is performed.  Note that
this parameter is used when an expression is @italic{compiled}, not
when it is @italic{evaluated}.}

@defboolparam[compile-context-preservation-enabled on?]{

A parameter that determines whether compilation should avoid
function-call inlining and other optimizations that may cause
information to be lost from stack traces (as reported by
@racket[continuation-mark-set->context]). The default is @racket[#f],
which allows such optimizations.}

@defboolparam[eval-jit-enabled on?]{

A parameter that determines whether the native-code just-in-time
compiler (JIT) is enabled for code (compiled or not) that is passed to
the default evaluation handler.  The default is @racket[#t], unless
the JIT is disabled through the @Flag{j}/@DFlag{no-jit} command-line
flag to stand-alone Racket (or GRacket), or through the
@as-index{@envvar{PLTNOMZJIT}} environment variable (set to any
value).}

@defboolparam[load-on-demand-enabled on?]{

A parameter that determines whether the default @tech{load handler}
sets @racket[read-on-demand-source]. See @racket[current-load] for
more information. The default is @racket[#t], unless it is disabled
through the @Flag{d}/@DFlag{no-delay} command-line flag.}
