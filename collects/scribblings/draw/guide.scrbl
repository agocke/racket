#lang scribble/doc
@(require scribble/manual
          "common.ss"
          scribble/eval
          scribble/racket
          racket/runtime-path
          (for-syntax racket/base)
          (for-label racket/math))

@(define draw-eval (make-base-eval))
@interaction-eval[#:eval draw-eval (require racket/class
                                            racket/draw)]
@interaction-eval[#:eval draw-eval (define (copy-bitmap bm0)
                                     (let ([w (send bm0 get-width)]
                                           [h (send bm0 get-height)])
                                       (let ([bm (make-bitmap w h)])
                                         (let ([dc (make-object bitmap-dc% bm)])
                                           (send dc draw-bitmap bm0 0 0)
                                           (send dc set-bitmap #f))
                                          bm)))]
@interaction-eval[#:eval draw-eval (define (line-bitmap mode)
                                      (let* ([bm (make-bitmap 30 4)]
                                            [dc (make-object bitmap-dc% bm)])
                                        (send dc set-smoothing mode)
                                        (send dc draw-line 0 2 30 2)
                                       (send dc set-bitmap #f)
                                       bm))]
@interaction-eval[#:eval draw-eval (define (path-bitmap zee join brush?)
                                     (let* ([bm (make-bitmap 40 40)]
                                            [dc (new bitmap-dc% [bitmap bm])])
                                       (send dc set-smoothing 'aligned)
                                       (send dc set-pen (new pen% [width 5] [join join]))
                                       (if brush?
                                           (send dc set-brush blue-brush)
                                           (send dc set-brush "white" 'transparent))
                                       (send dc draw-path zee 5 5)
                                       (send dc set-bitmap #f)
                                       bm))]

@(define-syntax-rule (define-linked-method name interface)
   (define-syntax name 
     (make-element-id-transformer
      (lambda (stx)
        #'(method interface name)))))
@(define-linked-method draw-line dc<%>)
@(define-linked-method draw-rectangle dc<%>)
@(define-linked-method set-pen dc<%>)
@(define-linked-method set-font dc<%>)
@(define-linked-method set-clipping-region dc<%>)
@(define-linked-method set-alpha dc<%>)
@(define-linked-method get-pen dc<%>)
@(define-linked-method set-brush dc<%>)
@(define-linked-method get-brush dc<%>)
@(define-linked-method set-smoothing dc<%>)
@(define-linked-method draw-path dc<%>)
@(define-linked-method draw-ellipse dc<%>)
@(define-linked-method draw-text dc<%>)
@(define-linked-method draw-bitmap dc<%>)
@(define-linked-method get-text-extent dc<%>)
@(define-linked-method set-text-foreground dc<%>)
@(define-linked-method draw-arc dc<%>)
@(define-linked-method erase dc<%>)
@(define-linked-method set-stipple brush%)
@(define-linked-method line-to dc-path%)
@(define-linked-method curve-to dc-path%)
@(define-linked-method move-to dc-path%)
@(define-linked-method append dc-path%)
@(define-linked-method arc dc-path%)
@(define-linked-method reverse dc-path%)
@(define-linked-method ellipse dc-path%)
@(define-linked-method translate dc<%>)
@(define-linked-method scale dc<%>)
@(define-linked-method rotate dc<%>)
@(define-linked-method set-path region%)

@title[#:tag "overview"]{Overview}

The @racketmodname[racket/draw] library provides a drawing API that is
based on the PostScript drawing model. It supports line drawing, shape
filling, bitmap copying, alpha blending, and affine transformations
(i.e., scale, rotation, and translation).

@margin-note{See @secref["classes" #:doc '(lib
"scribblings/guide/guide.scrbl")] for an introduction to classes and
interfaces in Racket.}

Drawing with @racketmodname[racket/draw] requires a @deftech{drawing context}
(@deftech{DC}), which is an instance of the @scheme[dc<%>]
interface. For example, the @racket[post-script-dc%] class implements
a @racket[dc<%>] for drawing to a PostScript file, while @racket[bitmap-dc%] 
draws to a bitmap. When using the @racketmodname[racket/gui] library for GUIs,
the @method[canvas<%> get-dc] method of a
canvas returns a @scheme[dc<%>] instance for drawing into the canvas
window.

@margin-note{See @secref["canvas-drawing" #:doc '(lib
"scribblings/gui/gui.scrbl")] for an introduction to drawing 
in a GUI window.}

@; ------------------------------------------------------------
@section{Lines and Simple Shapes}

To draw into a bitmap, first create the bitmap with
@racket[make-bitmap], and then create a @racket[bitmap-dc%] that draws
into the new bitmap:

@racketblock+eval[
#:eval draw-eval
(define target (make-bitmap 30 30)) (code:comment "A 30x30 bitmap")
(define dc (new bitmap-dc% [bitmap target]))
]

Then, use methods like @method[dc<%> draw-line] on the @tech{DC} to draw
into the bitmap. For example, the sequence

@racketblock+eval[
#:eval draw-eval
(send dc draw-rectangle 
      0 10   (code:comment @#,t{Top-left at (0, 10), 10 pixels down from top-left})
      30 10) (code:comment @#,t{30 pixels wide and 10 pixels high})
(send dc draw-line 
      0 0    (code:comment @#,t{Start at (0, 0), the top-left corner})
      30 30) (code:comment @#,t{and draw to (30, 30), the bottom-right corner})
(send dc draw-line 
      0 30   (code:comment @#,t{Start at (0, 30), the bottom-left corner})
      30 0)  (code:comment @#,t{and draw to (30, 0), the top-right corner})
]

draws an ``X'' on top of a smaller rectangle into the bitmap @racket[target]. If
you save the bitmap to a file with @racket[(send target #,(:: bitmap% save-file)
"box.png" 'png)], the @filepath{box.png} contains the image

@centered{@interaction-eval-show[#:eval draw-eval (copy-bitmap target)]}

in PNG format.

A line-drawing drawing operation like @racket[draw-line] uses the
@tech{DC}'s current @defterm{pen} to draw the line. A pen has a color,
line width, and style, where pen styles include @racket['solid],
@racket['long-dash], and @racket['transparent]. Enclosed-shape
operations like @racket[draw-rectangle] use both the current pen and
the @tech{DC}'s current @deftech{brush}. A brush has a color and style,
where brush styles include @racket['solid], @racket['cross-hatch], and
@racket['transparent].

@margin-note{In DrRacket, instead of saving @racket[target] to a file
viewing the image from the file, you can use @racket[(require
racket/gui)] and @racket[(make-object image-snip% target)] to view the
bitmap in the DrRacket interactions window.}

For example, set the brush and pen before the drawing operations to
draw a thick, red ``X'' on a green rectangle with a thin, blue border:

@racketblock+eval[
#:eval draw-eval
(send dc set-brush "green" 'solid)
(send dc set-pen "blue" 1 'solid)
(send dc draw-rectangle 0 10 30 10)
(send dc set-pen "red" 3 'solid)
(send dc draw-line 0 0 30 30)
(send dc draw-line 0 30 30 0)
]

@centered{@interaction-eval-show[#:eval draw-eval (copy-bitmap target)]}

To draw a filled shape without an outline, set the pen to
@racket['transparent] mode (with any color and line width). For
example,

@racketblock+eval[
#:eval draw-eval
(send dc set-pen "white" 1 'transparent)
(send dc set-brush "black" 'solid)
(send dc draw-ellipse 5 5 20 20)
]

@centered{@interaction-eval-show[#:eval draw-eval (copy-bitmap target)]}

By default, a @racket[bitmap-dc%] draws solid pixels without smoothing
the boundaries of shapes. To enable smoothing, set the
smoothing mode to either @racket['smoothed] or @racket['aligned]:

@racketblock+eval[
#:eval draw-eval
(send dc set-smoothing 'aligned)
(send dc set-brush "black" 'solid)
(send dc draw-ellipse 4 4 22 22) (code:comment @#,t{a little bigger})
]

@centered{@interaction-eval-show[#:eval draw-eval (copy-bitmap target)]}

The difference between @racket['aligned] mode and @racket['smoothed]
mode is related to the relatively coarse granularity of pixels in a
bitmap. Conceptually, drawing coordinates correspond to the lines
between pixels, and the pen is centered on the line. In
@racket['smoothed] mode, drawing on a line causes the pen to draw at
half strength on either side of the line, which produces the following
result for a 1-pixel black pen:

@centered[@interaction-eval-show[#:eval draw-eval (line-bitmap 'smoothed)]]

but @racket['aligned] mode shifts drawing coordinates to make the pen
fall on whole pixels, so a 1-pixel black pen draws a single line of
pixels:

@centered[@interaction-eval-show[#:eval draw-eval (line-bitmap 'aligned)]]

@; ------------------------------------------------------------
@section{Pen, Brush, and Color Objects}

The @racket[set-pen] and @racket[set-brush] methods of a @tech{DC}
 accept @scheme[pen%] and @scheme[brush%] objects, which group
 together pen and brush settings.

@schemeblock+eval[
#:eval draw-eval
(require racket/math)

(define no-pen (new pen% [style 'transparent]))
(define no-brush (new brush% [style 'transparent]))
(define blue-brush (new brush% [color "blue"]))
(define yellow-brush (new brush% [color "yellow"]))
(define red-pen (new pen% [color "red"] [width 2]))

(define (draw-face dc) 
  (send dc set-smoothing 'aligned)

  (send dc set-pen no-pen) 
  (send dc set-brush blue-brush) 
  (send dc draw-ellipse 25 25 100 100) 

  (send dc set-brush yellow-brush) 
  (send dc draw-rectangle 50 50 10 10) 
  (send dc draw-rectangle 90 50 10 10) 

  (send dc set-brush no-brush) 
  (send dc set-pen red-pen) 
  (send dc draw-arc 37 37 75 75 (* 5/4 pi) (* 7/4 pi)))

(define target (make-bitmap 150 150))
(define dc (new bitmap-dc% [bitmap target]))

(draw-face dc)
]

@centered{@interaction-eval-show[#:eval draw-eval (copy-bitmap target)]}

The @racket[get-pen] and @racket[get-brush] methods return a
@tech{DC}'s current pen and brush, so they can be restored after
changing them temporarily for drawing.

Besides grouping settings, a @racket[pen%] or @racket[brush%] object
includes extra settings that are not available by using
@racket[set-pen] or @racket[set-brush] directly. For example, a pen or
brush can have a @deftech{stipple}, which is a bitmap that is used
instead of a solid color when drawing. For example, if
@filepath{water.png} has the image

@;{We can't just use the runtime path for "water.png" because we need to 
make the eval below work. }
@(define-runtime-path here ".")
@(define-runtime-path water "water.png")
@(draw-eval `(current-directory ,here))

@centered{@image[water]}

then it can be loaded with @racket[read-bitmap] and installed as the
stipple for @racket[blue-brush]:

@schemeblock+eval[
#:eval draw-eval
(send blue-brush set-stipple (read-bitmap "water.png"))
(send dc erase)
(draw-face dc)
]

@centered{@interaction-eval-show[#:eval draw-eval (copy-bitmap target)]}

Along similar lines, a @racket[color%] object lets you specify a color
through its red, green, and blue components instead of a built-in
color name. Due to the way that @racket[color%] initialization is
overloaded, use @racket[make-object%] instead of @racket[new] to
instantiate @racket[color%]:

@schemeblock+eval[
#:eval draw-eval
(define red-pen 
  (new pen% [color (make-object color% 200 100 150)] [width 2]))
(send dc erase)
(draw-face dc)
]

@centered{@interaction-eval-show[#:eval draw-eval (copy-bitmap target)]}


@; ------------------------------------------------------------
@section{Transformations}

Any coordinates or lengths supplied to drawing commends are
transformed by a @tech{DC}'s current transformation matrix.  The
transformation matrix can scale an image, draw it at an offset, or
rotate all drawing. The transformation can be set directly, or the
current transformation can be transformed further with methods like
@racket[scale], @racket[translate], or @racket[rotate]:

@schemeblock+eval[
#:eval draw-eval
(send dc erase)
(send dc scale 0.5 0.5)
(draw-face dc)
(send dc rotate (/ pi 2))
(send dc translate 0 150)
(draw-face dc)
(send dc translate 0 -150)
(send dc rotate (/ pi 2))
(send dc translate 150 150)
(draw-face dc)
(send dc translate -150 -150)
(send dc rotate (/ pi 2))
(send dc translate 150 0)
(draw-face dc)
]

Use the @method[dc<%> get-transformation] method to get a @tech{DC}'s
current transformation, and restore a saved transformation (or any
affine transformation) using @method[dc<%> set-transformation].

@centered{@interaction-eval-show[#:eval draw-eval (copy-bitmap target)]}

@; ------------------------------------------------------------
@section{Drawing Paths}

Drawing functions like @racket[draw-line] and @racket[draw-rectangle]
 are actually convenience functions for the more general
 @racket[draw-path] operation. The @racket[draw-path] operation takes
 a @deftech{path}, which describes a set of line segments and curves
 to draw with the pen and---in the case of closed set of lines and
 curves---fill with the current brush.

An instance of @racket[dc-path%] holds a path. Conceptually, a path
 has a current pen position that is manipulated by methods like
 @racket[move-to], @racket[line-to], and @racket[curve-to]. The
 @racket[move-to] method starts a sub-path, and @racket[line-to] and
 @racket[curve-to] extend it. The @racket[close] method moves the pen
 from its current position in a straight line to its starting
 position, completing the sub-path and forming a closed path that can
 be filled with the brush. A @racket[dc-path%] object can have
 multiple closed sub-paths and one final open path, where the open
 path is drawn only with the pen.

For example,

@racketblock+eval[
#:eval draw-eval
(define zee (new dc-path%))
(send zee move-to 0 0)
(send zee line-to 30 0)
(send zee line-to 0 30)
(send zee line-to 30 30)
]

creates an open path. Drawing this path with a black pen of width 5
and a transparent brush produces

@centered{@interaction-eval-show[#:eval draw-eval (path-bitmap zee 'round #f)]}

Drawing a single path with three line segments is not the same as
drawing three separate lines. When multiple line segments are drawn at
once, the corner frm one line to the next is shaped according to the
pen's join style. The image above uses the default @racket['round]
join style. With @racket['miter], line lines are joined with sharp
corners:

@centered{@interaction-eval-show[#:eval draw-eval (path-bitmap zee 'miter #f)]}

If the sub-path in @racket[zee] is closed with @racket[close], then
all of the corners are joined, including the corner at the initial
point:

@racketblock+eval[
#:eval draw-eval
(send zee close)
]

@centered{@interaction-eval-show[#:eval draw-eval (path-bitmap zee 'miter #f)]}

Using @racket[blue-brush] instead of a transparent brush causes the
interior of the path to be filled:

@centered{@interaction-eval-show[#:eval draw-eval (path-bitmap zee 'miter #t)]}

When a sub-path is not closed, it is implicitly closed for brush
filling, but left open for pen drawing. When both a pen and brush are
available (i.e., not transparent), then the brush is used first, so
that the pen draws on top of the brush.

At this point we can't resist showing an extended example using
@racket[dc-path%] to draw the Racket logo:

@racketblock+eval[
#:eval draw-eval
(define red-brush (new brush% [stipple (read-bitmap "fire.png")]))

(define left-lambda-path
  (let ([p (new dc-path%)])
    (send p move-to 153 44)
    (send p line-to 161.5 60)
    (send p curve-to 202.5 49 230 42 245 61)
    (send p curve-to 280.06 105.41 287.5 141 296.5 186)
    (send p curve-to 301.12 209.08 299.11 223.38 293.96 244)
    (send p curve-to 281.34 294.54 259.18 331.61 233.5 375)
    (send p curve-to 198.21 434.63 164.68 505.6 125.5 564)
    (send p line-to 135 572)
    p))

(define left-logo-path
  (let ([p (new dc-path%)])
    (send p append left-lambda-path)
    (send p arc 0 0 630 630 (* 235/360 2 pi) (* 121/360 2 pi) #f)
    p))

(define bottom-lambda-path 
  (let ([p (new dc-path%)])
    (send p move-to 135 572)
    (send p line-to 188.5 564)
    (send p curve-to 208.5 517 230.91 465.21 251 420)
    (send p curve-to 267 384 278.5 348 296.5 312)
    (send p curve-to 301.01 302.98 318 258 329 274)
    (send p curve-to 338.89 288.39 351 314 358 332)
    (send p curve-to 377.28 381.58 395.57 429.61 414 477)
    (send p curve-to 428 513 436.5 540 449.5 573)
    (send p line-to 465 580)
    (send p line-to 529 545)
    p))

(define bottom-logo-path
  (let ([p (new dc-path%)])
    (send p append bottom-lambda-path)
    (send p arc 0 0 630 630 (* 314/360 2 pi) (* 235/360 2 pi) #f)
    p))

(define right-lambda-path
  (let ([p (new dc-path%)])
    (send p move-to 153 44)
    (send p curve-to 192.21 30.69 233.21 14.23 275 20)
    (send p curve-to 328.6 27.4 350.23 103.08 364 151)
    (send p curve-to 378.75 202.32 400.5 244 418 294)
    (send p curve-to 446.56 375.6 494.5 456 530.5 537)
    (send p line-to 529 545)
    p))

(define right-logo-path
  (let ([p (new dc-path%)])
    (send p append right-lambda-path)
    (send p arc 0 0 630 630 (* 314/360 2 pi) (* 121/360 2 pi) #t)    
    p))

(define lambda-path
  (let ([p (new dc-path%)])
    (send p append left-lambda-path)
    (send p append bottom-lambda-path)
    (let ([t (new dc-path%)])
        (send t append right-lambda-path)
        (send t reverse)
        (send p append t))
    (send p close)
    p))

(define (paint-racket dc)
  (send dc set-pen "black" 0 'transparent)
  (send dc set-brush "white" 'solid)
  (send dc draw-path lambda-path)

  (send dc set-pen "black" 4 'solid)

  (send dc set-brush red-brush)
  (send dc draw-path left-logo-path)
  (send dc draw-path bottom-logo-path)

  (send dc set-brush blue-brush)
  (send dc draw-path right-logo-path))

(define racket-logo (make-bitmap 170 170))
(define dc (new bitmap-dc% [bitmap racket-logo]))

(send dc set-smoothing 'smoothed)
(send dc translate 5 5)
(send dc scale 0.25 0.25)
(paint-racket dc)
]

@centered{@interaction-eval-show[#:eval draw-eval racket-logo]}

In addition to the core @racket[move-to], @racket[line-to],
@racket[curve-to], and @racket[close] methods, a @racket[dc-path%]
includes many convenience methods, such as @racket[ellipse] for adding
a closed elliptical sub-path to the path.

@; ------------------------------------------------------------
@section{Text}

Draw text using the @racket[draw-text] method, which takes a string to
draw and a location for the top-left of the drawn text:

@racketblock+eval[
#:eval draw-eval
(define text-target (make-bitmap 100 30))
(define dc (new bitmap-dc% [bitmap text-target]))
(send dc set-brush "white" 'transparent)

(send dc draw-rectangle 0 0 100 30)
(send dc draw-text "Hello, World!" 5 1)
]

@centered{@interaction-eval-show[#:eval draw-eval (copy-bitmap text-target)]}

The font used to draw text is determined by the @tech{DC}'s current
font.  A font is described by a @racket[font%] object and installed
with @racket[set-font]. The color of drawn text which is separate from
either the pen or brush, can be set using
@racket[set-text-foreground].


@racketblock+eval[
#:eval draw-eval
(send dc erase)
(send dc set-font (make-object font% 14 'roman 'normal 'bold))
(send dc set-text-foreground "blue")
(send dc draw-rectangle 0 0 100 30)
(send dc draw-text "Hello, World!" 5 1)
]

@centered{@interaction-eval-show[#:eval draw-eval (copy-bitmap text-target)]}

To compute the size that will be used by drawn text, use
@racket[get-text-extent], which returns four values: the total width,
total height, difference between the baseline and total height, and
extra space (if any) above the text in a line. For example, the result
of @racket[get-text-extent] can be used to position text within the
center of a box:

@racketblock+eval[
#:eval draw-eval
(send dc erase)
(send dc draw-rectangle 0 0 100 30)
(define-values (w h d a) (send dc get-text-extent "Hello, World!"))
(send dc draw-text "Hello, World!" (/ (- 100 w) 2) (/ (- 30 h) 2))
]

@centered{@interaction-eval-show[#:eval draw-eval (copy-bitmap text-target)]}


@; ------------------------------------------------------------
@section{Alpha Channels and Alpha Blending}

When you create or @racket[erase] a bitmap, the content is
nothing. ``Nothing'' isn't the same as white; it's the absence of
drawing. For example, if you take @racket[text-target] from the
previous section and copy it onto another @tech{DC} using
@racket[draw-bitmap], then the black rectangle and blue text is
transferred, and the background is left alone:

@racketblock+eval[
#:eval draw-eval
(define new-target (make-bitmap 100 30))
(define dc (new bitmap-dc% [bitmap new-target]))
(send dc set-pen "black" 1 'transparent)
(send dc set-brush "pink" 'solid)

(send dc draw-rectangle 0 0 100 30)
(send dc draw-bitmap text-target 0 0)
]

@centered{@interaction-eval-show[#:eval draw-eval (copy-bitmap new-target)]}

The information about which pixels of a bitmap are drawn (as opposed
to ``nothing'') is the bitmap's @deftech{alpha channel}. Not all
@tech{DC}s keep an alpha channel, but bitmaps created with
@racket[make-bitmap] keep an alpha channel by default. Bitmaps loaded
with @racket[read-bitmap] preserve transparency in the image file
through the bitmap's alpha channel.

An alpha channel isn't all or nothing. When the edges text is
anti-aliased by @racket[draw-text], for example, the pixels are
partially transparent. When the pixels are transferred to another
@tech{DC}, the partially transparent pixel is blended with the target
pixel in a process called @deftech{alpha blending}. Furthermore, a
@tech{DC} has an alpha value that is applied to all drawing
operations; an alpha value of @racket[1.0] corresponds to solid
drawing, an alpha value of @racket[0.0] makes the drawing have no
effect, and values in between make the drawing translucent.

For example, setting the @tech{DC}'s alpha to @racket[0.25] before
calling @racket[draw-bitmap] causes the blue and black of the ``Hello,
World!'' bitmap to be quarter strength as it is blended with the
destination image:

@racketblock+eval[
#:eval draw-eval
(send dc erase)
(send dc draw-rectangle 0 0 100 30)
(send dc set-alpha 0.25)
(send dc draw-bitmap text-target 0 0)
]

@centered{@interaction-eval-show[#:eval draw-eval (copy-bitmap new-target)]}

@; ------------------------------------------------------------
@section{Clipping}

In addition to tempering the opacity of drawing operations, a
@tech{DC} has a @deftech{clipping region} that constrains all drawing to
inside the region. In the simplest case, a clipping region corresponds
to a closed path, but it can also be the union, intersection,
subtraction, or exclusive-or of two paths.

For example, a clipping region could be set to three circles to clip
the drawing of a rectangle (with the 0.25 alpha still in effect):

@racketblock+eval[
#:eval draw-eval
(define r (new region%))
(let ([p (new dc-path%)])
  (send p ellipse 00 0 35 30)
  (send p ellipse 35 0 30 30)
  (send p ellipse 65 0 35 30)
  (send r set-path p))
(send dc set-clipping-region r)
(send dc set-brush "green" 'solid)
(send dc draw-rectangle 0 0 100 30)
]

@centered{@interaction-eval-show[#:eval draw-eval (copy-bitmap new-target)]}

The clipping region can be viewed as a convenient alternative to path
filling or drawing with stipples. Conversely, stippled drawing can be
viewed as a convenience alternative to clipping repeated calls of
@racket[draw-bitmap].


@; ------------------------------------------------------------
@section{Portability}

Drawing effects are not completely portable across platforms or across
types of DC. For example. drawing to a bitmap produced by
@racket[make-bitmap] may produce slightly different results than
drawing to one produced by @racketmodname[racket/gui]'s
@racket[make-screen-bitmap], but drawing to a bitmap from
@racket[make-screen-bitmap] should be the same as drawing to an
onscreen @racket[canvas%]. Fonts and text, especially, can vary across
platforms and types of @tech{DC}, but so can the precise set of pixels
touched by drawing a line.
