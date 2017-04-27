#lang scribble/manual

@(require (for-label typed/racket/base
                     syntax/parse
                     ;"template.rkt"
                     ))

@(define ellipses (racket ...))

@title[#:tag "template-lib"]{Versatile parser and template library}

Keywords: grammar, parser, template.

@defform[(parse expr [pattern body …] …)]{
 Analogous to @racket[syntax-parse], except it isn't
 specialized for syntax, but rather works for arbitrary
 s-expressions, including syntax ones (denoted by 
 @racket[#'(…)] in the pattern).}

@defform[#:literals (: :: ... else struct)
         (tmpl template)
         #:grammar
         [(template variable
                    [variable : type] ;; (ann variable type)
                    ;; cons-template
                    (template . template)
                    (template :: template)
                    
                    ;; list
                    (template**)
                    ;; list*
                    template**-dotted
                    
                    ;; vector
                    #(template**)
                    (vector . template**-dotted)
                    
                    ;; hash-template: template** must expand to a list of pairs.
                    (hash . template**-dotted)   ;; TODO: how to distinguish
                    (hasheq . template**-dotted) ;; mutable and immutable?
                    (hasheqv . template**-dotted)
                    #hash([template . template])
                    #hasheq([template . template])
                    #hasheqv([template . template])
                    
                    ;; struct-template
                    (struct-id template …)
                    (struct struct-id template …)
                    #s(prefab-id template …)
                    #s(template template …) ;; Only allowed in untyped racket

                    ;; box
                    #&template
                    
                    ;; call-template
                    (~identifier args …) ;; calls (identifier args …)
                    (~ expr args …)      ;; calls (expr args …)
                    
                    ;; unquote-template
                    ,expr
                    ,@(list expr)
                    ,@(list* expr) ;; must appear in last position.
                    
                    
                    ;; template-expander
                    template-expander-id
                    (template-expander-id args …)
                    
                    ;; maybe-template (should all be template expanders
                    ;; which means the system is extensible enough to express
                    ;; these special cases).
                    (?? alt-template …)
                    (?@ . template**-dotted)
                    (??@ . template**-dotted)
                    (?if condition template template)
                    (|@if| condition template template)
                    (if@ condition template template)
                    (|@cond| [condition template] …)
                    (|@cond| [condition template] … [else template])
                    (cond@ condition template template)
                    
                    ;; like #,@(with-syntax ([meta-var #'template])
                    ;;           #'(template**))
                    (~let ([meta-var+args template])
                          . template**-dotted)
                    
                    (~sort key template ooo)
                    (~loc stxloc . template)
                    ;; Like (template . template), but discards the first and
                    ;; keeps just the second. If the first contains pattern
                    ;; variables which are repeated, this has the effect of
                    ;; repeating the second as many times as the first. Example:
                    ;; #'(vector (~each some-pattern-var '()))
                    ;; => (vector '() '() '() '() '())
                    (~each template template)
                    
                    ;; escaped
                    (ddd escaped)
                    
                    ;; 
                    
                    ;; literal
                    #t
                    #f
                    string
                    bytes
                    number
                    char
                    keyword
                    regexp
                    pregexp)

          (meta-var+args meta-var
                         (meta-var meta-arg …))
          
          (tail-template template)
          
          ;; specialize mid-sequence in repetition (diagonal-matrix-style)
          
          (variable identifier)

          (template**-dotted (template* … . template)
                             template**)
          (template** (code:line template* …)
                      (code:line template* … :: template)
                      (code:line template* … (~rest . template)))
          (template* template
                     (code:line template ooo)
                     special-cased-template)
          (special-cased-template (code:line template vardd)
                                  (code:line template ddvar))
          ;; Where var is an iterated variable.
          (vardd var..   ;; exclude the current iteration
                 var...) ;; include the current iteration
          (ddvar ..var   ;; exclude the current iteration
                 ...var) ;; include the current iteration
          
          (ooo #,ellipses ;; TODO: make it a hyperlink
               ___
               ..k ;; k positive integer
               __k ;; k positive integer
               (code:line .. expr)  ;; expr must return a positive integer
               (code:line __ expr)) ;; expr must return a positive integer
          (ddd #,ellipses)
          ]]{
 TODO: implement the versatile template library. 
 @racket[...]
 
 TODO: support for typed/racket.

 TODO: optimization feature: would it be useful if the
 expanded code could be optimized? For example, when looking
 at the output of syntax-parse, the code is far from being
 concise.
 
 The patterns for @racket[parse] should all have a way to
 create a symmetric counterpart for @racket[tmpl], which
 produces the original value. This symmetry is important
 because it allows lens-like macros, which operate on only a
 part of the data structure, leaving everything else
 intact.
 
 @racket[??] works like @racket[??] from 
 @racket[syntax/parse/experimental/template], except it
 allows any number of alternatives (including 0, to avoid
 special-casing in macros). It is more or less equivalent to
 @racket[(?? a (?? b (?? c …)))], following syntax/parse's
 semantics.
 
 @racket[?@] has the same meaning as in syntax/parse.
 
 @racket[(??@ t* …)] is a shortcut for 
 @racket[(?? (?@ t* …))]
 
 For better compatibility with at-exp, @racket[|@if|] can be
 written @racket[if@], and the same goes for 
 @racket[|@cond|] etc.
 
 TODO: what's the difference between @racket[~], 
 @racket[template-expander] and @racket[unquote]? 
 @racket[template-expander] runs at compile-time and should
 treat its arguments as syntax.
 
 Concerning unquoting, unlike @racket[racket]'s default
 behaviour in @RACKET[#'([x #,(y …)] …)], unquoting should
 not break the nesting of ellipses. How should we express
 voluntary variation of the level of nesting? @racket[~let]
 already allows expanding part of the template at some level
 and inserting it verbatim somewhere below, but it's not a
 silver bullet. One case which comes to mind is when some of
 the nested data should be mixed with less-nested data, for
 example going from 
 @racket[([10 1 2 3] [100 4 5] [1000 6])] to 
 @racket[([10 20 30] [400 500] [6000])] should be relatively
 easy to express. Maybe @racket[~let] with parameters can be
 a suitable generalized solution: 
 @RACKET[({~let ([(addx v) #,(+ x v)]) [(addx y) …]} …)]
 
 The special-cased template syntax should allow special
 treatment of the @racket[i]-th iteration in a doubly-nested
 loop: matching @racket[x] on @racket[(1 2 3 4 5)], and
 using the template @racket[(0 x.. ,(* x x) ..x 1) …] will
 produce @racket[(1 1 1 1 1)
                 (0 4 1 1 1)
                 (0 0 9 1 1)
                 (0 0 0 16 1)
                 (0 0 0 0 24)]. The pattern before 
 @racket[x..] and the pattern after @racket[..x] can expand
 to multiple items which will be spliced in by wrapping it
 with @racket[?@].}

@section{Ideas for implementation}

@subsection{Extensibility (expanders)}

Allow normal, inline-prefix, inline-postfix and inline-infix
expanders, which can bind using regular expressions. This
allows implementing exotic syntax like @racket[var..]
(postfix, operates on the pattern preceeding it), 
@racket[..var] (postfix, operates on the pattern after it),
@racket[(… escaped-pattern)] (normal, operates on the
containing s-exp)

@subsection{Customization}

For things that are likely to be customized by the user in
the whole file scope, define a grammar/custom module, used
as follows:

@racketblock[(require grammar/custom)
             (grammar/custom option …)]

The @racket[grammar/custom] macro expands to 
@racket[(require grammar/core)] followed by a bunch of 
@racket[define-syntax] which wrap the core macros, providing
them the custom options:

@racketblock[(require grammar/core)
             (define-syntax-rule (parse . rest)
               (parse/core #:global-options (option …) . rest))
             (define-syntax-rule (tmpl . rest)
               (parse/core #:global-options (option …) . rest))]

This can also be used to rename the @racket[parse] and 
@racket[tmpl] macros, if desired (for example, 
@racket[tmpl] could be renamed to @racket[quasisyntax], or
something similar).

Otherwise, @racket[grammar/custom] could just @racket[set!]
some for-syntax variable which stores the options. A second
boolean for-syntax variable could be used to check if 
@racket[grammar/custom] was called twice, and throw an error
in that case.

Or maybe we should just use units? Can they be customized in
a similar way?

The idea is to avoid having to wrap the whole file in a 
@racket[(parameterize …)], and be able to easily 
@racket[provide] a customized variation of this library:

@racketblock[(provide (customized-out grammar/custom))]

@subsection{Unsorted ideas}

@subsubsection{Global pattern constraints}

For patterns, have global constraints: @racket[(~global-or id)] binds 
@racket[id] to true if the enclosing pattern was matched at least once, and
false otherwise. Multiple occurrences of the same @racket[(~global-or id)] make
the @racket[id] true if any of the containing clauses was matched at least
once.

Inside a @racket[{~no-order}], it should be possible to impose some partial
order constraints, so that we can say:

@racketblock[
 {~no-order
  {~optional pat-a}
  {~optional pat-b}
  pat-c
  {~optional {~constrain pat-d {~after pat-a}}}}]

The above code means that @racket[pat-a], @racket[pat-b] and @racket[pat-d] are
optional (but not @racket[pat-c]), the four patterns can appear in any order,
but if @racket[pat-a] and @racket[pat-d] are both present, then @racket[pat-d]
must appear after @racket[pat-a].

Scopes: the global constraints apply within a scope. By default, there is an
implicit top-level scope, and some forms might implicitly introduce a catch-all
scope unless otherwise specified, like the implicit @racket[~demimit-cut] for 
@racket[define-syntax-class] from @racket[syntax/parse]. There could be two
kinds of scopes: unhygienic catch-all scopes which scope all "global"
constraints within, and naming scopes, which explicitly say which identifiers
they scope.

@racketblock[
 {~scope {a}
  {~vector
   {~scope {b} {~no-order {~once a} {~optional b}}}
   {~scope {b} {~no-order {~once a} {~optional b}}}}}]

The code above matches against a vector of two @racket[~no-order] lists. The 
@racket[a] pattern must appear exactly once, either in the first list or in the
second, but not in both. On the other hand, the @racket[b] pattern may appear
zero or one time in the first list, zero or one time in the second list, and may
appear in both since its constraint is scoped for each list. Although it is less
clear, the following code is semantically identical:

@racketblock[
 {~scope {a b}
  {~vector
   {~no-order {~once a} {~optional b}}
   {~scope {b} {~no-order {~once a} {~optional b}}}}}]

Since the @racket[b] in the @racket{~no-order} is bound to the enclosing 
@racket[{~scope {b} …}], it does not interact in any way with the outer scope.
The @racket[~optional] constraint on the @racket[b] in the first 
@racket[~no-order] therefore does not interact withe the @racket[~optional]
constraint in the second @racket[~no-order].

@subsubsection{Generalization of pattern/template kinds}

Nearly all patterns and templates should work equally well for regular lists and
syntax objects. It should be possible and easy enough to create new "kinds" of
data, which modify how patterns and templates work all the way through the
pattern or template tree, until it switches to a new kind. As an example, the
following pattern starts as a normal s-expr pattern, and switches to syntax in
two nodes:

@racketblock[
 {~s-expr 1 2 (buckle {~optional my} shoe)
  3 4 {~syntax (knock {~optional at the} door)}
  5 6 (pick {~optional-wrap (up _) (sticks)})
  7 8 {~syntax (lay {~optional-wrap (them _) (straight)})}}]

That pattern should match the following value:

@racketblock[
 `(1 2 (buckle shoe)
     3 4 ,#'(knock door)
     5 6 (pick (up (sticks)))
     7 8 ,#'(lay (them (straight))))]

The @racket[~syntax] indicates that the whole subtree should start matching (or
producing) syntax objects, instead of regular s-expressions. It is worht noting
that syntax objects have extra information (source location, syntax properties)
that regular s-expressions lack. One way of implementing this would be to make
the pattern directives operate on "enhanced" s-expressions. Enhanced
s-expressions are s-expressions with arbitrary kind-specific data attached to
them. The @racket[~s-expr] simply translates s-expressions into enhanced
s-expressions with an empty data attached, while @racket[~syntax] is a sort of
pre-processor which turns syntax objects into enhanced s-expressions with source
location and syntax properties attached. These "kind" pre-processors run before
the normal pattern directives are applied. Some kind-specific pattern directives
can access those properties (if they are used in within the scope of the
appropriate @racket[~kind]), so that a @racket[(~loc srcloc . pattern)] matches
@racket[pattern] and saves its source location into the variable 
@racket[srcloc].

Kinds should also be able to alter how the pattern variables are bound: 
@racket[~s-expr] simply binds (in patterns) and uses (in templates) normal
Racket variables. On the other hand, @racket[~syntax] binds and uses syntax
pattern variables, so that the bound variables are used as @racket[#'var]
instead of @racket[var].

Different pattern and template forms can specify a default kind (possibly by
simply wrapping their pattern or tempalte with the appropriate @racket[~kind]).
For example, a @racket[define/match] form would use @racket[~s-expr] by default,
whereas a @racket[define-syntax/match] would use @racket[~syntax]. The same
would apply for re-implementations of Racket's @racket[match] and 
@racket[syntax-parse].

Do the "kinds" form some sort of monad? TODO: Think about this, and try to see
if there are some monads which can be translated to pattern/template kinds
usefully.

@subsubsection{Lenses}

It should be possible to describe lenses using the patterns: you can work on
the focused part of the match, possibly access (read-only) other parts, and
return a new value. What should happen when the focused part is under an
ellipsis and has more than one match ? Implicitly execute the code n times, like
a sort of @racket[for/list]?

@subsubsection{Backtracking}

Since the parser may need to backtrack, we need to expose the backtracking
mechanism to the user in some way, so that the user can:
@itemlist[
 @item{Cut the current branch}
 @item{Perform some side-effects and undo them when backtracking (dangerous)}
 @item{Record a side-effectful lambda which is executed when the match succeeds
  or when the current branch is @racket[~commit]ted.}
 @item{Querry information about the previously failed branches}
 @item{Maybe affect the order in which non-deterministic branches are taken.
  This feature would mainly be used by optimizers.

  As a toy "just because we can" example, the backtracking mechanism should be
  configurable enough that some CSP algorithm like AC2003 can be expressed by
  the user, turning the pattern library into a CSP solver (where the CSP problem
  is expressed as a pattern over an empty object). Another toy "just because we
  can" example would be a datalog implementation built upon this library, where
  the deduction rules are expressed as patterns.

  The goal is that the parser's backtracking mechanism should be modular enough
  to allow us to implement a dead-simple unoptimized backtracker, and allow
  optimizers to be written as plug-ins. For example, an optimiazer could
  statically detect branches that can be cut due to a prior failure (e.g. if the
  two-element-list pattern @racket[(foo:id bar:number)] failed because the first
  element was not an @racket[identifier?], there's no point in trying 
  @racket[(baz:id quux:string fuzz:number)] on the same term.

  Extensive configurability of the backtracking mechanism and optimization
  features may interact badly with partial application and partial compilation,
  see below. Think it through before giving too much or too little expressivity
  to the user.}]

@subsubsection{Partial application}

It should be possible to give a partial input with holes to a pattern or
template form, and, for optimization purposes, request that the pattern or
template processes the input as much as it can (for the parser, it would
potentially open a bounded number of backtracking branches, ready to switch to
the next one if one fails), leaving an efficient "continuation".

@subsubsection{Partial compilation}

One of the drawbacks of @racketmodname[syntax/parse] is that compiling a 
@racket[syntax-parse] form takes some non-negligible time. This means that if a
macro generates another macro, and the generated macro code uses syntax-parse,
each call to the "generator" macro will be expensive. A complex macro generating
syntax which contains hundreds of uses of syntax-case will be reasonnably fast.
The same code using syntax-parse will be much slower. Since the generated uses
of @racket[syntax-parse] will all have the same "shape" with a few identifiers
etc. changing, it would be nice to be able to partially pre-expand a use of 
@racket[syntax-parse], leaving only the "holes" to be expanded. With a bottom-up
expansion mechanism there's not much to do, so we have to try hard to make the
pattern / template expander top-down as much as possible, and/or use a lazy
language (for which most things can be evaluated, leaving a continuation for the
few things that actually depend on the holes).

Although partial compilation sounds like a very interesting academic project,
it might be too difficult to get something useful out of it in practice. An
alternative, which would procude the sought performance benefits for macros
generating code which uses the pattern/template library, would be to make as
many of the concepts first-class, so that they can easily be supplied as a
parameter. Note that firs-class in this case does not necessarily mean "run-time
first-class", but possibly "compile-time first-class": we only need to be able
to pre-declare parametric templates, then use them in the code generated by a
macro. As long as the parametric templates support a form of "separate
compilation" and optimization, filling in the parameters can be handled by a
fast macro.

Some of the optimization plug-ins may however rely on a closed-world assumption
(i.e. they want to have the whole, final pattern or template, in order to
optimize it). If such an optimization plug-in is used, we may have to fall back
to the idea of using partial compilation, or simply accept that macros which
generate such code will take a while to expand.

@subsubsection{QuickCheck test generation}

It should be possible to generate random data that matches (and does not match,
too, that's a distinct problem) a pattern (unless there's a user-provided
predicate that is opaque to the library, in which case we can just ignore it and
generate instances at random, hoping that some will match and some won't).

Combined with the fact that pattern directives should be reversible into
template directives, and vica versa, it means that each directive should also
express its set of accepted values in terms of its contents. Of course, we don't
expect to be able to uniformly sample random instances, nor do we expect to be
able to support in a useful way complex patterns with lots of opaque predicates.

@subsubsection{Error messages}

@racketmodname[syntax/parse] generates good error messages, but it does not
work as well when the patterns become complex. Think this through, so that the
annotation burden is minimal, and so that users don't have to think too hard
about where to put a @racket[~describe] (I frequently had the problem with 
@racket[syntax/parse] where I wrote a @racket[~describe], but it wasn't taken
into account.

@subsection{Things to look at}

@itemlist[
 @item{@racket[math/arry], for @racket[::] and array
  broadcasting.}
 @item{Quasipatterns in @racket[match].}
 @item{The @racket[lens] library}
 @item{@url{https://github.com/racket/racket/issues/1304}
  non-linear matching (with repeated binding variables, for
  example, that should be eq? or equal?)}]
