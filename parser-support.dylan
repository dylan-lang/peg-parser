module: peg-parser
synopsis: The <token> and <parse-context> classes and misc. exports and support.


/**
SYNOPSIS: Parsing context.

If part of a stream is parsed once using a given context state and the results
cached, and then the context state is updated and the stream re-parsed again,
the result will be other than expected because the cached result assumes the
same context state. This may be avoided by removing the cached result using
'invalidate-parser-cache'.
**/

define open class <parse-context> (<object>)
   // 'Req-next' and 'not-next' increment this and decrement when done.
   slot lookahead-depth :: <integer> = 0;
   // Each element is a stream position and may be #f or a cached result table.
   slot cache :: <vector>;

   /// A table, keyed by production name, containing the number of cache hits.
   constant slot parser-cache-hits = make(<table>);

   /// If supplied, the cache is preallocated, otherwise it is created and grown
   /// as needed.
   keyword #"cache-stream", type: <positionable-stream>;
end class;

define method initialize (obj :: <parse-context>, #key cache-stream = #f) => ()
   next-method();
   obj.cache :=
         if (cache-stream)
            // Can't use stream-size because it isn't defined for several
            // common streams.
            let pos = cache-stream.stream-position;
            let stream-size = adjust-stream-position(cache-stream, 0, from: #"end");
            cache-stream.stream-position := pos;
            // The +1 is for the parse result at the end-of-stream position.
            let cache-size = as(<integer>, stream-size) + 1;
            make(<vector>, size: cache-size)
         else
            make(<stretchy-vector>)
         end if;
end method;


/**
SYNOPSIS: Determine whether cache hits are tracked.

The only parse results worth caching are those with the most hits. Make all
parsers cached, then run through a few sample files and take a look at the
results to decide which ones to keep cached.
**/

define variable *parser-cache-hits* :: <boolean> = #f;


/**
SYNOPSIS: Clears parsing cache.

Call this after a parse-affecting context or attribute change to remove
invalid cached parse results.
**/

define method invalidate-parser-cache
   (context :: <parse-context>, #key from :: <integer> = 0)
=> ()
   unless (context.cache.size < from)
      fill!(context.cache, #f, start: from)
   end unless
end method;


// SYNOPSIS: Element of parse results cache.
define class <parse-result> (<object>)
   constant slot semantic-value :: <object>, required-init-keyword: #"value";
   constant slot success-pos :: false-or(type-union(<integer>, <stream-position>)),
      required-init-keyword: #"success-pos";
   constant slot parse-failure :: false-or(<parse-failure>),
      required-init-keyword: #"failure";
end class;


/**
SYNOPSIS: Indicates whether parser is currently evaluating 'req-next' or
'not-next'.
**/

define inline function look-ahead? (context :: <parse-context>)
=> (look-ahead? :: <boolean>)
   context.lookahead-depth > 0
end function;


/**
SYNOPSIS: Indicates why a parser did not parse further than it did.

See 'rule parser' for more information. This is a subclass of <warning>, but
is not signaled by the PEG parser library.
**/

define class <parse-failure> (<warning>)
   /// A list of <string>. Descriptions of what parser expected at
   /// 'failure-position'.
   slot parse-expected-list :: <list> = #(),
      init-keyword: #"expected-list";
   /// As 'parse-expected-list', but what the parser did not expect at
   /// 'failure-position'.
   slot parse-expected-other-than-list :: <list> = #(),
      init-keyword: #"expected-other-than-list";
   /// An instance of <integer> or <stream-position>.
   slot failure-position
      :: false-or(type-union(<integer>, <stream-position>)) = #f,
      init-keyword: #"position";
   keyword #"expected", type: <string>;
   keyword #"expected-other-than", type: <string>;
end class;

define method initialize
   (obj :: <parse-failure>,
    #key expected :: false-or(<string>) = #f,
         expected-other-than :: false-or(<string>) = #f,
         expected-list, expected-other-than-list)
=> ()
   next-method();
   if (expected)
      obj.parse-expected-list :=
            add-new!(obj.parse-expected-list, expected, test: \=);
   end if;
   if (expected-other-than)
      obj.parse-expected-other-than-list :=
            add-new!(obj.parse-expected-other-than-list, expected-other-than,
                     test: \=);
   end if;
end method;

define method empty-description? (err :: <parse-failure>) => (empty? :: <boolean>)
   err.parse-expected-list.empty? & err.parse-expected-other-than-list.empty?
end method;


/**
SYNOPSIS: Printable version of 'parse-expected-list' and
'parse-expected-other-than-list'.
**/

define method parse-expected (err :: <parse-failure>) => (string :: <string>)
   let expected-list = err.parse-expected-list.shallow-copy;
   for (other-than-desc in err.parse-expected-other-than-list)
      expected-list := remove!(expected-list, other-than-desc, test: \=)
   end for;

   let exp = "";
   unless (expected-list.empty?)
      exp := reduce(method (a, b) concatenate(a, " or ", b) end,
                    expected-list.first,
                    expected-list.tail);
   end unless;

   let unexp = "";
   unless (err.parse-expected-other-than-list.empty?)
      unexp := reduce(method (a, b) concatenate(a, " or ", b) end,
                      err.parse-expected-other-than-list.first,
                      err.parse-expected-other-than-list.tail);
   end unless;
   
   case
      ~exp.empty? & ~unexp.empty? =>
         concatenate(exp, " and not ", unexp);
      exp.empty? & ~unexp.empty? =>
         concatenate("other than ", unexp);
      exp.empty? & unexp.empty? =>
         "?";
      otherwise =>
         exp;
   end case;
end method;


define method condition-to-string (err :: <parse-failure>) => (string :: <string>)
   format-to-string("Parse failure at stream pos #x%x: expected %s",
         err.failure-position, err.parse-expected)
end method;


/**
SYNOPSIS: Merges two <parse-failure>s into one. The rightmost of the two is
assumed to be the more relevant.

If you write a manual parser that calls other parser functions, use this
function to combine all the <parse-failure>s returned by those functions.
**/

define method combine-errors
   (a :: false-or(<parse-failure>), b :: false-or(<parse-failure>))
=> (combined-error :: false-or(<parse-failure>))
   case
      ~a => b;
      ~b => a;
      a.failure-position > b.failure-position => a;
      b.failure-position > a.failure-position => b;
      otherwise =>
         // Combine them, removing duplicates.
         for (exp in b.parse-expected-list)
            a.parse-expected-list :=
                  add-new!(a.parse-expected-list, exp, test: \=);
         end for;
         for (exp in b.parse-expected-other-than-list)
            a.parse-expected-other-than-list :=
                  add-new!(a.parse-expected-other-than-list, exp, test: \=);
         end for;
         a;
   end case;
end method;


/**
SYNOPSIS: A token, a class containing information parsed from a stream.

Subclasses of <token> are created by 'parser-definer', but tokens do not
'need' [em] to be of this class. Any object may be returned by a 'rule
parser', but the rule parsers created by 'seq', 'choice', etc. use #f to
indicate an optional item not present or a rule that doesn't consume any
characters (i.e. all look-ahead rules).
**/

define open abstract class <token> (<object>)
   slot parse-start :: type-union(<integer>, <stream-position>),
      required-init-keyword: #"start";
   slot parse-end :: type-union(<integer>, <stream-position>),
      required-init-keyword: #"end";
end class;


/**
SYNOPSIS: Control of parser debugging output.

Set to #f or an output stream. As each defined parser processes its rules, it
will print a trace.
**/

define variable *parser-trace* :: false-or(<stream>) = #f;


// SYNOPSIS: Indents and prints a line, as a replacement for <indented-stream>
// which is not implemented in Open Dylan.
define inline function format-trace(format-string :: <string>, #rest params) => ()
   when (*parser-trace*)
      write(*parser-trace*, make(<string>, size: *indent-level*));
      apply(format, *parser-trace*, format-string, params);
      write(*parser-trace*, "\n");
   end when;
end function;

define variable *indent-level* = 0;

define inline function indent-trace () => ()
   when (*parser-trace*)
      *indent-level* := *indent-level* + 2;
   end when;
end function;

define inline function outdent-trace () => ()
   when (*parser-trace*)
      *indent-level* := *indent-level* - 2;
      if (*indent-level* < 0) *indent-level* := 0 end;
   end when;
end function;


/**
SYNOPSIS: Combines common elements of a set of sequences into a new sequence.

: collect-subelements(#[#[0, 1, 2], #["red", "blue", "green"]], 1)

returns

: #[1, "blue"]

ARGUMENTS:
   sequences   - A collection of <sequence>, or #f.
   index       - An <integer>. The element of each of 'sequences' that should
                 be pulled out into a new sequence.
   default:    - An <object>. If the sequence doesn't have an element at
                 'index', this value is used instead. Defaults to #f.
VALUES:
   new-sequence - The resulting sequence. It is empty if 'sequences' is #f.
**/

define function collect-subelements
   (sequences :: false-or(<collection>), index :: <integer>, #key default = #f)
=> (new-sequence :: <sequence>)
   if (sequences)
      map-as(<deque>,
             method (sequence :: <sequence>) => (item :: <object>)
                element(sequence, index, default: default)
             end,
             sequences)
   else
      #()
   end if
end function;
