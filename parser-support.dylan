module: peg-parser
synopsis: The <token> class and miscellaneous exports and internal support.

/// SYNOPSIS: Names of parsers.
/// DISCUSSION: This is a table of <function> to <string>. Each 'rule parser'
/// should have a corresponding <string>. This string will be used in exceptions
/// and logging. If no string is provided for a rule parser, defaults to "?".
define constant *rule-names* = make(<table>);


/// A table keyed by rule parser function and containing a format string and
/// arguments. Each argument is a parser function. 'initialize-parser' finds
/// the name of each parser argument and substitutes it into the format string.
/// The result is the name of the keying parser function.
define constant *rule-name-parts* = make(<table>);


/// SYNOPSIS: Signalled when parsing fails. Non-recoverable.
define class <parse-failure> (<warning>)
   // "expected:" is required because parent tokens add themselves to
   // parse-expected.
   slot parse-expected :: <string>, required-init-keyword: #"expected";
   slot failure-position = #f, init-keyword: #"position";
end class;


/// SYNOPSIS: A token, a class containing information parsed from a stream.
/// DISCUSSION: Subclasses of <token> are created by 'parser-definer', but
/// tokens do not 'need' [em] to be of this class. Any object may be returned
/// by a 'rule parser', but the rule parsers created by 'seq', 'choice', etc.
/// use #f to indicate an optional item not present or a rule that doesn't 
/// consume any characters (i.e. all look-ahead rules).
define open abstract class <token> (<object>)
   constant slot parse-start, required-init-keyword: #"start";
   constant slot parse-end, required-init-keyword: #"end";
end class;


/// SYNOPSIS: Builds and caches a parser name, for debugging and exceptions.
define function rule-name (rule-func :: <function>) => (name :: <string>)
   if (~member?(rule-func, *rule-names*))
      let parts = element(*rule-name-parts*, rule-func, default: #("?"));
      let format-string = parts.first;
      let rule-parameters = copy-sequence(parts, start: 1);
      *rule-names*[rule-func] :=
            apply(format-to-string, format-string, map(rule-name, rule-parameters));
   end if;
   *rule-names*[rule-func]
end function;


/// SYNOPSIS: Control of parser debugging output.
/// DISCUSSION: Set to #f or an output stream. As each defined parser processes
/// its rules, it will print a trace.
define variable *parser-trace* :: false-or(<stream>) = #f;


/// SYNOPSIS: Indents and prints a line, as a replacement for <indented-stream>
/// which is not implemented in Open Dylan.
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


/// SYNOPSIS: Combines common elements of a set of sequences into a new sequence.
/// 
/// : collect-subelements(#[#[0, 1, 2], #["red", "blue", "green"]], 1)
/// returns
/// : #[1, "blue"]
///
/// ARGUMENTS:
///   sequences   - A collection of <sequence>.
///   index       - An <integer>. The element of each of 'sequences' that should
///                 be pulled out into a new resulting sequence.
///   default:    - An <object>. If the sequence doesn't have an element at
///                 'index', this value is used instead. Defaults to #f.
/// VALUES:
///   new-sequence - The resulting sequence.
define function collect-subelements
   (sequences :: <collection>, index :: <integer>, #key default = #f)
=> (new-sequence :: <sequence>)
   map-as(<deque>,
          method (sequence :: <sequence>) => (item :: <object>)
             element(sequence, index, default: default)
          end,
          sequences)
end function;

