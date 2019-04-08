/**
 * Produces the value of a block string from its parsed raw value, similar to
 * CoffeeScript's block string, Python's docstring trim or Ruby's strip_heredoc.
 *
 * This implements the GraphQL spec's BlockStringValue() static algorithm.
 * 
 * NOTE: Check https://graphql.github.io/graphql-spec/June2018/#sec-String-Value for details. 
 */
let dedent: string => string = (raw) => {
  let leadingWhitespace = (str: string) => {
    let length = String.length(str);

    let rec internal = pos => {
      switch(pos < length) {
      | true => {
        switch(String.get(str, pos)) {
        | ' ' | '\t' => internal(pos + 1)
        | _ => pos
        }
      }
      | false => pos
      }
    };

    internal(0);
  };

  let isBlank = (str: string) => {
    leadingWhitespace(str) == String.length(str);
  };

  // Expand a block string's raw value into independent lines.
  let lines = Str.split(Str.regexp("\r\n\|[\r\n]"), raw);

  /**
   * NOTE: Unlike String.split in JavaScript, 
   * Str.split ignores the occurence of delimiter at the beginning.
   * (In JavaScript, empty string('') is added.)
   * Because of that, if the raw string starts with a line break,
   * the second line is regarded as the first line. 
   * So, we add an empty line for that case. 
   */
  let lines = 
    switch(
      String.length(raw) > 1 && 
      (String.get(raw, 0) == '\n' ||
      String.get(raw, 0) == '\r')
    ) {
    | true => ["", ...lines]
    | false => lines
    }

  switch(lines) {
  | [] => ""
  | [first, ...rest] => {
    let commonIndent = 
      rest
      |> List.map(line => {
        let indent = leadingWhitespace(line);
        let length = String.length(line);

        indent < length ? indent : max_int; // To ignore empty lines
      })
      |> List.fold_left((prev, cur) => {
        prev < cur ? prev : cur
      }, max_int);

    let rest = 
      rest |> List.map(line => {
        let length = String.length(line);
        switch(length - commonIndent > 0) {
        | true => String.sub(line, commonIndent, length - commonIndent);
        | false => ""
        }
      });

    let rec removeLeadingBlankLines = lines => {
      switch(lines) {
      | [] => []
      | [first, ...rest] => {
        switch(isBlank(first)) {
        | true => removeLeadingBlankLines(rest)
        | false => lines
        }
      }
      }
    };

    let final = [first, ...rest];
    let final = removeLeadingBlankLines(List.rev(final)); // remove at the back
    let final = removeLeadingBlankLines(List.rev(final)); // remove at the front

    String.concat("\n", final);
  }
  }
};

/**
 * Print a block string in the indented block form by adding a leading and
 * trailing blank line. However, if a block string starts with whitespace and is
 * a single-line, adding a leading blank line would strip that whitespace.
 */
let print = (
  value: string,
  ~indentation: string="",
  ~preferMultipleLines: bool =false,
  ()
): string => {
  ""
};