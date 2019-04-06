/**
 * The starting offset of the GraphQL code. 
 * In many cases, GraphQL code starts 
 *
 * NOTE: In graphql-js, it is called location. But I concluded that offset is the better name for this.
 */
type offset = {
  line: int,
  column: int,
};

/**
 * A representation of source input to GraphQL.
 * `name` and `locationOffset` are optional. They are useful for clients who
 * store GraphQL documents in source files; for example, if the GraphQL input
 * starts at line 40 in a file named Foo.graphql, it might be useful for name to
 * be "Foo.graphql" and location to be `{ line: 40, column: 0 }`.
 * line and column in locationOffset are 1-indexed
 */
type t = {
  body: string,
  name: string,
  locationOffset: offset,
}

exception InvalidLocation(string);

let make = (
  ~body: string, 
  ~name: option(string)=?, 
  ~locationOffset: option(offset)=?,
  ()
) => {
  body: body,
  name: switch(name) {
  | Some(name) => name
  | None => "GraphQL Request"
  },
  locationOffset: switch(locationOffset) {
  | Some(loc) => {
    if (loc.line < 1) {
      raise(InvalidLocation("line in locationOffset is 1-indexed and must be positive"));
    };
    if (loc.column < 1) {
      raise(InvalidLocation("column in locationOffset is 1-indexed and must be positive"));
    }

    loc
  }
  | None => {
    line: 1,
    column: 1,
  }
  }
};

/**
 * Represents a location in a Source.
 * 
 * NOTE: In graphql-js, it's called SourceLocation and is located at language/location.js
 * But if we follow that convention here, it becomes Language.Locaction.sourceLocation. 
 * It's a bit uneasier to read than Util.Source.location. 
 * So, we changed the Location in source.js to offset and SourceLocation to location.
 */
type location = {
  line: int,
  column: int,
};

/**
 * Takes a Source and a UTF-8 character offset, and returns the corresponding
 * line and column as a location.
 * 
 * NOTE: In graphql-js, it's written with regex and while statement. 
 * But it's re-written without regex and with pattern matching. 
 */
let getLocation = (source: t, position: int) => {
  let body = source.body;
  let bodyLength = String.length(body);
  let line = 1;
  let lineStart = 0; // It's index. So, it's 0-based.

  let rec getLocationInternal = (index, line, lineStart) => {
    let loc = () => {
      line,
      column: index - lineStart + 1,
    };

    switch(index < bodyLength) {
    | false => loc()
    | true => {
      switch(index < position) {
      | false => loc()
      | true => {
        let c = String.get(body, index);

        switch(c) {
        | '\n' => getLocationInternal(index + 1, line + 1, index + 1)
        | '\r' => {
          switch(index + 1 < bodyLength) {
          | false => getLocationInternal(index + 1, line + 1, index + 1);
          | true => {
            let next = String.get(body, index + 1);
            switch(next) {
            | '\n' => getLocationInternal(index + 2, line + 1, index + 2);
            | _ => getLocationInternal(index + 1, line + 1, index + 1);
            }
          }
          }
        }
        | _ => getLocationInternal(index + 1, line, lineStart)
        }
      }
      }
    }
    }
  };

  getLocationInternal(0, line, lineStart);
}