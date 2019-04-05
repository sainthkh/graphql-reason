type location = {
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
  locationOffset: location,
}

exception InvalidLocation(string);

let make = (
  ~body: string, 
  ~name: option(string)=?, 
  ~locationOffset: option(location)=?,
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
}