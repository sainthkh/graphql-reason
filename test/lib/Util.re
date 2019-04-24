open Framework;

let expectSyntaxError = (
  test: Rely.Test.testFn('a),
  f: string => 'b,
  text: string, 
  errMsg: string, 
  line: int, 
  col: int
) => {
  test("Test: " ++ text, ({expect}) => {
    switch(f(text)) {
    | _token => expect.int(0).toBe(-1) // shouldn't be here.
    | exception Error.GraphQLError.Exception(err) => {
      expect.string(err.message).toEqual("Syntax Error: " ++ errMsg);
      
      switch(err.locations) {
      | None => expect.int(1).toBe(-1) // shouldn't be here.
      | Some(locations) => {
        expect.int(Array.length(locations)).toBe(1);
        let loc = locations[0];
        expect.string("Line: " ++ string_of_int(loc.line))
          .toEqual("Line: " ++ string_of_int(line));
        expect.string("Col: " ++ string_of_int(loc.column))
          .toEqual("Col: " ++ string_of_int(col));
      }
      }
    }
    }
  })
};

let isNone = (v: option('a)) => {
  switch(v) {
  | Some(_a) => false
  | None => true
  };
}