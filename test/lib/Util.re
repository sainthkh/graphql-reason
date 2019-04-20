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
        expect.int(loc.line).toBe(line);
        expect.int(loc.column).toBe(col);
      }
      }
    }
    }
  })
};