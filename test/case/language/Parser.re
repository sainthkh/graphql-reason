open Test.Framework;

describe("Parser", ({describe, test}) => {
  let parse = text => Language.Parser.parse(text, ());
  let expectSyntaxError = Test.Util.expectSyntaxError(test, parse);
  let parseSucceeded = (msg, text) => {
    test(msg, ({expect}) => {
      let _r = parse(text);
      expect.int(0).toBe(0); // parse doesn't throw any Error.
    });
  };

  describe("parse provides useful errors", ({test}) => {
    expectSyntaxError("{", "Expected Name, found <EOF>", 1, 2);
    expectSyntaxError({|
      { ...MissingOn }
      fragment MissingOn Type|},
      "Expected \"on\", found Name \"Type\"",
      3, 26
    );
    expectSyntaxError("{ field: {} }", "Expected Name, found {", 1, 10);
    expectSyntaxError(
      "notanoperation Foo { field }",
      "Unexpected Name \"notanoperation\"",
      1, 1
    );
    expectSyntaxError("...", "Unexpected ...", 1, 1);
  });

  describe("parses variable inline values", ({test}) => {
    parseSucceeded("simple", "{ field(simple: 3) }");
    parseSucceeded("empty object", "{ field(empty: { }) }");
    parseSucceeded("simple object", "{ field(obj: { a: 1 }) }");
    parseSucceeded("nested object", "{ field(complex: { a: { b: [ \"Hi?\" ] } }) }");
    parseSucceeded("nested object with variable", "{ field(complex: { a: { b: [ $var ] } }) }");
  })

  describe("parses constant default values", ({test}) => {
    expectSyntaxError(
      "query Foo($x: Complex = { a: { b: [ $var ] } }) { field }",
      "Unexpected $",
      1, 37
    );
  });

  describe("parses variable definition directives", ({test}) => {
    parseSucceeded("parse directives", "query Foo($x: Boolean = false @bar) { field }");
  });

  describe("does not accept fragments named \"on\"", ({test}) => {
    expectSyntaxError("fragment on on on { on }", "Unexpected Name \"on\"", 1, 10);
  });

  describe("does not accept fragments spread of \"on\"", ({test}) => {
    expectSyntaxError("{ ...on }", "Expected Name, found }", 1, 9);
  });

  test("parses multi-byte characters", ({expect}) => {
    // Note: \u0A0A could be naively interpreted as two line-feed chars.
    let ast = Language.Parser.parse({|
      # This comment has a \u0A0A multi-byte character.
      { field(arg: "Has a \u0A0A multi-byte character.") }
    |}, ());

    // NOTE: Unlike JavaScript, we cannot simply retrieve value like
    // "definitions[0].selectionSet.selections[0].arguments[0].value.value"
    let o = ast.definitions[0]
      |> Type.Ast.toExecutableDefinitionNode
      |> Type.Ast.toOperationDefinitionNode
    let field = o.selectionSet.selections[0] 
      |> Type.Ast.toFieldNode;
    let value = Type.Ast.toStringValueNode(field.arguments[0].value).value;

    // NOTE: hexadecimal representation of \u0a0a in utf8 is e0 a8 8a.
    // check here: http://www.ltg.ed.ac.uk/~richard/utf-8.cgi?input=0a0a&mode=hex
    expect.string(value).toEqual("Has a \xe0\xa8\x8a multi-byte character.");
  });

  /*
  NOTE: commented out until I finish other parser functions. 
  it('parses kitchen sink', () => {
    expect(() => parse(kitchenSinkQuery)).to.not.throw();
  });
  */

  describe("allows non-keywords anywhere a Name is allowed", ({test}) => {
    let nonKeywords = [
      "on",
      "fragment",
      "query",
      "mutation",
      "subscription",
      "true",
      "false",
    ];

    nonKeywords
    |> List.iter(keyword => {
      let fragmentName = keyword != "on" ? keyword : "a";
      /* NOTE: The result of code below is like this in JavaScript.
      `
        query ${keyword} {
          ... ${fragmentName}
          ... on ${keyword} { field }
        }
        fragment ${fragmentName} on Type {
          ${keyword}(${keyword}: $${keyword})
            @${keyword}(${keyword}: ${keyword})
        }
      `
      As ReasonML doesn't have backtick string interpolation, I had to use ++.
      (FYI, {j||j} interpolation is for BuckleScript, not native ReasonML project.)
      */
      let doc = "\n" ++
        "query " ++ keyword ++ " {\n" ++
        " ... " ++ fragmentName ++ "\n" ++
        " ... " ++ " on " ++ keyword ++ "{ field }\n" ++
        "}\n" ++
        "fragment " ++ fragmentName ++ " on Type {\n" ++
        "  " ++ keyword ++ "(" ++ keyword ++ ": $" ++ keyword ++ ")\n" ++
        "    " ++ "@" ++ keyword ++ "(" ++ keyword ++": " ++ keyword ++ ")\n" ++
        "}";
      parseSucceeded(keyword ++ " parse success", doc);
    })
  });

  describe("parses mutation/subscription", ({test}) => {
    parseSucceeded("parses anonymous mutation operations", {|
      mutation {
        mutationField
      }
    |});
    parseSucceeded("parses anonymous subscription operations", {|
      subscription {
        subscriptionField
      }
    |});
    parseSucceeded("parses named mutation operations", {|
      mutation Foo {
        mutationField
      }
    |});
    parseSucceeded("parses named subscription operations", {|
      subscription Foo {
        subscriptionField
      }
    |});
  });

  test("creates ast", ({expect}) => {
    let document = Language.Parser.parse({|
{
  node(id: 4) {
    id,
    name
  }
}|}, ());

    let expectLoc = (loc: Type.Ast.location, start, end_) => {
      expect.int(loc.start).toBe(start);
      expect.int(loc.end_).toBe(end_);
    }

    // documentNode
    expectLoc(document.loc, 0, 41);
    
    // operationDefinitionNode
    let o = document.definitions[0]
      |> Type.Ast.toExecutableDefinitionNode
      |> Type.Ast.toOperationDefinitionNode
    // NOTE: Each location is added by 1 because we didn't use dedent here. 
    expectLoc(o.loc, 1, 41);
    expect.bool(o.operation == Query).toBe(true);
    expect.bool(Test.Util.isNone(o.name)).toBe(true);
    expect.int(Array.length(o.variableDefinitions)).toBe(0);
    expect.int(Array.length(o.directives)).toBe(0);

    // selectionSet for root node.
    let s0 = o.selectionSet;
    expectLoc(s0.loc, 1, 41);

    // fieldNode for node(id: 4)
    let node = o.selectionSet.selections[0]
      |> Type.Ast.toFieldNode;
    expectLoc(node.loc, 5, 39);
    expect.bool(Test.Util.isNone(node.alias)).toBe(true);
    let nodeName = node.name;
    expectLoc(nodeName.loc, 5, 9);
    expect.string(nodeName.value).toEqual("node");
    let arg0 = node.arguments[0];
    let arg0Name = arg0.name;
    expectLoc(arg0Name.loc, 10, 12);
    expect.string(arg0Name.value).toEqual("id");
    let arg0Value = arg0.value |> Type.Ast.toIntValueNode;
    expectLoc(arg0Value.loc, 14, 15);
    expect.int(arg0Value.value).toBe(4);
    expect.int(Array.length(node.directives)).toBe(0);

    // selectionSet under node(id: 4) (i.e. id, name)
    let content = node.selectionSet |> Type.Ast.unwrap;
    expectLoc(content.loc, 17, 39);
    let id = content.selections[0] |> Type.Ast.toFieldNode;
    expectLoc(id.loc, 23, 25);
    expect.bool(Test.Util.isNone(id.alias)).toBe(true);
    expectLoc(id.name.loc, 23, 25);
    expect.string(id.name.value).toEqual("id");
    expect.int(Array.length(id.arguments)).toBe(0);
    expect.int(Array.length(id.directives)).toBe(0);
    expect.bool(Test.Util.isNone(id.selectionSet)).toBe(true);
    let name = content.selections[1] |> Type.Ast.toFieldNode;
    expectLoc(name.loc, 31, 35);
    expect.bool(Test.Util.isNone(name.alias)).toBe(true);
    expectLoc(name.name.loc, 31, 35);
    expect.string(name.name.value).toEqual("name");
    expect.int(Array.length(name.arguments)).toBe(0);
    expect.int(Array.length(name.directives)).toBe(0);
    expect.bool(Test.Util.isNone(name.selectionSet)).toBe(true);
  });

  describe("Experimental: allows parsing fragment defined variables", ({test}) => {
    let document = "fragment a($v: Boolean = false) on t { f(v: $v) }";

    ignore(Language.Parser.parse(document, ~options=?Some(Language.Option.Parse.make(
      ~experimentalFragmentVariables=true,
      ()
    )), ()));
    expectSyntaxError(document, "Fragment variables are not allowed. If you want to use it, pass experimentalFragmentVariables option to parser", 1, 11);
  });

  test("contains references to source", ({expect}) => {
    let source = Util.Source.make(~body="{ id }", ());
    let result = Language.Parser.parseSource(source, ());

    expect.string(result.loc.source.body).toEqual(source.body);
  });

  test("contains references to start and end tokens", ({expect}) => {
    let result = Language.Parser.parse("{ id }", ())

    expect.bool(result.loc.startToken.kind == Type.Token.SOF).toBe(true);
    expect.bool(result.loc.endToken.kind == Type.Token.EOF).toBe(true);
  });
});

/* NOTE: Tests to be implemented.
  describe('parseValue', () => {
    it('parses null value', () => {
      const result = parseValue('null');
      expect(toJSONDeep(result)).to.deep.equal({
        kind: Kind.NULL,
        loc: { start: 0, end: 4 },
      });
    });

    it('parses list values', () => {
      const result = parseValue('[123 "abc"]');
      expect(toJSONDeep(result)).to.deep.equal({
        kind: Kind.LIST,
        loc: { start: 0, end: 11 },
        values: [
          {
            kind: Kind.INT,
            loc: { start: 1, end: 4 },
            value: '123',
          },
          {
            kind: Kind.STRING,
            loc: { start: 5, end: 10 },
            value: 'abc',
            block: false,
          },
        ],
      });
    });

    it('parses block strings', () => {
      const result = parseValue('["""long""" "short"]');
      expect(toJSONDeep(result)).to.deep.equal({
        kind: Kind.LIST,
        loc: { start: 0, end: 20 },
        values: [
          {
            kind: Kind.STRING,
            loc: { start: 1, end: 11 },
            value: 'long',
            block: true,
          },
          {
            kind: Kind.STRING,
            loc: { start: 12, end: 19 },
            value: 'short',
            block: false,
          },
        ],
      });
    });
  });

  describe('parseType', () => {
    it('parses well known types', () => {
      const result = parseType('String');
      expect(toJSONDeep(result)).to.deep.equal({
        kind: Kind.NAMED_TYPE,
        loc: { start: 0, end: 6 },
        name: {
          kind: Kind.NAME,
          loc: { start: 0, end: 6 },
          value: 'String',
        },
      });
    });

    it('parses custom types', () => {
      const result = parseType('MyType');
      expect(toJSONDeep(result)).to.deep.equal({
        kind: Kind.NAMED_TYPE,
        loc: { start: 0, end: 6 },
        name: {
          kind: Kind.NAME,
          loc: { start: 0, end: 6 },
          value: 'MyType',
        },
      });
    });

    it('parses list types', () => {
      const result = parseType('[MyType]');
      expect(toJSONDeep(result)).to.deep.equal({
        kind: Kind.LIST_TYPE,
        loc: { start: 0, end: 8 },
        type: {
          kind: Kind.NAMED_TYPE,
          loc: { start: 1, end: 7 },
          name: {
            kind: Kind.NAME,
            loc: { start: 1, end: 7 },
            value: 'MyType',
          },
        },
      });
    });

    it('parses non-null types', () => {
      const result = parseType('MyType!');
      expect(toJSONDeep(result)).to.deep.equal({
        kind: Kind.NON_NULL_TYPE,
        loc: { start: 0, end: 7 },
        type: {
          kind: Kind.NAMED_TYPE,
          loc: { start: 0, end: 6 },
          name: {
            kind: Kind.NAME,
            loc: { start: 0, end: 6 },
            value: 'MyType',
          },
        },
      });
    });

    it('parses nested types', () => {
      const result = parseType('[MyType!]');
      expect(toJSONDeep(result)).to.deep.equal({
        kind: Kind.LIST_TYPE,
        loc: { start: 0, end: 9 },
        type: {
          kind: Kind.NON_NULL_TYPE,
          loc: { start: 1, end: 8 },
          type: {
            kind: Kind.NAMED_TYPE,
            loc: { start: 1, end: 7 },
            name: {
              kind: Kind.NAME,
              loc: { start: 1, end: 7 },
              value: 'MyType',
            },
          },
        },
      });
    });
  });
});*/


// NOTE: Tests below are in graphql-js but ignored in graphql-reason
// They're outside of describe because when a function ends with comment, it's an error.

/**
  * NOTE: Don't need to test no argument cases in graphql-js. 
  * Because they're handled at compile time. 
  */
/*
it('asserts that a source to parse was provided', () => {
  // $DisableFlowOnNegativeTest
  expect(() => parse()).to.throw('Must provide Source. Received: undefined');
});

it('asserts that an invalid source to parse was provided', () => {
  // $DisableFlowOnNegativeTest
  expect(() => parse({})).to.throw('Must provide Source. Received: {}');
});
*/

/*
  * NOTE: Ignore this test because ReasonML doesn't have the root Error class. 
  * Maybe we need to create JavaScript-ish error printing function.
it('parse provides useful error when using source', () => {
  let caughtError;
  try {
    parse(new Source('query', 'MyQuery.graphql'));
  } catch (error) {
    caughtError = error;
  }
  expect(String(caughtError)).to.equal(dedent`
    Syntax Error: Expected {, found <EOF>

    MyQuery.graphql (1:6)
    1: query
            ^
  `);
});
*/

/* NOTE: Skips nameless query test. 
Because it's too long and most of the content is tested above. 

it('creates ast from nameless query without variables', () => {
  const result = parse(dedent`
    query {
      node {
        id
      }
    }
  `);

  expect(toJSONDeep(result)).to.deep.equal({
    kind: Kind.DOCUMENT,
    loc: { start: 0, end: 30 },
    definitions: [
      {
        kind: Kind.OPERATION_DEFINITION,
        loc: { start: 0, end: 29 },
        operation: 'query',
        name: undefined,
        variableDefinitions: [],
        directives: [],
        selectionSet: {
          kind: Kind.SELECTION_SET,
          loc: { start: 6, end: 29 },
          selections: [
            {
              kind: Kind.FIELD,
              loc: { start: 10, end: 27 },
              alias: undefined,
              name: {
                kind: Kind.NAME,
                loc: { start: 10, end: 14 },
                value: 'node',
              },
              arguments: [],
              directives: [],
              selectionSet: {
                kind: Kind.SELECTION_SET,
                loc: { start: 15, end: 27 },
                selections: [
                  {
                    kind: Kind.FIELD,
                    loc: { start: 21, end: 23 },
                    alias: undefined,
                    name: {
                      kind: Kind.NAME,
                      loc: { start: 21, end: 23 },
                      value: 'id',
                    },
                    arguments: [],
                    directives: [],
                    selectionSet: undefined,
                  },
                ],
              },
            },
          ],
        },
      },
    ],
  });
});

  */

/* NOTE: We don't need to test noLocation.
Because we removed that in graphql-reason.

it('allows parsing without source location information', () => {
  const result = parse('{ id }', { noLocation: true });
  expect(result.loc).to.equal(undefined);
});
*/

/* NOTE: There is no JSON.stringify in graphql-reason.

it('contains location information that only stringifys start/end', () => {
  const result = parse('{ id }');

  expect(JSON.stringify(result.loc)).to.equal('{"start":0,"end":6}');
  expect(nodeInspect(result.loc)).to.equal('{ start: 0, end: 6 }');
  expect(inspect(result.loc)).to.equal('{ start: 0, end: 6 }');
});
*/