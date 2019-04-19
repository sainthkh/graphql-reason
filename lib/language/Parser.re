/**
 * If the next token is of the given kind, return that token after advancing
 * the lexer. Otherwise, do not change the parser state and throw an error.
 */
let expectToken = (
  lexer: Lexer.t,
  kind: Type.Token.kind
): Type.Token.t => {
  let token = lexer.token^;
  switch(token.kind == kind) {
  | true => {
    ignore(Lexer.advance(lexer));
    token
  }
  | false => Error.API.syntaxError(
    lexer.source,
    token.start, 
    "Expected " ++ Type.Token.string_of_kind(kind) ++ ", found " ++ Type.Token.desc(token)
  )
  }
};

/**
 * If the next token is of the given kind, return that token after advancing
 * the lexer. Otherwise, do not change the parser state and return undefined.
 *
 * NOTE: Reason doesn't have undefined. So, we return None. 
 */
let expectOptionalToken = (
  lexer: Lexer.t,
  kind: Type.Token.kind
): option(Type.Token.t) => {
  let token = lexer.token^;
  switch(token.kind == kind) {
  | true => {
    ignore(Lexer.advance(lexer));
    Some(token);
  }
  | false => None
  }
};

/**
 * If the next token is a given keyword, return that token after advancing
 * the lexer. Otherwise, do not change the parser state and return undefined.
 */
let expectOptionalKeyword = (
  lexer: Lexer.t,
  value: string
) => {
  let token = lexer.token^;
  switch(token.kind == Type.Token.Name) {
  | true => {
    switch(token.value) {
    | Some(value) => {
      ignore(Lexer.advance(lexer));
      Some(token);
    }
    | None => None;
    }
  }
  | false => None;
  }
};

/**
 * Helper function for creating an error when an unexpected lexed token
 * is encountered.
 *
 * NOTE: unexpected function in graphql-js is broken into 2 functions. 
 */
let unexpectedToken = (lexer: Lexer.t, token: Type.Token.t) => {
  Error.API.syntaxError(
    lexer.source,
    token.start,
    "Unexpected " ++ Type.Token.desc(token)
  );
};

let unexpected = (lexer: Lexer.t) => {
  unexpectedToken(lexer, lexer.token^);
};

/**
 * Determines if the next token is of a given kind
 */
let peek = (lexer: Lexer.t, kind: Type.Token.kind) =>
  lexer.token^.kind == kind;

/**
 * NOTE: In ReasonML, it is more natural to use pattern matching than
 * if-else statements. So, it is wiser to peek Type.Token.kind and 
 * take appropriate action. 
 */
let peekKind = (lexer: Lexer.t) => lexer.token^.kind;

/**
 * Returns a possibly empty list of parse nodes, determined by
 * the parseFn. This list begins with a lex token of openKind
 * and ends with a lex token of closeKind. Advances the parser
 * to the next lex token after the closing token.
 */
let any = (
  lexer: Lexer.t,
  openKind: Type.Token.kind,
  parseFn: Lexer.t => 'a,
  closeKind: Type.Token.kind
): array('a) => {
  let rec anyInternal = nodes =>  
    switch(expectOptionalToken(lexer, closeKind)) {
    | Some(_) => nodes
    | None => anyInternal(nodes)
    }
  
  ignore(expectToken(lexer, openKind));
  let nodes = [||];
  anyInternal(nodes);
};

/**
 * Returns a non-empty list of parse nodes, determined by
 * the parseFn. This list begins with a lex token of openKind
 * and ends with a lex token of closeKind. Advances the parser
 * to the next lex token after the closing token.
 */
let many = (
  lexer: Lexer.t,
  openKind: Type.Token.kind,
  parseFn: Lexer.t => 'a,
  closeKind: Type.Token.kind
): array('a) => {
  let rec manyInternal = nodes =>  
    switch(expectOptionalToken(lexer, closeKind)) {
    | Some(_) => nodes
    | None => manyInternal(nodes)
    }
  
  ignore(expectToken(lexer, openKind));
  let nodes = [|parseFn(lexer)|];
  manyInternal(nodes);
};

/**
 * Returns a location object, used to identify the place in
 * the source that created a given parsed object.
 */
let loc = (
  lexer: Lexer.t,
  startToken: Type.Token.t
): Type.Ast.location => {
  start: startToken.start,
  end_: lexer.lastToken^.end_,
  startToken: startToken,
  endToken: lexer.lastToken^,
  source: lexer.source,
};

/**
 * NOTE: In some functions like parseDirectives or parseArguments, 
 * when there is no item in the list, we should return None rather than 
 * empty array. To solve that problem, this original function has been added. 
 */
let optionizeArray = (
  a: array('a)
): option(array('a)) => {
  switch(Array.length(a)) {
  | 0 => None
  | _ => Some(a)
  }
};

// Implements the parsing rules in the Document section.

/**
 * Converts a name lex token into a name parse node.
 */
let parseName = (
  lexer: Lexer.t
): Type.Ast.nameNode => {
  let token = expectToken(lexer, Type.Token.Name);
  {
    loc: loc(lexer, token),
    value: Type.Token.unwrapValue(token),
  }
};

/**
 * NamedType : Name
 */
let parseNamedType = (
  lexer: Lexer.t
): Type.Ast.namedTypeNode => {
  let start = lexer.token^;

  {
    name: parseName(lexer),
    loc: loc(lexer, start),
  }    
};

let parseStringLiteral = (
  lexer: Lexer.t
): Type.Ast.valueNode => {
  let token = lexer.token^;
  ignore(Lexer.advance(lexer));

  Type.Ast.StringValueNode(
    loc(lexer, token),
    Type.Token.unwrapValue(token),
    token.kind == Type.Token.BlockString
  );
};

/**
 * Variable : $ Name
 */
let parseVariable = (
  lexer: Lexer.t
): Type.Ast.variableNode => {
  let start = lexer.token^;
  ignore(expectToken(lexer, Type.Token.Dollar));

  {
    name: parseName(lexer),
    loc: loc(lexer, start),
  };
};

let rec parseConstValue = (
  lexer: Lexer.t
): Type.Ast.valueNode => {
  parseValueLiteral(lexer, true);
}

and parseValueValue = (
  lexer: Lexer.t
): Type.Ast.valueNode => {
  parseValueLiteral(lexer, false);
}

/**
 * ListValue[Const] :
 *   - [ ]
 *   - [ Value[?Const]+ ]
 */
and parseList = (
  lexer: Lexer.t,
  isConst: bool
): Type.Ast.valueNode => {
  let start = lexer.token^;
  let item = isConst ? parseConstValue : parseValueValue;

  Type.Ast.ListValueNode(
    loc(lexer, start),
    any(lexer, Type.Token.BracketLeft, item, Type.Token.BracketRight)
  );
}

/**
 * ObjectField[Const] : Name : Value[?Const]
 */
and parseObjectField = (
  lexer: Lexer.t,
  isConst: bool
): Type.Ast.objectFieldNode => {
  let start = lexer.token^;
  let name = parseName(lexer);
  ignore(expectToken(lexer, Type.Token.Colon));

  {
    name,
    value: parseValueLiteral(lexer, isConst),
    loc: loc(lexer, start),
  }
}

/**
 * ObjectValue[Const] :
 *   - { }
 *   - { ObjectField[?Const]+ }
 */
and parseObject = (
  lexer: Lexer.t,
  isConst: bool
): Type.Ast.valueNode => {
  let start = lexer.token^;
  let item = _ => parseObjectField(lexer, isConst);

  Type.Ast.ObjectValueNode(
    loc(lexer, start),
    any(lexer, Type.Token.BraceLeft, item, Type.Token.BraceRight)
  );
}

/**
 * Value[Const] :
 *   - [~Const] Variable
 *   - IntValue
 *   - FloatValue
 *   - StringValue
 *   - BooleanValue
 *   - NullValue
 *   - EnumValue
 *   - ListValue[?Const]
 *   - ObjectValue[?Const]
 *
 * BooleanValue : one of `true` `false`
 *
 * NullValue : `null`
 *
 * EnumValue : Name but not `true`, `false` or `null`
 */
and parseValueLiteral = (
  lexer: Lexer.t,
  isConst: bool
): Type.Ast.valueNode => {
  let token = lexer.token^;
  switch(token.kind) {
  | Type.Token.BracketLeft => parseList(lexer, isConst)
  | Type.Token.BraceLeft => parseObject(lexer, isConst)
  | Type.Token.Int => {
    ignore(Lexer.advance(lexer));
    IntValueNode(loc(lexer, token), Type.Token.unwrapValue(token))
  }
  | Type.Token.Float => {
    ignore(Lexer.advance(lexer));
    FloatValueNode(loc(lexer, token), Type.Token.unwrapValue(token))
  }
  | Type.Token.String | Type.Token.BlockString => parseStringLiteral(lexer)
  | Type.Token.Name => {
    ignore(Lexer.advance(lexer));
    let value = Type.Token.unwrapValue(token)
    switch(value) {
    | "true" | "false" => BooleanValueNode(loc(lexer, token), value == "true")
    | "null" => NullValueNode(loc(lexer, token))
    | _ => EnumValueNode(loc(lexer, token), value)
    }
  }
  | Type.Token.Dollar => {
    switch(isConst) {
    | false => {
      let node = parseVariable(lexer);
      Type.Ast.VariableNode(node.loc, node);
    }
    | true => unexpected(lexer)
    }
  }
  | _ => unexpected(lexer)
  }
};

/**
 * Argument[Const] : Name : Value[?Const]
 */
let parseArgument = (
  lexer: Lexer.t
): Type.Ast.argumentNode => {
  let start = lexer.token^;
  let name = parseName(lexer);
  ignore(expectToken(lexer, Type.Token.Colon));

  {
    name,
    value: parseValueLiteral(lexer, false),
    loc: loc(lexer, start),
  }
};

let parseConstArgument = (
  lexer: Lexer.t
): Type.Ast.argumentNode => {
  let start = lexer.token^;
  let name = parseName(lexer);
  ignore(expectToken(lexer, Type.Token.Colon));
  
  {
    name,
    value: parseConstValue(lexer),
    loc: loc(lexer, start),
  }
};

/**
 * Arguments[Const] : ( Argument[?Const]+ )
 */
let parseArguments = (
  lexer: Lexer.t,
  isConst: bool
): option(array(Type.Ast.argumentNode)) => {
  let item = isConst ? parseConstArgument : parseArgument;
  
  optionizeArray(
    peek(lexer, Type.Token.ParenLeft)
    ? many(lexer, Type.Token.ParenLeft, item, Type.Token.ParenRight)
    : [||]
  )
};

/**
 * Directive[Const] : @ Name Arguments[?Const]?
 */
let parseDirective = (
  lexer: Lexer.t,
  isConst: bool
): Type.Ast.directiveNode => {
  let start = lexer.token^;
  ignore(expectToken(lexer, Type.Token.At));

  {
    name: parseName(lexer),
    arguments: parseArguments(lexer, isConst),
    loc: loc(lexer, start),
  }
};

/**
 * Directives[Const] : Directive[?Const]+
 */
let parseDirectives = (
  lexer: Lexer.t,
  isConst: bool
): option(array(Type.Ast.directiveNode)) => {
  let rec parseDirectivesInternal = (directives) => {
    switch(peek(lexer, Type.Token.At)) {
    | true => parseDirectivesInternal(
      Array.append(directives, [|parseDirective(lexer, isConst)|])
    )
    | false => directives;
    }
  };

  optionizeArray(
    parseDirectivesInternal([||])
  );
};

/**
 * Field : Alias? Name Arguments? Directives? SelectionSet?
 *
 * Alias : Name :
 */
let rec parseField = (
  lexer: Lexer.t
): Type.Ast.selectionNode => {
  let start = lexer.token^;

  let nameOrAlias = parseName(lexer);
  let colonExists = expectOptionalToken(lexer, Type.Token.Colon);

  let alias = switch(colonExists) {
  | Some(_) => Some(nameOrAlias)
  | None => None
  };

  let name = switch(colonExists) {
  | Some(_) => parseName(lexer)
  | None => nameOrAlias
  };

  Type.Ast.FieldNode(
    loc(lexer, start),
    alias,
    name,
    parseArguments(lexer, false),
    parseDirectives(lexer, false),
    peek(lexer, Type.Token.BraceLeft) 
    ? Some(parseSelectionSet(lexer))
    : None
  )
}

/**
 * FragmentName : Name but not `on`
 */
and parseFragmentName = (
  lexer: Lexer.t
): Type.Ast.nameNode => {
  switch(Type.Token.unwrapValue(lexer.token^) == "on") {
  | false => parseName(lexer)
  | true => unexpected(lexer)
  }
}

/**
 * Corresponds to both FragmentSpread and InlineFragment in the spec.
 *
 * FragmentSpread : ... FragmentName Directives?
 *
 * InlineFragment : ... TypeCondition? Directives? SelectionSet
 */
and parseFragment = (
  lexer: Lexer.t
): Type.Ast.selectionNode => {
  let start = lexer.token^;
  ignore(expectToken(lexer, Type.Token.Spread));

  switch(expectOptionalKeyword(lexer, "on"), peek(lexer, Type.Token.Name)) {
  | (None, true) => FragmentSpreadNode(
    loc(lexer, start),
    parseFragmentName(lexer),
    parseDirectives(lexer, false)
  )
  | (Some(_keyword), _) => InlineFragmentNode(
    loc(lexer, start),
    Some(parseNamedType(lexer)),
    parseDirectives(lexer, false),
    parseSelectionSet(lexer)
  )
  | (None, false) => InlineFragmentNode(
    loc(lexer, start),
    None,
    parseDirectives(lexer, false),
    parseSelectionSet(lexer)
  )
  }
}

and parseSelection = (
  lexer: Lexer.t
): Type.Ast.selectionNode => {
  switch(peekKind(lexer)) {
  | Type.Token.Spread => parseFragment(lexer)
  | _ => parseField(lexer)
  }
}

and parseSelectionSet = (
  lexer: Lexer.t
): Type.Ast.selectionSetNode => {
  let start = lexer.token^;

  {
    selections: many(
      lexer, 
      Type.Token.BraceLeft, 
      parseSelection, 
      Type.Token.BraceRight
    ),
    loc: loc(lexer, start),
  }
};

/**
 * OperationType : one of query mutation subscription
 * 
 * NOTE: It's one of the variant value in our case. 
 */
let parseOperationType = (
  lexer: Lexer.t
): Type.Ast.operationTypeNode => {
  let token = expectToken(lexer, Type.Token.Name);
  switch(Type.Token.unwrapValue(token)) {
  | "query" => Type.Ast.Query
  | "mutation" => Type.Ast.Mutation
  | "subscription" => Type.Ast.Subscription
  | _ => unexpectedToken(lexer, token)
  };
};

let rec parseTypeReference = (
  lexer: Lexer.t
): Type.Ast.typeNode => {
  let start = lexer.token^;

  switch(expectOptionalToken(lexer, Type.Token.BracketLeft)) {
  | Some(_token) => {
    let type_ = parseTypeReference(lexer);
    ignore(expectToken(lexer, Type.Token.BracketRight));

    Type.Ast.(ComplexTypeNode(loc(lexer, start), ListTypeNode(type_)))
  }
  | None => {
    let type_ = parseNamedType(lexer);
    switch(expectOptionalToken(lexer, Type.Token.Bang)) {
    | Some(_token) => {
      Type.Ast.(
        ComplexTypeNode(
          loc(lexer, start), 
          NonNullTypeNode(NamedTypeNode(type_.loc, type_))
        )
      )
    }
    | None => Type.Ast.NamedTypeNode(type_.loc, type_);
    }
  }
  }
};

/**
 * VariableDefinition : Variable : Type DefaultValue? Directives[Const]?
 */
let parseVariableDefinition = (
  lexer: Lexer.t
): Type.Ast.variableDefinitionNode => {
  let start = lexer.token^;

  let variable = parseVariable(lexer);
  ignore(expectToken(lexer, Type.Token.Colon));
  let type_ = parseTypeReference(lexer);

  {
    variable,
    type_,
    defaultValue: switch(expectOptionalToken(lexer, Type.Token.Equals)) {
    | Some(_) => Some(parseValueLiteral(lexer, true))
    | None => None
    },
    directives: parseDirectives(lexer, true),
    loc: loc(lexer, start),
  }
};

/**
 * VariableDefinitions : ( VariableDefinition+ )
 */
let parseVariableDefinitions = (
  lexer: Lexer.t
): option(array(Type.Ast.variableDefinitionNode)) => {
  optionizeArray(
    peek(lexer, Type.Token.ParenLeft)
    ? many(lexer, Type.Token.ParenLeft, parseVariableDefinition, Type.Token.ParenRight)
    : [||]
  );
};

let parseOperationDefinition = (
  lexer: Lexer.t
): Type.Ast.operationDefinitionNode => {
  let start = lexer.token^;
  switch(peek(lexer, Type.Token.BraceLeft)) {
  | true => {
    operation: Query,
    name: None,
    variableDefinitions: None,
    directives: None,
    selectionSet: parseSelectionSet(lexer),
    loc: loc(lexer, start)
  }
  | false => {
    let operation = parseOperationType(lexer);
    let name = 
      switch(peek(lexer, Type.Token.Name)) {
      | true => Some(parseName(lexer))
      | false => None
      };
    {
      operation,
      name,
      variableDefinitions: parseVariableDefinitions(lexer),
      directives: parseDirectives(lexer, false),
      selectionSet: parseSelectionSet(lexer),
      loc: loc(lexer, start),
    }
  }
  }
};

/**
 * ExecutableDefinition :
 *   - OperationDefinition
 *   - FragmentDefinition
 */
let parseExecutableDefinition = (
  lexer: Lexer.t
): Type.Ast.executableDefinitionNode => {
  switch(peekKind(lexer)) {
  | Type.Token.Name => {
    switch(lexer.token^.value) {
    | Some(v) => {
      switch(v) {
      | "query" | "mutation" | "subscription"
        => Type.Ast.OperationDefinitionNode(parseOperationDefinition(lexer))
      /*
      | "fragment"
        => parseFragmentDefinition(lexer)
      */
      | _ => unexpected(lexer)
      }
    }
    | None => unexpected(lexer)
    }
  }
  | Type.Token.BraceLeft 
    => Type.Ast.OperationDefinitionNode(parseOperationDefinition(lexer))
  | _ => unexpected(lexer)
  }
};

/**
 * Definition :
 *   - ExecutableDefinition
 *   - TypeSystemDefinition
 *   - TypeSystemExtension
 */
let parseDefinition = (
  lexer: Lexer.t
): Type.Ast.definitionNode => {
  open Type;
  switch(peekKind(lexer)) {
  | Token.Name => {
    switch(lexer.token^.value) {
    | Some(v) => {
      switch(v) {
      | "query" | "mutation" | "subscription" | "fragment" 
        => Type.Ast.ExecutableDefinitionNode(parseExecutableDefinition(lexer));
      /*
      | "schema" | "scalar" | "type" | "interface"
      | "union" | "enum" | "input" | "directive"
        => parseTypeSystemDefinition(lexer);
      | "extend"
        => parseTypeSystemExtension(lexer);
      */
      | _ => unexpected(lexer);
      }
    }
    | None => unexpected(lexer);
    }
  }
  | Token.BraceLeft
    => Type.Ast.ExecutableDefinitionNode(parseExecutableDefinition(lexer));
  /*
  | Token.String | Token.BlockString
    => parseTypeSystemDefinition(lexer);
  */
  | _ => unexpected(lexer);
  }
};

/**
 * Document : Definition+
 */
let parseDocument = (
  lexer: Lexer.t
): Type.Ast.documentNode => {
  let start = lexer.token^;

  {
    definitions: many(lexer, Type.Token.SOF, parseDefinition, Type.Token.EOF),
    loc: loc(lexer, start),
  }
};

/**
 * Given a GraphQL source, parses it into a Document.
 * Throws GraphQLError if a syntax error is encountered.
 *
 * NOTE: parse function in graphql-js is broken into 2 functions. 
 */
let parseSource = (
  source: Util.Source.t,
  ~options: option(Option.Parse.t) =?,
  ()
): Type.Ast.documentNode => {
  let lexer = Lexer.make(source, ~options=?options, ());
  
  parseDocument(lexer);
};

let parse = (
  source: string,
  ~options: option(Option.Parse.t) =?,
  ()
): Type.Ast.documentNode => {
  let source = Util.Source.make(source, ());

  parseSource(source, ~options=?options, ());
};