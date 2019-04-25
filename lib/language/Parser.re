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
  switch(expectToken(lexer, kind)) {
  | token => Some(token)
  | exception _ => None;
  }
};

/**
 * If the next token is a given keyword, return that token after advancing
 * the lexer. Otherwise, do not change the parser state and throw an error.
 */
let expectKeyword = (
  lexer: Lexer.t,
  value: string
): Type.Token.t => {
  let token = lexer.token^;
  let error = () => 
    Error.API.syntaxError(
      lexer.source,
      token.start,
      "Expected \"" ++ value ++ "\", found " ++ Type.Token.desc(token)
    );
  
  switch(token.kind == Type.Token.Name) {
  | true => {
    switch(token.value) {
    | Some(v) => {
      switch(v == value) {
      | true => {
        ignore(Lexer.advance(lexer))
        token;
      }
      | false => error()
      }
    }
    | None => error()
    }
  }
  | false => error()
  }
};

/**
 * If the next token is a given keyword, return that token after advancing
 * the lexer. Otherwise, do not change the parser state and return undefined.
 */
let expectOptionalKeyword = (
  lexer: Lexer.t,
  value: string
): option(Type.Token.t) => {
  switch(expectKeyword(lexer, value)) {
  | token => Some(token)
  | exception _ => None
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
    | None => anyInternal(Array.append(nodes, [|parseFn(lexer)|]))
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
    | None => manyInternal(Array.append(nodes, [|parseFn(lexer)|]))
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

// Implements the parsing rules in the Document section.

/**
 * Converts a name lex token into a name parse node.
 */
let parseName = (
  lexer: Lexer.t
): Type.Ast.nameNode => {
  let token = expectToken(lexer, Type.Token.Name);
  let value = Type.Token.unwrapValue(token);

  {
    loc: loc(lexer, token),
    value,
  }
};

/**
 * NamedType : Name
 */
let parseNamedType = (
  lexer: Lexer.t
): Type.Ast.namedTypeNode => {
  let start = lexer.token^;
  let name = parseName(lexer);

  {
    name,
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
  let name = parseName(lexer);

  {
    name,
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
  let value = parseValueLiteral(lexer, isConst);

  {
    name,
    value,
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
    IntValueNode(loc(lexer, token), int_of_string(Type.Token.unwrapValue(token)))
  }
  | Type.Token.Float => {
    ignore(Lexer.advance(lexer));
    FloatValueNode(loc(lexer, token), float_of_string(Type.Token.unwrapValue(token)))
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
  let value = parseValueLiteral(lexer, false);

  {
    name,
    value,
    loc: loc(lexer, start),
  }
};

let parseConstArgument = (
  lexer: Lexer.t
): Type.Ast.argumentNode => {
  let start = lexer.token^;
  let name = parseName(lexer);
  ignore(expectToken(lexer, Type.Token.Colon));
  let value = parseConstValue(lexer);

  {
    name,
    value,
    loc: loc(lexer, start),
  }
};

/**
 * Arguments[Const] : ( Argument[?Const]+ )
 */
let parseArguments = (
  lexer: Lexer.t,
  isConst: bool
): array(Type.Ast.argumentNode) => {
  let item = isConst ? parseConstArgument : parseArgument;
  
  peek(lexer, Type.Token.ParenLeft)
  ? many(lexer, Type.Token.ParenLeft, item, Type.Token.ParenRight)
  : [||]
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
  let name = parseName(lexer);
  let arguments = parseArguments(lexer, isConst);

  {
    name,
    arguments,
    loc: loc(lexer, start),
  }
};

/**
 * Directives[Const] : Directive[?Const]+
 */
let parseDirectives = (
  lexer: Lexer.t,
  isConst: bool
): array(Type.Ast.directiveNode) => {
  let rec parseDirectivesInternal = (directives) => {
    switch(peek(lexer, Type.Token.At)) {
    | true => parseDirectivesInternal(
      Array.append(directives, [|parseDirective(lexer, isConst)|])
    )
    | false => directives;
    }
  };

  parseDirectivesInternal([||])
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

  let arguments = parseArguments(lexer, false);
  let directives = parseDirectives(lexer, false);
  let selectionSet = peek(lexer, Type.Token.BraceLeft) 
    ? Some(parseSelectionSet(lexer))
    : None;

  Type.Ast.FieldNode(
    loc(lexer, start),
    alias,
    name,
    arguments,
    directives,
    selectionSet
  );
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

  let hasTypeCondition = expectOptionalKeyword(lexer, "on");
  let peekName = peek(lexer, Type.Token.Name);
  switch(hasTypeCondition, peekName) {
  | (None, true) => {
    let name = parseFragmentName(lexer);
    let directives = parseDirectives(lexer, false);
    FragmentSpreadNode(
      loc(lexer, start),
      name,
      directives
    )
  }
  | (_, _) => {
    let typeCondition = switch(hasTypeCondition) {
    | Some(_v) => Some(parseNamedType(lexer))
    | None => None
    };
    let directives = parseDirectives(lexer, false);
    let selectionSet = parseSelectionSet(lexer);

    InlineFragmentNode(
      loc(lexer, start),
      typeCondition,
      directives,
      selectionSet
    )
  }
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
  let selections = many(
    lexer, 
    Type.Token.BraceLeft, 
    parseSelection, 
    Type.Token.BraceRight
  );

  {
    selections,
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
  let defaultValue = switch(expectOptionalToken(lexer, Type.Token.Equals)) {
    | Some(_) => Some(parseValueLiteral(lexer, true))
    | None => None
    };
  let directives = parseDirectives(lexer, true);

  {
    variable,
    type_,
    defaultValue,
    directives,
    loc: loc(lexer, start),
  }
};

/**
 * VariableDefinitions : ( VariableDefinition+ )
 */
let parseVariableDefinitions = (
  lexer: Lexer.t
): array(Type.Ast.variableDefinitionNode) => {
  peek(lexer, Type.Token.ParenLeft)
  ? many(lexer, Type.Token.ParenLeft, parseVariableDefinition, Type.Token.ParenRight)
  : [||]
};

let parseOperationDefinition = (
  lexer: Lexer.t
): Type.Ast.operationDefinitionNode => {
  let start = lexer.token^;
  switch(peek(lexer, Type.Token.BraceLeft)) {
  | true => {
    operation: Query,
    name: None,
    variableDefinitions: [||],
    directives: [||],
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
    let variableDefinitions = parseVariableDefinitions(lexer);
    let directives = parseDirectives(lexer, false);
    let selectionSet = parseSelectionSet(lexer);

    {
      operation,
      name,
      variableDefinitions,
      directives,
      selectionSet,
      loc: loc(lexer, start),
    }
  }
  }
};

/**
 * FragmentDefinition :
 *   - fragment FragmentName on TypeCondition Directives? SelectionSet
 *
 * TypeCondition : NamedType
 * 
 * Experimental support for defining variables within fragments changes
 * the grammar of FragmentDefinition:
 *   - fragment FragmentName VariableDefinitions? on TypeCondition Directives? SelectionSet
 */
let parseFragmentDefinition = (
  lexer: Lexer.t
): Type.Ast.fragmentDefinitionNode => {
  let start = lexer.token^;
  ignore(expectKeyword(lexer, "fragment"));

  let name = parseFragmentName(lexer);
  let variableDefinitions = switch(lexer.options.experimentalFragmentVariables) {
  | true => parseVariableDefinitions(lexer)
  | false => {
    /** 
     * NOTE: In graphql-js, it's just skipped when experimentalFragmentVariables is false. 
     * But I think showing the users helpful message is better idea. 
     */ 
    switch(peek(lexer, Type.Token.ParenLeft)){
    | true => Error.API.syntaxError(
      lexer.source,
      lexer.token^.start,
      "Fragment variables are not allowed. If you want to use it, pass experimentalFragmentVariables option to parser"
    );
    | false => [||]
    }
  }
  };
  ignore(expectKeyword(lexer, "on"));
  let typeCondition = parseNamedType(lexer);
  let directives = parseDirectives(lexer, false);
  let selectionSet = parseSelectionSet(lexer);

  {
    name,
    variableDefinitions,
    typeCondition,
    directives,
    selectionSet,
    loc: loc(lexer, start),
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
      | "fragment"
        => Type.Ast.FragmentDefinitionNode(parseFragmentDefinition(lexer))
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

 
// Implements the parsing rules in the Type Definition section.

let peekDescription = (
  lexer: Lexer.t
): bool => {
  peek(lexer, Type.Token.String) || peek(lexer, Type.Token.BlockString);
};

/**
 * Description : StringValue
 */
let parseDescription = (
  lexer: Lexer.t
): option(Type.Ast.stringValueNode) => {
  switch(peekDescription(lexer)) {
  | true => Some(parseStringLiteral(lexer) |> Type.Ast.toStringValueNode)
  | false => None
  };
};

/**
 * OperationTypeDefinition : OperationType : NamedType
 */
let parseOperationTypeDefinition = (
  lexer: Lexer.t
): Type.Ast.operationTypeDefinitionNode => {
  let start = lexer.token^;
  let operation = parseOperationType(lexer);
  ignore(expectToken(lexer, Type.Token.Colon));
  let type_ = parseNamedType(lexer);

  {
    operation,
    type_,
    loc: loc(lexer, start),
  }
};

/**
 * SchemaDefinition : schema Directives[Const]? { OperationTypeDefinition+ }
 */
let parseSchemaDefinition = (
  lexer: Lexer.t
): Type.Ast.schemaDefinitionNode => {
  let start = lexer.token^;
  ignore(expectKeyword(lexer, "schema"));
  let directives = parseDirectives(lexer, true);
  let operationTypes = many(
    lexer,
    Type.Token.BraceLeft,
    parseOperationTypeDefinition,
    Type.Token.BraceRight
  );

  {
    directives,
    operationTypes,
    loc: loc(lexer, start),
  }
};

/**
 * ScalarTypeDefinition : Description? scalar Name Directives[Const]?
 */
let parseScalarTypeDefinition = (
  lexer: Lexer.t
): Type.Ast.scalarTypeDefinitionNode => {
  let start = lexer.token^;
  let description = parseDescription(lexer);
  ignore(expectKeyword(lexer, "scalar"));
  let name = parseName(lexer);
  let directives = parseDirectives(lexer, true);

  {
    description,
    name, 
    directives, 
    loc: loc(lexer, start),
  }
};

/**
 * ImplementsInterfaces :
 *   - implements `&`? NamedType
 *   - ImplementsInterfaces & NamedType
 */
let parseImplementsInterfaces = (
  lexer: Lexer.t
): array(Type.Ast.namedTypeNode) => {
  switch(expectOptionalKeyword(lexer, "implements")) {
  | Some(_) => {
    // optional leading ampersand
    ignore(expectOptionalToken(lexer, Type.Token.Amp));

    let rec parseImplementsInterfacesInternal = types => {
      let n = Array.append(types, [|parseNamedType(lexer)|]);

      let token = expectOptionalToken(lexer, Type.Token.Amp);
      let oldSDL = lexer.options.allowLegacySDLImplementsInterfaces &&
        peek(lexer, Type.Token.Name);
      
      switch(token, oldSDL) {
      | (Some(_), _) | (_, true) => parseImplementsInterfacesInternal(n);
      | _ => n;
      }
    };

    parseImplementsInterfacesInternal([||]);
  }
  | None => [||]
  }
};

/**
 * InputValueDefinition :
 *   - Description? Name : Type DefaultValue? Directives[Const]?
 */
let parseInputValueDef = (
  lexer: Lexer.t
): Type.Ast.inputValueDefinitionNode => {
  let start = lexer.token^;
  let description = parseDescription(lexer);
  let name = parseName(lexer);
  ignore(expectToken(lexer, Type.Token.Colon));
  let type_ = parseTypeReference(lexer);
  let defaultValue = switch(expectOptionalToken(lexer, Type.Token.Equals)) {
  | Some(_) => Some(parseConstValue(lexer));
  | None => None
  };
  let directives = parseDirectives(lexer, true);

  {
    description,
    name,
    type_,
    defaultValue,
    directives,
    loc: loc(lexer, start),
  }
};

/**
 * ArgumentsDefinition : ( InputValueDefinition+ )
 */
let parseArgumentDefs = (
  lexer: Lexer.t
): array(Type.Ast.inputValueDefinitionNode) => {
  peek(lexer, Type.Token.ParenLeft)
  ? many(lexer, Type.Token.ParenLeft, parseInputValueDef, Type.Token.ParenRight)
  : [||]
};

/**
 * FieldDefinition :
 *   - Description? Name ArgumentsDefinition? : Type Directives[Const]?
 */
let parseFieldDefinition = (
  lexer: Lexer.t
): Type.Ast.fieldDefinitionNode => {
  let start = lexer.token^;
  let description = parseDescription(lexer);
  let name = parseName(lexer);
  let arguments = parseArgumentDefs(lexer);
  ignore(expectToken(lexer, Type.Token.Colon));
  let type_ = parseTypeReference(lexer);
  let directives = parseDirectives(lexer, true);

  {
    description,
    name,
    arguments,
    type_,
    directives,
    loc: loc(lexer, start),
  }
};

/**
 * FieldsDefinition : { FieldDefinition+ }
 */
let parseFieldsDefinition = (
  lexer: Lexer.t
): array(Type.Ast.fieldDefinitionNode) => {
  let isEmpty = lexer.options.allowLegacySDLEmptyFields &&
    peek(lexer, Type.Token.BraceLeft) &&
    Lexer.lookahead(lexer).kind == Type.Token.BraceRight;

  switch(isEmpty) {
  | true => {
    ignore(Lexer.lookahead(lexer));
    ignore(Lexer.lookahead(lexer));
    [||]
  }
  | false => {
    peek(lexer, Type.Token.BraceLeft)
    ? many(lexer, Type.Token.BraceLeft, parseFieldDefinition, Type.Token.BraceRight)
    : [||]
  }
  }
};

/**
 * ObjectTypeDefinition :
 *   Description?
 *   type Name ImplementsInterfaces? Directives[Const]? FieldsDefinition?
 */
let parseObjectTypeDefinition = (
  lexer: Lexer.t
): Type.Ast.objectTypeDefinitionNode => {
  let start = lexer.token^;
  let description = parseDescription(lexer);
  ignore(expectKeyword(lexer, "type"));
  let name = parseName(lexer);
  let interfaces = parseImplementsInterfaces(lexer);
  let directives = parseDirectives(lexer, true);
  let fields = parseFieldsDefinition(lexer);

  {
    description,
    name, 
    interfaces,
    directives,
    fields,
    loc: loc(lexer, start),
  }
};

/**
 * InterfaceTypeDefinition :
 *   - Description? interface Name Directives[Const]? FieldsDefinition?
 */
let parseInterfaceTypeDefinition = (
  lexer: Lexer.t
): Type.Ast.interfaceTypeDefinitionNode => {
  let start = lexer.token^;
  let description = parseDescription(lexer);
  ignore(expectKeyword(lexer, "interface"));
  let name = parseName(lexer);
  let directives = parseDirectives(lexer, true);
  let fields = parseFieldsDefinition(lexer);

  {
    description,
    name,
    directives,
    fields,
    loc: loc(lexer, start),
  }
};

/**
 * UnionMemberTypes :
 *   - = `|`? NamedType
 *   - UnionMemberTypes | NamedType
 */
let parseUnionMemberTypes = (
  lexer: Lexer.t
): array(Type.Ast.namedTypeNode) => {
  switch(expectOptionalToken(lexer, Type.Token.Equals)) {
  | Some(_) => {
    // optional leading pipe
    ignore(expectOptionalToken(lexer, Type.Token.Pipe));

    let rec parseUnionMemberTypesInternal = members => {
      let n = Array.append(members, [|parseNamedType(lexer)|]);

      switch(expectOptionalToken(lexer, Type.Token.Pipe)) {
      | Some(_) => parseUnionMemberTypesInternal(n)
      | None => n;
      }
    };

    parseUnionMemberTypesInternal([||]);
  }
  | None => [||]
  }
};

/**
 * UnionTypeDefinition :
 *   - Description? union Name Directives[Const]? UnionMemberTypes?
 */
let parseUnionTypeDefinition = (
  lexer: Lexer.t
): Type.Ast.unionTypeDefinitionNode => {
  let start = lexer.token^;
  let description = parseDescription(lexer);
  ignore(expectKeyword(lexer, "union"));
  let name = parseName(lexer);
  let directives = parseDirectives(lexer, true);
  let types = parseUnionMemberTypes(lexer);

  {
    description,
    name,
    directives,
    types,
    loc: loc(lexer, start),
  }
};

/**
 * EnumValueDefinition : Description? EnumValue Directives[Const]?
 *
 * EnumValue : Name
 */
let parseEnumValueDefinition = (
  lexer: Lexer.t
): Type.Ast.enumValueDefinitionNode => {
  let start = lexer.token^;
  let description = parseDescription(lexer);
  let name = parseName(lexer);
  let directives = parseDirectives(lexer, true);

  {
    description,
    name,
    directives,
    loc: loc(lexer, start),
  }
};

/**
 * EnumValuesDefinition : { EnumValueDefinition+ }
 */
let parseEnumValuesDefinition = (
  lexer: Lexer.t
): array(Type.Ast.enumValueDefinitionNode) => {
  peek(lexer, Type.Token.BraceLeft)
  ? many(lexer, Type.Token.BraceLeft, parseEnumValueDefinition, Type.Token.BraceRight)
  : [||]
};

/**
 * EnumTypeDefinition :
 *   - Description? enum Name Directives[Const]? EnumValuesDefinition?
 */
let parseEnumTypeDefinition = (
  lexer: Lexer.t
): Type.Ast.enumTypeDefinitionNode => {
  let start = lexer.token^;
  let description = parseDescription(lexer);
  ignore(expectKeyword(lexer, "enum"));
  let name = parseName(lexer);
  let directives = parseDirectives(lexer, true);
  let values = parseEnumValuesDefinition(lexer);

  {
    description,
    name,
    directives,
    values,
    loc: loc(lexer, start),
  }
};

/**
 * InputFieldsDefinition : { InputValueDefinition+ }
 */
let parseInputFieldsDefinition = (
  lexer: Lexer.t
): array(Type.Ast.inputValueDefinitionNode) => {
  peek(lexer, Type.Token.BraceLeft)
  ? many(lexer, Type.Token.BraceLeft, parseInputValueDef, Type.Token.BraceRight)
  : [||]
};

/**
 * InputObjectTypeDefinition :
 *   - Description? input Name Directives[Const]? InputFieldsDefinition?
 */
let parseInputObjectTypeDefinition = (
  lexer: Lexer.t
): Type.Ast.inputObjectTypeDefinitionNode => {
  let start = lexer.token^;
  let description = parseDescription(lexer);
  ignore(expectKeyword(lexer, "input"));
  let name = parseName(lexer);
  let directives = parseDirectives(lexer, true);
  let fields = parseInputFieldsDefinition(lexer);

  {
    description,
    name,
    directives,
    fields,
    loc: loc(lexer, start),
  }
};

/** 
 * TypeDefinition :
 *   - ScalarTypeDefinition
 *   - ObjectTypeDefinition
 *   - InterfaceTypeDefinition
 *   - UnionTypeDefinition
 *   - EnumTypeDefinition
 *   - InputObjectTypeDefinition
 * 
 * NOTE: In graphql-js, parseTypeSystemDefinition and parseTypeDefinition are 
 * in the same function. But in our graphql-reason, it doesn't look good because
 * one function cannot return 2 different types. 
 */
let parseTypeDefinition = (
  lexer: Lexer.t
): Type.Ast.typeDefinitionNode => {
  let start = lexer.token^;

  switch(Type.Token.unwrapValue(start)) {
  | "scalar" => Type.Ast.ScalarTypeDefinitionNode(parseScalarTypeDefinition(lexer))
  | "type" => Type.Ast.ObjectTypeDefinitionNode(parseObjectTypeDefinition(lexer))
  | "interface" => Type.Ast.InterfaceTypeDefinitionNode(parseInterfaceTypeDefinition(lexer))
  | "union" => Type.Ast.UnionTypeDefinitionNode(parseUnionTypeDefinition(lexer))
  | "enum" => Type.Ast.EnumTypeDefinitionNode(parseEnumTypeDefinition(lexer))
  | "input" => Type.Ast.InputObjectTypeDefinitionNode(parseInputObjectTypeDefinition(lexer))
  | _ => unexpectedToken(lexer, start)
  };
};

/*
 * DirectiveLocation :
 *   - ExecutableDirectiveLocation
 *   - TypeSystemDirectiveLocation
 *
 * ExecutableDirectiveLocation : one of
 *   `QUERY`
 *   `MUTATION`
 *   `SUBSCRIPTION`
 *   `FIELD`
 *   `FRAGMENT_DEFINITION`
 *   `FRAGMENT_SPREAD`
 *   `INLINE_FRAGMENT`
 *
 * TypeSystemDirectiveLocation : one of
 *   `SCHEMA`
 *   `SCALAR`
 *   `OBJECT`
 *   `FIELD_DEFINITION`
 *   `ARGUMENT_DEFINITION`
 *   `INTERFACE`
 *   `UNION`
 *   `ENUM`
 *   `ENUM_VALUE`
 *   `INPUT_OBJECT`
 *   `INPUT_FIELD_DEFINITION`
 */
let parseDirectiveLocation = (
  lexer: Lexer.t
): Type.Ast.directiveLocationNode => {
  let start = lexer.token^;
  let name = parseName(lexer);

  switch(Type.DirectiveLocation.t_of_string(name.value)) {
  | exception Type.DirectiveLocation.UnexpectedLocation 
    => unexpectedToken(lexer, start)
  | t => {
    locationType: t,
    loc: name.loc,
  }
  }
};

/**
 * DirectiveLocations :
 *   - `|`? DirectiveLocation
 *   - DirectiveLocations | DirectiveLocation
 */
let parseDirectiveLocations = (
  lexer: Lexer.t
): array(Type.Ast.directiveLocationNode) => {
  // optional leading pipe
  ignore(expectOptionalToken(lexer, Type.Token.Pipe));
  
  let rec parseDirectiveLocationsInternal = locations => {
    let n = Array.append(locations, [|parseDirectiveLocation(lexer)|])

    switch(expectOptionalToken(lexer, Type.Token.Pipe)) {
    | Some(_) => parseDirectiveLocationsInternal(n)
    | None => n
    }
  };

  parseDirectiveLocationsInternal([||]);
};

/**
 * DirectiveDefinition :
 *   - Description? directive @ Name ArgumentsDefinition? on DirectiveLocations
 */
let parseDirectiveDefinition = (
  lexer: Lexer.t
): Type.Ast.directiveDefinitionNode => {
  let start = lexer.token^;
  let description = parseDescription(lexer);
  ignore(expectKeyword(lexer, "directive"));
  ignore(expectToken(lexer, Type.Token.At));
  let name = parseName(lexer);
  let arguments = parseArgumentDefs(lexer);
  ignore(expectKeyword(lexer, "on"));
  let locations = parseDirectiveLocations(lexer);

  {
    description,
    name,
    arguments,
    locations,
    loc: loc(lexer, start),
  }
};

/**
 * TypeSystemDefinition :
 *   - SchemaDefinition
 *   - TypeDefinition
 *   - DirectiveDefinition
 */
let parseTypeSystemDefinition = (
  lexer: Lexer.t
): Type.Ast.typeSystemDefinitionNode => {
  let keywordToken = peekDescription(lexer) 
    ? lexer |> Lexer.lookahead
    : lexer.token^;
  
  switch(keywordToken.kind) {
  | Type.Token.Name => {
    switch(Type.Token.unwrapValue(keywordToken)) {
    | "schema" 
      => Type.Ast.SchemaDefinitionNode(parseSchemaDefinition(lexer))
    | "scalar" | "type" | "interface"
    | "union" | "enum" | "input" 
      => Type.Ast.TypeDefinitionNode(parseTypeDefinition(lexer))
    | "directive" 
      => Type.Ast.DirectiveDefinitionNode(parseDirectiveDefinition(lexer))
    | _ => unexpectedToken(lexer, keywordToken)
    }
  }
  | _ => unexpectedToken(lexer, keywordToken)
  }
};

/**
 * SchemaExtension :
 *  - extend schema Directives[Const]? { OperationTypeDefinition+ }
 *  - extend schema Directives[Const]
 */
let parseSchemaExtension = (
  lexer: Lexer.t
): Type.Ast.schemaExtensionNode => {
  let start = lexer.token^;
  ignore(expectKeyword(lexer, "extend"));
  ignore(expectKeyword(lexer, "schema"));
  let directives = parseDirectives(lexer, true);
  let operationTypes = 
    peek(lexer, Type.Token.BraceLeft)
    ? many(lexer, Type.Token.BraceLeft, parseOperationTypeDefinition, Type.Token.BraceRight)
    : [||]
  
  switch(Array.length(directives), Array.length(operationTypes)) {
  | (0, 0) => unexpected(lexer)
  | (_, _) => {
    directives,
    operationTypes,
    loc: loc(lexer, start),
  }
  }
};

/**
 * ScalarTypeExtension :
 *   - extend scalar Name Directives[Const]
 */
let parseScalarTypeExtension = (
  lexer: Lexer.t
): Type.Ast.scalarTypeExtensionNode => {
  let start = lexer.token^;
  ignore(expectKeyword(lexer, "extend"));
  ignore(expectKeyword(lexer, "scalar"));
  let name = parseName(lexer);
  let directives = parseDirectives(lexer, true);
  
  switch(Array.length(directives)) {
  | 0 => unexpected(lexer)
  | _ => {
    name,
    directives,
    loc: loc(lexer, start),
  }
  }
};

/**
 * ObjectTypeExtension :
 *  - extend type Name ImplementsInterfaces? Directives[Const]? FieldsDefinition
 *  - extend type Name ImplementsInterfaces? Directives[Const]
 *  - extend type Name ImplementsInterfaces
 */
let parseObjectTypeExtension = (
  lexer: Lexer.t
): Type.Ast.objectTypeExtensionNode => {
  let start = lexer.token^;
  ignore(expectKeyword(lexer, "extend"));
  ignore(expectKeyword(lexer, "type"));
  let name = parseName(lexer);
  let interfaces = parseImplementsInterfaces(lexer);
  let directives = parseDirectives(lexer, true);
  let fields = parseFieldsDefinition(lexer);
  
  switch(Array.length(interfaces), Array.length(directives), Array.length(fields)) {
  | (0, 0, 0) => unexpected(lexer)
  | (_, _, _) => {
    name, 
    interfaces,
    directives,
    fields,
    loc: loc(lexer, start),
  }
  }
};

/**
 * InterfaceTypeExtension :
 *   - extend interface Name Directives[Const]? FieldsDefinition
 *   - extend interface Name Directives[Const]
 */
let parseInterfaceTypeExtension = (
  lexer: Lexer.t
): Type.Ast.interfaceTypeExtensionNode => {
  let start = lexer.token^;
  ignore(expectKeyword(lexer, "extend"));
  ignore(expectKeyword(lexer, "interface"));
  let name = parseName(lexer);
  let directives = parseDirectives(lexer, true);
  let fields = parseFieldsDefinition(lexer);
  
  switch(Array.length(directives), Array.length(fields)) {
  | (0, 0) => unexpected(lexer)
  | (_, _) => {
    name,
    directives,
    fields,
    loc: loc(lexer, start),
  }
  }
};

/**
 * UnionTypeExtension :
 *   - extend union Name Directives[Const]? UnionMemberTypes
 *   - extend union Name Directives[Const]
 */
let parseUnionTypeExtension = (
  lexer: Lexer.t
): Type.Ast.unionTypeExtensionNode => {
  let start = lexer.token^;
  ignore(expectKeyword(lexer, "extend"));
  ignore(expectKeyword(lexer, "union"));
  let name = parseName(lexer);
  let directives = parseDirectives(lexer, true);
  let types = parseUnionMemberTypes(lexer);
  
  switch(Array.length(directives), Array.length(types)) {
  | (0, 0) => unexpected(lexer)
  | (_, _) => {
    name,
    directives,
    types,
    loc: loc(lexer, start),
  }
  }
};

/**
 * EnumTypeExtension :
 *   - extend enum Name Directives[Const]? EnumValuesDefinition
 *   - extend enum Name Directives[Const]
 */
let parseEnumTypeExtension = (
  lexer: Lexer.t
): Type.Ast.enumTypeExtensionNode => {
  let start = lexer.token^;
  ignore(expectKeyword(lexer, "extend"));
  ignore(expectKeyword(lexer, "enum"));
  let name = parseName(lexer);
  let directives = parseDirectives(lexer, true);
  let values = parseEnumValuesDefinition(lexer);
  
  switch(Array.length(directives), Array.length(values)) {
  | (0, 0) => unexpected(lexer)
  | (_, _) => {
    name,
    directives,
    values,
    loc: loc(lexer, start),
  }
  }
};

/**
 * InputObjectTypeExtension :
 *   - extend input Name Directives[Const]? InputFieldsDefinition
 *   - extend input Name Directives[Const]
 */
let parseInputObjectTypeExtension = (
  lexer: Lexer.t
): Type.Ast.inputObjectTypeExtensionNode => {
  let start = lexer.token^;
  ignore(expectKeyword(lexer, "extend"));
  ignore(expectKeyword(lexer, "input"));
  let name = parseName(lexer);
  let directives = parseDirectives(lexer, true);
  let fields = parseInputFieldsDefinition(lexer);
  
  switch(Array.length(directives), Array.length(fields)) {
  | (0, 0) => unexpected(lexer)
  | (_, _) => {
    name,
    directives,
    fields,
    loc: loc(lexer, start),
  }
  }
};

/**
 * TypeExtension :
 *   - ScalarTypeExtension
 *   - ObjectTypeExtension
 *   - InterfaceTypeExtension
 *   - UnionTypeExtension
 *   - EnumTypeExtension
 *   - InputObjectTypeExtension
 *
 * NOTE: Like parseTypeDefinition, parseTypeSystemExtension and parseTypeExtension are 
 * in the same function in graphql-js. But in graphql-reason, it doesn't look good because
 * one function cannot return 2 different types. 
 */
let parseTypeExtension = (
  lexer: Lexer.t
): Type.Ast.typeExtensionNode => {
  let start = lexer.token^;

  switch(Type.Token.unwrapValue(start)) {
  | "scalar" => Type.Ast.ScalarTypeExtensionNode(parseScalarTypeExtension(lexer))
  | "type" => Type.Ast.ObjectTypeExtensionNode(parseObjectTypeExtension(lexer))
  | "interface" => Type.Ast.InterfaceTypeExtensionNode(parseInterfaceTypeExtension(lexer))
  | "union" => Type.Ast.UnionTypeExtensionNode(parseUnionTypeExtension(lexer))
  | "enum" => Type.Ast.EnumTypeExtensionNode(parseEnumTypeExtension(lexer))
  | "input" => Type.Ast.InputObjectTypeExtensionNode(parseInputObjectTypeExtension(lexer))
  | _ => unexpectedToken(lexer, start)
  };
};

/**
 * TypeSystemExtension :
 *   - SchemaExtension
 *   - TypeExtension
 */
let parseTypeSystemExtension = (
  lexer: Lexer.t
): Type.Ast.typeSystemExtensionNode => {
  let keywordToken = lexer |> Lexer.lookahead;

  switch(keywordToken.kind) {
  | Type.Token.Name => {
    switch(Type.Token.unwrapValue(keywordToken)){
    | "schema" 
      => Type.Ast.SchemaExtensionNode(parseSchemaExtension(lexer))
    | "scalar" | "type" | "interface"
    | "union" | "enum" | "input"
      => Type.Ast.TypeExtensionNode(parseTypeExtension(lexer))
    | _ => unexpectedToken(lexer, keywordToken)
    }
  }
  | _ => unexpectedToken(lexer, keywordToken)
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
        => Ast.ExecutableDefinitionNode(parseExecutableDefinition(lexer));
      | "schema" | "scalar" | "type" | "interface"
      | "union" | "enum" | "input" | "directive"
        => Ast.TypeSystemDefinitionNode(parseTypeSystemDefinition(lexer));
      | "extend"
        => Ast.TypeSystemExtensionNode(parseTypeSystemExtension(lexer));
      | _ => unexpected(lexer);
      }
    }
    | None => unexpected(lexer);
    }
  }
  | Token.BraceLeft
    => Type.Ast.ExecutableDefinitionNode(parseExecutableDefinition(lexer));
  | Token.String | Token.BlockString
    => Type.Ast.TypeSystemDefinitionNode(parseTypeSystemDefinition(lexer));
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
 * NOTE: 
 * 1. parse function in graphql-js is broken into 2 functions. 
 * Because you can call parse() with a simple string or a Source object. 
 * 2. These functions don't check validity of arguments at runtime. 
 * Because they're checked by compiler. 
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
