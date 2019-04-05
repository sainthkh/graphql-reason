open Ast;

let parseSource: Util.Source.t => Type.Ast.node = source => DocumentNode;
let parse: string => Type.Ast.node = (source) => DocumentNode;