let syntaxError = (
  source: Util.Source.t,
  position: int,
  description: string,
) => {
  raise(GraphQLError.Exception(GraphQLError.make("Syntax Error: " ++ description)));
};