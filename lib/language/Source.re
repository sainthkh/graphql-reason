/*
NOTE: Content has been moved to util/Source.re to avoid circular dependency. 

Currently, Source is used in language and error. 
If we place Source inside language, then language needs error and error needs language. 
Only solution is to move it somewhere else. 

As Source is not GraphQL itself 
and it is a class to help solve the problems about the source file,
Util seems to be the best choice.
*/