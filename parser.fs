//
// Parser for simple C programs.  This component checks 
// the input program to see if it meets the syntax rules
// of simple C.  The parser returns a string denoting
// success or failure. 
//
// Returns: the string "success" if the input program is
// legal, otherwise the string "syntax_error: ..." is
// returned denoting an invalid simple C program.
//
// Sohum Bhole
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module parser =
  //
  // NOTE: all functions in the module must be indented.
  //

  //
  // matchToken
  //
  let private matchToken expected_token (tokens: string list) =
    //
    // if the next token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation
    // at the first error.
    //
    let nextToken = List.head tokens

    if expected_token = nextToken then  
      List.tail tokens
    elif nextToken.StartsWith(expected_token) then
      List.tail tokens      
    else
      failwith ("expecting " + expected_token + ", but found " + nextToken)

 
  let rec empty tokens = 

    matchToken ";" tokens

  let rec vardecl tokens =

     let inttoken = matchToken "int" tokens

     let tokenHead = List.head inttoken

     if tokenHead.StartsWith("identifier") then

        let tokenMatch = matchToken tokenHead inttoken

        matchToken ";" tokenMatch

     else

        failwith ("expecting statement, but found " + tokenHead) 

  let rec input tokens =

     let firstToken = matchToken "cin" tokens

     let tokenHead = List.head firstToken

     if tokenHead.StartsWith(">>") then

        let token_3 = matchToken tokenHead firstToken

        let nextToken = List.head token_3

        if nextToken.StartsWith("identifier") then

          let token_4 = matchToken nextToken token_3

          matchToken ";" token_4

        else

           failwith ("expecting identifier, but found " + nextToken) 

     else

        failwith ("expecting >>, but found " + tokenHead) 

  let rec exprValue tokens =

    let nextToken = List.head tokens

    if  nextToken = "true" || nextToken = "false" || string(nextToken).StartsWith("int_literal") || string(nextToken).StartsWith("identifier") || string(nextToken).StartsWith("str_literal")  then

       let T = matchToken nextToken tokens

       T

    else 
       failwith ("expecting identifier or literal, but found " + nextToken) 

  let rec outputValue tokens = 

     let token:string = List.head tokens

     if token = "endl" then

       let Ttoken = matchToken "endl" tokens

       Ttoken

     else

       let Ttoken2 = exprValue tokens

       Ttoken2

  let rec output tokens = 

    let tokenFirst = matchToken "cout" tokens

    let T1 = matchToken "<<" tokenFirst

    let T2 = outputValue T1

    let T3 = matchToken ";" T2

    T3

  let rec exprOp tokens = 

    let token = List.head tokens

    if token = "+" || token = "-" || token = "*" || token = "/" || token = "^" || token = "<" || token = "<=" || token = ">" || token = ">=" || token = "==" || token = "!=" then

      let T = matchToken token tokens

      T

    else

      failwith ("expecting operator, but found " + token)

  let rec expr tokens = 

    let T1 = exprValue tokens

    let token = List.head T1

    if token = "+" || token = "-" || token = "*" || token = "/" || token = "^" || token = "<" || token = "<=" || token = ">" || token = ">=" || token = "==" || token = "!=" then

      let T = exprOp T1

      let T2 = exprValue T

      T2

    else

      T1

  let rec assignment tokens = 
    let tokenFirst = matchToken (List.head tokens) tokens

    let T1 = matchToken "=" tokenFirst

    let T2 = expr T1

    let T3 = matchToken ";" T2

    T3



  let rec private stmt tokens = 

    let next = List.head tokens
    match tokens with
    | head::tail when head = ";" -> empty tokens
    | head::tail when head = "int" -> vardecl tokens
    | head::tail when head = "cin" -> input tokens
    | head::tail when head = "cout" -> output tokens
    | head::tail when head = "if" -> ifstmt tokens
    | head::tail -> if string(next).StartsWith("identifier") then assignment tokens else failwith("expecting statement, but found " + next)

  and private condition tokens = 
    let T1 = expr tokens
    T1

  and private then_part tokens =
    let T1 = stmt tokens
    T1

  and private else_part tokens =

    let T1 = List.head tokens
    if T1 = "else" then
      let T2 = matchToken "else" tokens
      let T3 = stmt T2
      T3

    else  

      tokens

  and private ifstmt tokens =

    let T1 = matchToken "if" tokens
    let T2 = matchToken "(" T1
    let T3 = condition T2
    let T4 = matchToken ")" T3
    let T5 = then_part T4
    let T6 = else_part T5
    T6



  let rec private morestmt tokens = 

    let nextToken = List.head tokens

    if nextToken = "int" || nextToken = "cin"|| nextToken = ";" || nextToken = "cout" || nextToken = "if" || string(nextToken).StartsWith("identifier") then

      let T = stmt tokens

      let TMore = morestmt T

      TMore

    else

      tokens



  let rec private stmts tokens = 

    let T1 = List.head tokens

    if T1 = "}" then 

       failwith("expecting statement, but found " + T1)

    else

    let T2 = stmt tokens

    let T3 = morestmt T2

    T3


  //
  // simpleC
  //
  let private simpleC tokens =

      let T2 = matchToken "void" tokens 

      let T3 = matchToken "main" T2

      let T4 = matchToken "(" T3

      let T5 = matchToken ")" T4

      let T6 = matchToken "{" T5

      let T7 = stmts T6

      let T8 = matchToken "}" T7

      let T9 = matchToken "$" T8

      T9


  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid simple C program.  Returns
  // the string "success" if valid, otherwise returns a 
  // string of the form "syntax_error:...".
  //
  let parse tokens = 
    try
      let result = simpleC tokens
      "success"
    with 
      | ex -> "syntax_error: " + ex.Message
