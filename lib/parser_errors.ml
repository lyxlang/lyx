
(* This file was auto-generated based on "lib/parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 207 ->
        "Unexpected token after declaration. Expected another declaration or end of file.\n"
    | 200 ->
        "Expected either a comma \226\128\152,\226\128\153 for tuple elements or closing parenthesis \226\128\152)\226\128\153 to complete the expression.\n"
    | 198 ->
        "Expected closing parenthesis \226\128\152)\226\128\153 to end the tuple or grouped expression. Found \226\128\152]\226\128\153 instead.\n"
    | 196 ->
        "Expected another expression after comma in list literal.\n"
    | 195 ->
        "Expected either a comma \226\128\152,\226\128\153 for more list items or closing bracket \226\128\152]\226\128\153 to complete the list.\n"
    | 193 ->
        "Expected closing bracket \226\128\152]\226\128\153 to end the list literal. Found \226\128\152)\226\128\153 instead.\n"
    | 190 ->
        "Expected either another pattern case or closing brace \226\128\152}\226\128\153 to end the match expression.\n"
    | 186 ->
        "Expected a semicolon \226\128\152;\226\128\153 after match case body.\n"
    | 185 ->
        "Expected an expression for the match case body after \226\128\152->\226\128\153.\n"
    | 183 ->
        "Expected a semicolon \226\128\152;\226\128\153 after match case body.\n"
    | 182 ->
        "Expected an expression for the match case body after \226\128\152->\226\128\153.\n"
    | 181 ->
        "Expected \226\128\152->\226\128\153 after guard condition in match case.\n"
    | 180 ->
        "Expected a guard expression after \226\128\152if\226\128\153 in match case. This expression must evaluate to a boolean.\n"
    | 179 ->
        "Expected \226\128\152->\226\128\153 or \226\128\152if\226\128\153 after pattern in match case.\n"
    | 176 ->
        "Expected \226\128\152->\226\128\153 or more pattern parameters for constructor pattern.\n"
    | 174 ->
        "Expected closing parenthesis \226\128\152)\226\128\153 after pattern or comma for tuple pattern.\n"
    | 172 ->
        "Expected closing parenthesis \226\128\152)\226\128\153 after tuple pattern.\n"
    | 168 ->
        "Expected another pattern after comma in pattern list.\n"
    | 167 ->
        "Expected \226\128\152,\226\128\153 for more pattern items or closing delimiter for pattern list.\n"
    | 162 ->
        "Expected another pattern after \226\128\152;\226\128\153 in pattern alternative.\n"
    | 161 ->
        "Expected \226\128\152->\226\128\153 after pattern or \226\128\152;\226\128\153 to start alternative pattern.\n"
    | 159 ->
        "Expected closing bracket \226\128\152]\226\128\153 after list spread pattern.\n"
    | 158 ->
        "Expected either \226\128\152]\226\128\153 to close list pattern or \226\128\152...\226\128\153 for matching rest of list.\n"
    | 153 ->
        "Expected patterns or \226\128\152]\226\128\153 for an empty list pattern.\n"
    | 152 ->
        "Expected pattern inside parentheses.\n"
    | 150 ->
        "Expected \226\128\152->\226\128\153, \226\128\152if\226\128\153, or pattern parameters after constructor in pattern.\n"
    | 149 ->
        "Expected at least one case pattern in match expression.\n"
    | 148 ->
        "Expected \226\128\152{\226\128\153 after expression in match statement.\n"
    | 146 ->
        "Expected another binding after \226\128\152;\226\128\153 in let expression.\n"
    | 145 ->
        "Expected \226\128\152in\226\128\153 after binding(s), or \226\128\152;\226\128\153 to add more bindings.\n"
    | 143 ->
        "Expected an expression after \226\128\152in\226\128\153 in a let expression.\n"
    | 139 ->
        "Expected an expression for the \226\128\152else\226\128\153 branch of an if expression.\n"
    | 138 ->
        "Expected \226\128\152else\226\128\153 after the \226\128\152then\226\128\153 branch of an if expression.\n"
    | 137 ->
        "Expected an expression for the \226\128\152then\226\128\153 branch of an if expression.\n"
    | 136 ->
        "Expected \226\128\152then\226\128\153 after the condition in an if expression.\n"
    | 132 ->
        "Expected right operand for logical operator.\n"
    | 126 ->
        "Expected right operand for comparison operator.\n"
    | 115 ->
        "Expected right operand for pipe operator \226\128\152\226\150\183\226\128\153.\n"
    | 111 ->
        "Expected right operand for addition/subtraction operator.\n"
    | 107 ->
        "Expected right operand for list concatenation operator \226\128\152++\226\128\153.\n"
    | 104 ->
        "Expected right operand for multiplication operator.\n"
    | 97 ->
        "Unexpected token in expression. Function application must have arguments.\n"
    | 95 ->
        "Expected right operand for exponentiation operation.\n"
    | 92 ->
        "Unexpected token after binary expression. Consider adding parentheses to clarify precedence.\n"
    | 90 ->
        "Expected right operand for bitwise operation.\n"
    | 83 ->
        "Expected an expression after unary operator.\n"
    | 82 ->
        "Expected body expression after \226\128\152->\226\128\153 in lambda expression.\n"
    | 79 ->
        "Expected \226\128\152->\226\128\153 after parameters in lambda expression.\n"
    | 78 ->
        "Expected at least one parameter after \226\128\152\\\226\128\153 for lambda expression.\n"
    | 73 ->
        "Expected a condition expression after \226\128\152if\226\128\153 keyword.\n"
    | 72 ->
        "Expected an expression after \226\128\152=\226\128\153 in let binding.\n"
    | 71 ->
        "Expected \226\128\152=\226\128\153 after binding name and optional type annotation.\n"
    | 69 ->
        "Expected \226\128\152=\226\128\153 after name in let binding, or parameters if defining a local function.\n"
    | 68 ->
        "Expected binding(s) after \226\128\152let\226\128\153 keyword. Bindings should be of form \226\128\152name = expression\226\128\153.\n"
    | 67 ->
        "Expected an expression to match against after \226\128\152match\226\128\153 keyword.\n"
    | 66 ->
        "Expected expressions separated by commas or closing bracket \226\128\152]\226\128\153 for an empty list.\n"
    | 64 ->
        "Expected an expression or a comma-separated list of expressions inside parentheses.\n"
    | 58 ->
        "Expected an expression for the function body.\n"
    | 57 ->
        "Expected \226\128\152=\226\128\153 after function declaration to define the body.\n"
    | 55 ->
        "Expected type annotation after colon.\n"
    | 52 ->
        "Expected either a type annotation \226\128\152:\226\128\153 or \226\128\152=\226\128\153 followed by the function body.\n"
    | 50 ->
        "Expected another parameter after comma in tuple parameter.\n"
    | 49 ->
        "Expected either a comma \226\128\152,\226\128\153 for more parameters or closing parenthesis \226\128\152)\226\128\153 to complete the tuple parameter.\n"
    | 44 ->
        "Expected parameter(s) inside parentheses for a tuple parameter.\n"
    | 43 ->
        "Expected parameters (if any) followed by \226\128\152=\226\128\153 and an expression body for the definition.\n"
    | 38 ->
        "Expected either another variant definition (starting with an uppercase identifier) or \226\128\152}\226\128\153 to close the ADT definition.\n"
    | 36 ->
        "Expected a semicolon \226\128\152;\226\128\153 after variant definition.\n"
    | 34 ->
        "Expected optional type information followed by a semicolon \226\128\152;\226\128\153 after variant name.\n"
    | 33 ->
        "Expected at least one variant definition for an ADT. Each variant should start with an uppercase identifier.\n"
    | 29 ->
        "Expected \226\128\152{\226\128\153 after type parameters to begin ADT variant definitions.\n"
    | 28 ->
        "Expected \226\128\152{\226\128\153 after type parameters for an ADT definition, or use a valid type expression for a type alias.\n"
    | 22 ->
        "Expected either a comma \226\128\152,\226\128\153 for more tuple elements or closing parenthesis \226\128\152)\226\128\153 to complete the tuple type.\n"
    | 21 ->
        "Expected another type after comma in tuple type definition.\n"
    | 19 ->
        "Expected either a comma \226\128\152,\226\128\153 for tuple types or closing parenthesis \226\128\152)\226\128\153 for grouped types.\n"
    | 17 ->
        "Expected closing bracket \226\128\152]\226\128\153 after list type definition.\n"
    | 15 ->
        "Expected return type after \226\128\152->\226\128\153 in function type definition.\n"
    | 14 ->
        "Unexpected token after type. For function types, use the arrow \226\128\152->\226\128\153 syntax.\n"
    | 8 ->
        "Expected a type expression inside brackets for a list type, e.g., [Int] or [String].\n"
    | 6 ->
        "Expected a type expression inside parentheses. For tuple types, specify at least one type.\n"
    | 5 ->
        "Invalid type constructor application. Type arguments must be valid types.\n"
    | 4 ->
        "Expected either a type definition or type parameters (lowercase identifiers) for an ADT definition.\n"
    | 3 ->
        "Expected \226\128\152:=\226\128\153 after type name for a type definition.\n"
    | 1 ->
        "Expected an identifier after \226\128\152def\226\128\153. Use lowercase for functions/values or uppercase for types/ADTs.\n"
    | 0 ->
        "Expected a declaration starting with \226\128\152def\226\128\153 or a comment enclosed in backticks. Standalone expressions are not allowed at the top level.\n"
    | _ ->
        raise Not_found
