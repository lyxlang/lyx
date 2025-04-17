
(* This file was auto-generated based on "lib/parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 202 ->
        "Unexpected token after a valid declaration. Expected another definition, a comment, or the end of the file.\n"
    | 196 ->
        "Invalid function definition. Expected the function body expression after the equals sign \226\128\156=\226\128\157.\n"
    | 195 ->
        "Invalid function definition. Expected an equals sign \226\128\156=\226\128\157 after the type signature.\n"
    | 194 ->
        "Invalid function definition. Expected a type signature \226\128\156:\226\128\157 or an equals sign \226\128\156=\226\128\157 after the parameters.\n"
    | 189 ->
        "Invalid typed expression. Expected a closing parenthesis \226\128\156)\226\128\157 after the type annotation.\n"
    | 187 ->
        "Invalid tuple expression. Expected a closing parenthesis \226\128\156)\226\128\157.\n"
    | 186 ->
        "Invalid tuple expression. Expected another expression after the comma \226\128\156,\226\128\157.\n"
    | 185 ->
        "Invalid parenthesized expression or tuple. Expected a comma \226\128\156,\226\128\157 followed by another expression, a type annotation \226\128\156:\226\128\157, or a closing parenthesis \226\128\156)\226\128\157.\n"
    | 181 ->
        "Invalid list literal. Expected another expression after the comma \226\128\156,\226\128\157.\n"
    | 180 ->
        "Invalid list literal. Expected a comma \226\128\156,\226\128\157 followed by another expression, or a closing square bracket \226\128\156]\226\128\157.\n"
    | 178 ->
        "Invalid list literal. Expected a closing square bracket \226\128\156]\226\128\157.\n"
    | 173 ->
        "Invalid match expression. Expected another case or a closing brace \226\128\156}\226\128\157.\n"
    | 172 ->
        "Invalid match case. Expected a semicolon \226\128\156;\226\128\157 after the result expression.\n"
    | 171 ->
        "Invalid match case. Expected the result expression after the arrow \226\128\156->\226\128\157.\n"
    | 170 ->
        "Invalid match case. Expected an arrow \226\128\156->\226\128\157 after the pattern or guard.\n"
    | 168 ->
        "Invalid match case guard. Expected a condition expression after \226\128\156if\226\128\157.\n"
    | 166 ->
        "Invalid OR-pattern. Expected another pattern after the semicolon \226\128\156;\226\128\157.\n"
    | 165 ->
        "Invalid match case. Expected an arrow \226\128\156->\226\128\157, a guard condition starting with \226\128\156if\226\128\157, or an alternative pattern separated by \226\128\156;\226\128\157.\n"
    | 160 ->
        "Invalid tuple pattern. Expected a closing parenthesis \226\128\156)\226\128\157.\n"
    | 158 ->
        "Invalid tuple pattern. Expected another pattern after the comma \226\128\156,\226\128\157.\n"
    | 157 ->
        "Invalid tuple pattern. Expected a comma \226\128\156,\226\128\157 followed by another pattern, or a closing parenthesis \226\128\156)\226\128\157.\n"
    | 153 ->
        "Invalid list pattern. Expected another pattern after the comma \226\128\156,\226\128\157.\n"
    | 152 ->
        "Invalid list pattern. Expected a comma \226\128\156,\226\128\157 followed by another pattern, ellipsis \226\128\156...\226\128\157, or a closing square bracket \226\128\156]\226\128\157.\n"
    | 150 ->
        "Invalid list spread pattern. Expected a closing square bracket \226\128\156]\226\128\157 after the rest identifier.\n"
    | 149 ->
        "Invalid list spread pattern. Expected an identifier for the rest of the list after \226\128\156...\226\128\157.\n"
    | 148 ->
        "Invalid list pattern. Expected ellipsis \226\128\156...\226\128\157 or a closing square bracket \226\128\156]\226\128\157.\n"
    | 144 ->
        "Invalid list pattern. Expected a pattern, ellipsis \226\128\156...\226\128\157, or a closing square bracket \226\128\156]\226\128\157.\n"
    | 142 ->
        "Invalid tuple pattern. Expected a pattern inside the parentheses \226\128\156()\226\128\157.\n"
    | 140 ->
        "Invalid constructor pattern. Expected a nested pattern or the end of the pattern part.\n"
    | 139 ->
        "Invalid match expression. Expected at least one pattern case inside the braces \226\128\156{}\226\128\157.\n"
    | 138 ->
        "Invalid match expression. Expected an opening brace \226\128\156{\226\128\157 after the matched expression.\n"
    | 136 ->
        "Invalid let expression. Expected the body expression after \226\128\156in\226\128\157.\n"
    | 133 ->
        "Invalid let expression. Expected another binding after the semicolon \226\128\156;\226\128\157.\n"
    | 132 ->
        "Invalid let expression. Expected \226\128\156in\226\128\157 or a semicolon \226\128\156;\226\128\157 followed by another binding.\n"
    | 130 ->
        "Invalid if expression. Expected the \226\128\156else\226\128\157 branch expression after \226\128\156else\226\128\157.\n"
    | 129 ->
        "Invalid if expression. Expected \226\128\156else\226\128\157 after the \226\128\156then\226\128\157 branch expression.\n"
    | 128 ->
        "Invalid if expression. Expected the \226\128\156then\226\128\157 branch expression after \226\128\156then\226\128\157.\n"
    | 127 ->
        "Invalid if expression. Expected \226\128\156then\226\128\157 after the condition.\n"
    | 122 ->
        "Invalid logical AND operation. Expected an expression after the \226\128\156&&\226\128\157 operator.\n"
    | 118 ->
        "Invalid inequality comparison. Expected an expression after the \226\128\156!=\226\128\157 operator.\n"
    | 116 ->
        "Invalid greater than or equal comparison. Expected an expression after the \226\128\156>=\226\128\157 operator.\n"
    | 114 ->
        "Invalid greater than comparison. Expected an expression after the \226\128\156>\226\128\157 operator.\n"
    | 112 ->
        "Invalid less than or equal comparison. Expected an expression after the \226\128\156<=\226\128\157 operator.\n"
    | 109 ->
        "Invalid less than comparison. Expected an expression after the \226\128\156<\226\128\157 operator.\n"
    | 106 ->
        "Invalid subtraction operation. Expected an expression after the \226\128\156-\226\128\157 operator.\n"
    | 104 ->
        "Invalid addition operation. Expected an expression after the \226\128\156+\226\128\157 operator.\n"
    | 102 ->
        "Invalid concatenation operation. Expected an expression after the \226\128\156++\226\128\157 operator.\n"
    | 100 ->
        "Invalid equality comparison. Expected an expression after the \226\128\156==\226\128\157 operator.\n"
    | 96 ->
        "Invalid modulo operation. Expected an expression after the \226\128\156%\226\128\157 operator.\n"
    | 94 ->
        "Invalid division operation. Expected an expression after the \226\128\156/\226\128\157 operator.\n"
    | 92 ->
        "Invalid multiplication operation. Expected an expression after the \226\128\156*\226\128\157 operator.\n"
    | 90 ->
        "Invalid logical OR operation. Expected an expression after the \226\128\156||\226\128\157 operator.\n"
    | 88 ->
        "Invalid pipe operation. Expected an expression after the \226\128\156|>\226\128\157 operator.\n"
    | 85 ->
        "Invalid exponentiation operation. Expected an expression after the \226\128\156**\226\128\157 operator.\n"
    | 83 ->
        "Invalid lambda function. Expected the function body expression after the arrow \226\128\156->\226\128\157.\n"
    | 82 ->
        "Invalid lambda function. Expected an arrow \226\128\156->\226\128\157 after the parameters.\n"
    | 81 ->
        "Invalid lambda function. Expected at least one parameter name after the backslash \226\128\156\\\226\128\157.\n"
    | 79 ->
        "Invalid function application. Expected an argument after the function expression.\n"
    | 76 ->
        "Invalid unary not operation. Expected a boolean expression after the \226\128\156!\226\128\157.\n"
    | 72 ->
        "Invalid if expression. Expected a condition expression after \226\128\156if\226\128\157.\n"
    | 71 ->
        "Invalid let binding. Expected an expression after the equals sign \226\128\156=\226\128\157.\n"
    | 70 ->
        "Invalid let binding. Expected an equals sign \226\128\156=\226\128\157 after the type signature.\n"
    | 69 ->
        "Invalid let binding. Expected a type signature \226\128\156:\226\128\157 or an equals sign \226\128\156=\226\128\157.\n"
    | 68 ->
        "Invalid let expression. Expected at least one binding (e.g., \226\128\156x = 1\226\128\157) after \226\128\156let\226\128\157.\n"
    | 67 ->
        "Invalid match expression. Expected an expression to match against after \226\128\156match\226\128\157.\n"
    | 66 ->
        "Invalid list literal. Expected an expression or a closing square bracket \226\128\156]\226\128\157.\n"
    | 64 ->
        "Invalid unary minus operation. Expected an expression after the \226\128\156-\226\128\157 sign.\n"
    | 63 ->
        "Invalid unary plus operation. Expected an expression after the \226\128\156+\226\128\157 sign.\n"
    | 62 ->
        "Invalid expression. Expected an expression inside the parentheses \226\128\156()\226\128\157.\n"
    | 60 ->
        "Invalid constructor usage. Expected an argument for the constructor or the end of the expression.\n"
    | 58 ->
        "Invalid definition. Expected an expression after the equals sign \226\128\156=\226\128\157.\n"
    | 57 ->
        "Invalid value definition. Expected an equals sign \226\128\156=\226\128\157 after the type signature.\n"
    | 55 ->
        "Invalid function definition. Expected another parameter, a type signature \226\128\156:\226\128\157, or an equals sign \226\128\156=\226\128\157.\n"
    | 54 ->
        "Invalid type signature. Expected an equals sign \226\128\156=\226\128\157 or a function arrow \226\128\156->\226\128\157.\n"
    | 53 ->
        "Invalid type signature. Expected a type after the colon \226\128\156:\226\128\157.\n"
    | 49 ->
        "Invalid function parameter list. Expected another parameter after the comma \226\128\156,\226\128\157.\n"
    | 48 ->
        "Invalid function parameter list. Expected a comma \226\128\156,\226\128\157 followed by another parameter, or a closing parenthesis \226\128\156)\226\128\157.\n"
    | 46 ->
        "Invalid function parameter list. Expected another parameter after the comma \226\128\156,\226\128\157.\n"
    | 45 ->
        "Invalid function parameter list. Expected a comma \226\128\156,\226\128\157 followed by another parameter, or a closing parenthesis \226\128\156)\226\128\157.\n"
    | 43 ->
        "Invalid function parameter list. Expected a parameter name or nested parameters inside \226\128\156()\226\128\157.\n"
    | 42 ->
        "Invalid definition. Expected a type signature \226\128\156:\226\128\157, an equals sign \226\128\156=\226\128\157, or function parameters.\n"
    | 38 ->
        "Invalid ADT definition. Expected another variant constructor or a closing brace \226\128\156}\226\128\157.\n"
    | 36 ->
        "Invalid ADT variant type. Expected a semicolon \226\128\156;\226\128\157 or a function arrow \226\128\156->\226\128\157.\n"
    | 35 ->
        "Invalid ADT variant. Expected a type after \226\128\156as\226\128\157.\n"
    | 34 ->
        "Invalid ADT variant. Expected \226\128\156as\226\128\157 followed by a type, or a semicolon \226\128\156;\226\128\157.\n"
    | 33 ->
        "Invalid ADT definition. Expected at least one variant constructor inside the braces \226\128\156{}\226\128\157.\n"
    | 31 ->
        "Invalid type alias. Expected the end of the definition or a function arrow \226\128\156->\226\128\157.\n"
    | 29 ->
        "Invalid ADT definition. Expected another type parameter or the start of ADT variants \226\128\156{\226\128\157.\n"
    | 28 ->
        "Invalid type definition. Expected another type parameter, the start of ADT variants \226\128\156{\226\128\157, or the end of the type alias.\n"
    | 21 ->
        "Invalid tuple type. Expected another type after the comma \226\128\156,\226\128\157.\n"
    | 20 ->
        "Invalid tuple type. Expected a comma \226\128\156,\226\128\157 followed by another type, a closing parenthesis \226\128\156)\226\128\157, or a function arrow \226\128\156->\226\128\157.\n"
    | 19 ->
        "Invalid tuple type. Expected another type after the comma \226\128\156,\226\128\157.\n"
    | 18 ->
        "Invalid tuple type. Expected a comma \226\128\156,\226\128\157 followed by another type, or a closing parenthesis \226\128\156)\226\128\157.\n"
    | 16 ->
        "Invalid function type. Expected a return type after the arrow \226\128\156->\226\128\157.\n"
    | 14 ->
        "Invalid list type. Expected a closing square bracket \226\128\156]\226\128\157 or a function arrow \226\128\156->\226\128\157.\n"
    | 7 ->
        "Invalid list type. Expected a type inside the square brackets \226\128\156[]\226\128\157.\n"
    | 5 ->
        "Invalid tuple type. Expected a type inside the parentheses \226\128\156()\226\128\157.\n"
    | 4 ->
        "Invalid type constructor. Expected a type argument or the end of the type signature.\n"
    | 3 ->
        "Invalid type or ADT definition. Expected a type alias, a list of type parameters, or the start of ADT variants \226\128\156{\226\128\157.\n"
    | 2 ->
        "Invalid type or ADT definition. Expected \226\128\156:=\226\128\157 after the type name.\n"
    | 1 ->
        "Invalid definition. Expected an identifier after \226\128\156def\226\128\157.\n"
    | 0 ->
        "Unexpected token. Expected a definition or a comment.\n"
    | _ ->
        raise Not_found
