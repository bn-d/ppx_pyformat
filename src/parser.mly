%token <string> STR
%token <Types.raw_replacement_field> FIELD
%token EOF

%start <Types.elements> prog
%%

prog:
  | EOF { [] }
  | f = FIELD; tl = prog { (Types.Field (Type_utils.sanitize_field f)) :: tl }
  | str = string_; f = FIELD; tl = prog { (Types.Text str) :: (Types.Field (Type_utils.sanitize_field f)) :: tl }
  | str = string_; EOF { [Types.Text str] }

string_: l = string_list { String.concat "" l }

string_list:
  | str = STR { [str] }
  | str = STR; tl = string_list { str :: tl }
