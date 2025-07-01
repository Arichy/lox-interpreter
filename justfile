run:
  cargo run -- run test.lox

lexer:
  cargo run -- tokenize test.lox

parse:
  cargo run -- parse test.lox

evaluate:
  cargo run -- evaluate test.lox