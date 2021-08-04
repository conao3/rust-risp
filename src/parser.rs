use crate::types::*;
use std::num::ParseFloatError;

pub fn tokenize(expr: String) -> Vec<String> {
    expr.replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(|x| x.to_string())
        .collect()
}

pub fn parse1(tokens: &[String]) -> Result<(RispExp, &[String]), RispErr> {
    let (token, rest) = tokens
        .split_first()
        .ok_or_else(|| RispErr::Reason("unexpected EOF".to_string()))?;

    match &**token {
        "(" => {
            let mut res: Vec<RispExp> = vec![];
            let mut lrest = rest;
            while lrest
                .first()
                .ok_or_else(|| RispErr::Reason("missing `)".to_string()))?
                != ")"
            {
                let (exp, new_lrest) = parse1(lrest)?;
                res.push(exp);
                lrest = new_lrest;
            }
            Ok((RispExp::list(res), &lrest[1..]))
        }
        ")" => Err(RispErr::Reason("unexpected `)`".to_string())),
        _ => Ok((
            match &**token {
                "true" => RispExp::Bool(true),
                "false" => RispExp::Bool(false),
                _ => {
                    let potential_float: Result<f64, ParseFloatError> = token.parse();
                    match potential_float {
                        Ok(v) => RispExp::Number(v),
                        Err(_) => RispExp::Symbol(token.to_string()),
                    }
                }
            },
            rest,
        )),
    }
}

pub fn parse(tokens: Vec<String>) -> Result<RispExp, RispErr> {
    let (exp, rest) = parse1(&tokens)?;
    if rest.len() > 0 {
        return Err(RispErr::Reason(format!(
            "Unconsumed token remains: {}",
            rest.join(", ")
        )));
    }
    Ok(exp)
}

pub fn read(expr: String) -> Result<RispExp, RispErr> {
    parse(tokenize(expr))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fmt;

    impl PartialEq for RispExp {
        fn eq(&self, other: &Self) -> bool {
            match (self, other) {
                (RispExp::Bool(a), RispExp::Bool(b)) => a == b,
                (RispExp::Symbol(a), RispExp::Symbol(b)) => a == b,
                (RispExp::Number(a), RispExp::Number(b)) => a == b,
                (RispExp::List(a), RispExp::List(b)) => a == b,
                _ => false,
            }
        }
    }

    impl fmt::Debug for RispExp {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                RispExp::Bool(b) => f.debug_tuple("Bool").field(b).finish(),
                RispExp::Symbol(s) => f.debug_tuple("Symbol").field(s).finish(),
                RispExp::Number(n) => f.debug_tuple("Number").field(n).finish(),
                RispExp::List(xs) => f.debug_tuple("List").field(xs).finish(),
                RispExp::Func(_) => f.debug_tuple("Func").finish(),
                RispExp::Lambda(_) => f.debug_tuple("Lambda").finish(),
            }
        }
    }

    #[test]
    pub fn test_tokenize() {
        assert_eq!(tokenize("(+ 10 5)".to_string()), ["(", "+", "10", "5", ")"]);
        assert_eq!(tokenize("10".to_string()), ["10"]);
        assert_eq!(tokenize("()".to_string()), ["(", ")"]);
    }

    #[test]
    pub fn test_parse() {
        let exp = parse(tokenize("10".to_string())).unwrap();
        assert_eq!(exp, RispExp::integer(10));

        let exp = parse(tokenize("+".to_string())).unwrap();
        assert_eq!(exp, RispExp::symbol("+"));

        let exp = parse(tokenize("(+ 10 5)".to_string())).unwrap();
        assert_eq!(
            exp,
            RispExp::list([
                RispExp::symbol("+"),
                RispExp::integer(10),
                RispExp::integer(5),
            ])
        );

        let exp = parse(tokenize("(if (< 1 2) (+ 10 5) 1)".to_string())).unwrap();
        assert_eq!(
            exp,
            RispExp::list([
                RispExp::symbol("if"),
                RispExp::list([
                    RispExp::symbol("<"),
                    RispExp::integer(1),
                    RispExp::integer(2),
                ]),
                RispExp::list([
                    RispExp::symbol("+"),
                    RispExp::integer(10),
                    RispExp::integer(5),
                ]),
                RispExp::integer(1)
            ])
        );

        assert_eq!(
            parse(tokenize("(+ 10 5) 3".to_string())),
            Err(RispErr::Reason("Unconsumed token remains: 3".to_string()))
        );
    }
}
