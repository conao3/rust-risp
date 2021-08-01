use std::collections::HashMap;
use std::fmt;
use std::io;
use std::num::ParseFloatError;
use std::rc::Rc;

/*
  Types
*/

#[derive(Clone)]
enum RispExp {
    Bool(bool),
    Symbol(String),
    Number(f64),
    List(Vec<RispExp>),
    Func(fn(&[RispExp]) -> Result<RispExp, RispErr>),
    Lambda(RispLambda),
}

#[allow(dead_code)]
impl RispExp {
    fn symbol(symbol: impl Into<String>) -> Self {
        Self::Symbol(symbol.into())
    }

    fn integer(num: impl Into<f64>) -> Self {
        Self::Number(num.into())
    }

    fn list(list: impl Into<Vec<Self>>) -> Self {
        Self::List(list.into())
    }
}

#[derive(Clone)]
struct RispLambda {
    params_exp: Rc<RispExp>,
    body_exp: Rc<RispExp>,
}

impl fmt::Display for RispExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            RispExp::Bool(a) => a.to_string(),
            RispExp::Symbol(s) => s.clone(),
            RispExp::Number(n) => n.to_string(),
            RispExp::List(list) => {
                let xs: Vec<String> = list.iter().map(|x| x.to_string()).collect();
                format!("({})", xs.join(", "))
            }
            RispExp::Func(_) => "Function {}".to_string(),
            RispExp::Lambda(_) => "Lambda {}".to_string(),
        };

        write!(f, "{}", str)
    }
}

#[derive(Debug, PartialEq)]
enum RispErr {
    Reason(String),
}

#[derive(Clone)]
struct RispEnv<'a> {
    data: HashMap<String, RispExp>,
    outer: Option<&'a RispEnv<'a>>,
}

/*
  Parse
*/

fn tokenize(expr: String) -> Vec<String> {
    expr.replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(|x| x.to_string())
        .collect()
}

fn parse1(tokens: &[String]) -> Result<(RispExp, &[String]), RispErr> {
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

fn parse(tokens: Vec<String>) -> Result<RispExp, RispErr> {
    let (exp, rest) = parse1(&tokens)?;
    if rest.len() > 0 {
        return Err(RispErr::Reason(format!(
            "Unconsumed token remains: {}",
            rest.join(", ")
        )));
    }
    Ok(exp)
}

/*
  Env
*/

fn default_env<'a>() -> RispEnv<'a> {
    macro_rules! basic_op {
        ($fn:expr) => {
            |args: &[RispExp]| -> Result<RispExp, RispErr> {
                let floats = parse_list_of_floats(args)?;
                let (first, rest) = floats
                    .split_first()
                    .ok_or_else(|| RispErr::Reason("expected at least one number".to_string()))?;
                Ok(RispExp::Number(rest.iter().fold(*first, $fn)))
            }
        };
    }
    macro_rules! pred {
        ($fn:expr) => {
            |args: &[RispExp]| -> Result<RispExp, RispErr> {
                let floats = parse_list_of_floats(args)?;
                Ok(RispExp::Bool(floats.windows(2).all(|w| $fn(w[0], w[1]))))
            }
        };
    }

    let mut data: HashMap<String, RispExp> = HashMap::new();

    data.insert("+".to_string(), RispExp::Func(basic_op!(|acc, a| acc + a)));
    data.insert("-".to_string(), RispExp::Func(basic_op!(|acc, a| acc - a)));
    data.insert(
        "=".to_string(),
        RispExp::Func(pred!(|a: f64, b: f64| (a - b).abs() < f64::EPSILON)),
    );
    data.insert(">".to_string(), RispExp::Func(pred!(|a, b| a > b)));
    data.insert(">=".to_string(), RispExp::Func(pred!(|a, b| a >= b)));
    data.insert("<".to_string(), RispExp::Func(pred!(|a, b| a < b)));
    data.insert("<=".to_string(), RispExp::Func(pred!(|a, b| a <= b)));

    RispEnv { data, outer: None }
}

fn parse_list_of_floats(args: &[RispExp]) -> Result<Vec<f64>, RispErr> {
    args.iter()
        .map(|x| match x {
            RispExp::Number(x) => Ok(*x),
            _ => Err(RispErr::Reason("expected a number".to_string())),
        })
        .collect()
}

/*
  Eval
*/

fn eval_if_args(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<RispExp, RispErr> {
    let test_form = arg_forms
        .first()
        .ok_or_else(|| RispErr::Reason("expected test form".to_string()))?;

    let test_eval = eval(test_form, env)?;
    match test_eval {
        RispExp::Bool(b) => {
            let form_idx = if b { 1 } else { 2 };
            let res_form = arg_forms
                .get(form_idx)
                .ok_or_else(|| RispErr::Reason(format!("expected form idx={}", form_idx)))?;

            eval(res_form, env)
        }
        _ => Err(RispErr::Reason(format!(
            "unexpected test form='{}'",
            test_form.to_string()
        ))),
    }
}

fn eval_def_args(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<RispExp, RispErr> {
    let first_form = arg_forms
        .first()
        .ok_or_else(|| RispErr::Reason("expected first form".to_string()))?;
    let first_str = match first_form {
        RispExp::Symbol(s) => Ok(s.clone()),
        _ => Err(RispErr::Reason(
            "expected first form to be a symbol".to_string(),
        )),
    }?;
    let second_form = arg_forms
        .get(1)
        .ok_or_else(|| RispErr::Reason("expected second form".to_string()))?;

    if arg_forms.len() > 2 {
        return Err(RispErr::Reason("def can only have two forms".to_string()));
    }
    let second_eval = eval(second_form, env)?;
    env.data.insert(first_str, second_eval);

    Ok(first_form.clone())
}

fn eval_lambda_args(arg_forms: &[RispExp]) -> Result<RispExp, RispErr> {
    let params_exp = arg_forms
        .first()
        .ok_or_else(|| RispErr::Reason("expected args form".to_string()))?;
    let body_exp = arg_forms
        .get(1)
        .ok_or_else(|| RispErr::Reason("expected second form".to_string()))?;
    if arg_forms.len() > 2 {
        return Err(RispErr::Reason(
            "fn definition can only have two forms".to_string(),
        ));
    }

    Ok(RispExp::Lambda(RispLambda {
        body_exp: Rc::new(body_exp.clone()),
        params_exp: Rc::new(params_exp.clone()),
    }))
}

fn eval_built_in_form(
    exp: &RispExp,
    arg_forms: &[RispExp],
    env: &mut RispEnv,
) -> Option<Result<RispExp, RispErr>> {
    match exp {
        RispExp::Symbol(s) => match s.as_ref() {
            "if" => Some(eval_if_args(arg_forms, env)),
            "def" => Some(eval_def_args(arg_forms, env)),
            "fn" => Some(eval_lambda_args(arg_forms)),
            _ => None,
        },
        _ => None,
    }
}

fn env_get(k: &str, env: &RispEnv) -> Option<RispExp> {
    match env.data.get(k) {
        Some(exp) => Some(exp.clone()),
        None => match &env.outer {
            Some(outer_env) => env_get(k, outer_env),
            None => None,
        },
    }
}

fn parse_list_of_symbol_strings(form: Rc<RispExp>) -> Result<Vec<String>, RispErr> {
    let list = match form.as_ref() {
        RispExp::List(s) => Ok(s.clone()),
        _ => Err(RispErr::Reason(
            "expected args form to be a list".to_string(),
        )),
    }?;
    list.iter()
        .map(|x| match x {
            RispExp::Symbol(s) => Ok(s.clone()),
            _ => Err(RispErr::Reason(
                "expected symbols in the argument list".to_string(),
            )),
        })
        .collect()
}

fn env_for_lambda<'a>(
    params: Rc<RispExp>,
    arg_forms: &[RispExp],
    outer_env: &'a mut RispEnv,
) -> Result<RispEnv<'a>, RispErr> {
    let ks = parse_list_of_symbol_strings(params)?;
    if ks.len() != arg_forms.len() {
        return Err(RispErr::Reason(format!(
            "expected {} arguments, got {}",
            ks.len(),
            arg_forms.len()
        )));
    }
    let vs = eval_forms(arg_forms, outer_env)?;
    let mut data: HashMap<String, RispExp> = HashMap::new();
    for (k, v) in ks.iter().zip(vs.iter()) {
        data.insert(k.clone(), v.clone());
    }
    Ok(RispEnv {
        data,
        outer: Some(outer_env),
    })
}

fn eval_forms(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<Vec<RispExp>, RispErr> {
    arg_forms.iter().map(|x| eval(x, env)).collect()
}

fn eval(exp: &RispExp, env: &mut RispEnv) -> Result<RispExp, RispErr> {
    match exp {
        RispExp::Symbol(k) => {
            env_get(k, env).ok_or_else(|| RispErr::Reason(format!("unexpected symbol '{}'", k)))
        }
        RispExp::Bool(_a) => Ok(exp.clone()),
        RispExp::Number(_a) => Ok(exp.clone()),

        RispExp::List(list) => {
            let first_form = list
                .first()
                .ok_or_else(|| RispErr::Reason("expected a non-empty list".to_string()))?;
            let arg_forms = &list[1..];
            match eval_built_in_form(first_form, arg_forms, env) {
                Some(res) => res,
                None => {
                    let first_eval = eval(first_form, env)?;
                    match first_eval {
                        RispExp::Func(f) => f(&eval_forms(arg_forms, env)?),
                        RispExp::Lambda(lambda) => {
                            let new_env = &mut env_for_lambda(lambda.params_exp, arg_forms, env)?;
                            eval(&lambda.body_exp, new_env)
                        }
                        _ => Err(RispErr::Reason("first form must be a function".to_string())),
                    }
                }
            }
        }
        RispExp::Func(_) => Err(RispErr::Reason("unexpected form".to_string())),
        RispExp::Lambda(_) => Err(RispErr::Reason("unexpected form".to_string())),
    }
}

/*
  Repl
*/

fn parse_eval(expr: String, env: &mut RispEnv) -> Result<RispExp, RispErr> {
    let parsed_exp = parse(tokenize(expr))?;
    let evaled_exp = eval(&parsed_exp, env)?;

    Ok(evaled_exp)
}

fn slurp_expr() -> String {
    let mut expr = String::new();

    io::stdin()
        .read_line(&mut expr)
        .expect("Failed to read line");

    expr
}

fn main() {
    let env = &mut default_env();
    loop {
        println!("risp>");
        let expr = slurp_expr();
        match parse_eval(expr, env) {
            Ok(res) => println!(";;=> {}", res),
            Err(e) => match e {
                RispErr::Reason(msg) => println!(";;[ERROR]=> {}", msg),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn test_tokenize() {
        assert_eq!(tokenize("(+ 10 5)".to_string()), ["(", "+", "10", "5", ")"]);
        assert_eq!(tokenize("10".to_string()), ["10"]);
        assert_eq!(tokenize("()".to_string()), ["(", ")"]);
    }

    #[test]
    fn test_parse() {
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
