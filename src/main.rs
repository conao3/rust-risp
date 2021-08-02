use rustyline::Editor;
use std::collections::HashMap;
use std::fmt;
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

impl From<&'_ RispExp> for bool {
    fn from(exp: &RispExp) -> Self {
        match exp {
            RispExp::Bool(p) => *p,
            RispExp::List(l) => !l.is_empty(),
            _ => true,
        }
    }
}

impl From<RispExp> for bool {
    fn from(exp: RispExp) -> Self {
        (&exp).into()
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

fn read(expr: String) -> Result<RispExp, RispErr> {
    parse(tokenize(expr))
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
    data.insert("*".to_string(), RispExp::Func(basic_op!(|acc, a| acc * a)));
    data.insert("/".to_string(), RispExp::Func(basic_op!(|acc, a| acc / a)));
    data.insert(">".to_string(), RispExp::Func(pred!(|a, b| a > b)));
    data.insert("<".to_string(), RispExp::Func(pred!(|a, b| a < b)));
    data.insert(">=".to_string(), RispExp::Func(pred!(|a, b| a >= b)));
    data.insert("<=".to_string(), RispExp::Func(pred!(|a, b| a <= b)));
    data.insert(
        "=".to_string(),
        RispExp::Func(pred!(|a: f64, b: f64| (a - b).abs() < f64::EPSILON)),
    );

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
    match arg_forms {
        [cond_form, then_form, else_form] => {
            let exp = if eval(cond_form, env)?.into() {
                then_form
            } else {
                else_form
            };
            eval(exp, env)
        }
        _ => Err(RispErr::Reason(
            "wrong number of arguments: expect 3".to_string(),
        )),
    }
}

fn eval_set_args(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<RispExp, RispErr> {
    match arg_forms {
        [RispExp::Symbol(sym), val_form] => {
            let val = eval(val_form, env)?;
            env.data.insert(sym.clone(), val.clone());
            Ok(val)
        }
        [_, _] => Err(RispErr::Reason(
            "wrong type argument: expect symbol".to_string(),
        )),
        _ => Err(RispErr::Reason(
            "wrong number of arguments: expect 2".to_string(),
        )),
    }
}

fn eval_lambda_args(arg_forms: &[RispExp], _env: &mut RispEnv) -> Result<RispExp, RispErr> {
    match arg_forms {
        [RispExp::List(param_form), body_form] => Ok(RispExp::Lambda(RispLambda {
            params_exp: Rc::new(RispExp::List(param_form.to_vec()).clone()),
            body_exp: Rc::new(body_form.clone()),
        })),
        [_, _] => Err(RispErr::Reason(
            "wrong type argument: expect list".to_string(),
        )),
        _ => Err(RispErr::Reason(
            "wrong number of arguments: expect 2".to_string(),
        )),
    }
}

fn eval_forms(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<Vec<RispExp>, RispErr> {
    arg_forms.iter().map(|x| eval(x, env)).collect()
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

fn eval(exp: &RispExp, env: &mut RispEnv) -> Result<RispExp, RispErr> {
    match exp {
        RispExp::Symbol(k) => {
            env_get(k, env).ok_or_else(|| RispErr::Reason(format!("unexpected symbol '{}'", k)))
        }
        RispExp::Bool(_a) => Ok(exp.clone()),
        RispExp::Number(_a) => Ok(exp.clone()),

        RispExp::List(list_form) => {
            let (first_form, rest) = list_form
                .split_first()
                .ok_or_else(|| RispErr::Reason("expected a non-empty list".to_string()))?;

            match first_form {
                RispExp::Symbol(s) if s == "if" => eval_if_args(rest, env),
                RispExp::Symbol(s) if s == "set" => eval_set_args(rest, env),
                RispExp::Symbol(s) if s == "lambda" => eval_lambda_args(rest, env),
                _ => {
                    let first_eval = eval(first_form, env)?;
                    match first_eval {
                        RispExp::Func(f) => f(&eval_forms(rest, env)?),
                        RispExp::Lambda(lambda) => {
                            let new_env = &mut env_for_lambda(lambda.params_exp, rest, env)?;
                            eval(&lambda.body_exp, new_env)
                        }
                        _ => Err(RispErr::Reason("invalid function".to_string())),
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

fn repl_eval(read_res: Result<RispExp, RispErr>, env: &mut RispEnv) -> Result<RispExp, RispErr> {
    let exp = read_res?;

    Ok(eval(&exp, env)?)
}

fn repl_print(eval_res: Result<RispExp, RispErr>) {
    match eval_res {
        Ok(res) => println!(";;=> {}", res),
        Err(e) => match e {
            RispErr::Reason(msg) => println!(";;[ERROR]=> {}", msg),
        },
    }
}

fn main() {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history")
    }
    let env = &mut default_env();

    loop {
        if let Ok(line) = rl.readline("risp> ") {
            rl.add_history_entry(line.as_str());
            repl_print(repl_eval(read(line), env));
        } else {
            break;
        }
    }
    rl.save_history("history.txt").unwrap();
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
