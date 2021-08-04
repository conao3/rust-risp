use crate::types::*;
use std::collections::HashMap;
use std::rc::Rc;

pub fn eval_if_args(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<RispExp, RispErr> {
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

pub fn eval_set_args(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<RispExp, RispErr> {
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

pub fn eval_lambda_args(arg_forms: &[RispExp], _env: &mut RispEnv) -> Result<RispExp, RispErr> {
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

pub fn eval_forms(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<Vec<RispExp>, RispErr> {
    arg_forms.iter().map(|x| eval(x, env)).collect()
}

pub fn env_get(k: &str, env: &RispEnv) -> Option<RispExp> {
    match env.data.get(k) {
        Some(exp) => Some(exp.clone()),
        None => match &env.outer {
            Some(outer_env) => env_get(k, outer_env),
            None => None,
        },
    }
}

pub fn parse_list_of_symbol_strings(form: Rc<RispExp>) -> Result<Vec<String>, RispErr> {
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

pub fn env_for_lambda<'a>(
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

pub fn eval(exp: &RispExp, env: &mut RispEnv) -> Result<RispExp, RispErr> {
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
