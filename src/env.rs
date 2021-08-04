use crate::types::*;
use std::collections::HashMap;

pub fn default_env<'a>() -> RispEnv<'a> {
    macro_rules! basic_op {
        ($fn: expr) => {
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
        ($fn: expr) => {
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

pub fn parse_list_of_floats(args: &[RispExp]) -> Result<Vec<f64>, RispErr> {
    args.iter()
        .map(|x| match x {
            RispExp::Number(x) => Ok(*x),
            _ => Err(RispErr::Reason("expected a number".to_string())),
        })
        .collect()
}
