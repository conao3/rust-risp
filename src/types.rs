use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Clone)]
pub enum RispExp {
    Bool(bool),
    Symbol(String),
    Number(f64),
    List(Vec<RispExp>),
    Func(fn(&[RispExp]) -> Result<RispExp, RispErr>),
    Lambda(RispLambda),
}

#[allow(dead_code)]
impl RispExp {
    pub fn symbol(symbol: impl Into<String>) -> Self {
        Self::Symbol(symbol.into())
    }

    pub fn integer(num: impl Into<f64>) -> Self {
        Self::Number(num.into())
    }

    pub fn list(list: impl Into<Vec<Self>>) -> Self {
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
pub struct RispLambda {
    pub params_exp: Rc<RispExp>,
    pub body_exp: Rc<RispExp>,
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
pub enum RispErr {
    Reason(String),
}

#[derive(Clone)]
pub struct RispEnv<'a> {
    pub data: HashMap<String, RispExp>,
    pub outer: Option<&'a RispEnv<'a>>,
}
