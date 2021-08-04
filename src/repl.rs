use crate::eval;
use crate::types::*;

pub fn repl_eval(
    read_res: Result<RispExp, RispErr>,
    env: &mut RispEnv,
) -> Result<RispExp, RispErr> {
    let exp = read_res?;

    Ok(eval::eval(&exp, env)?)
}

pub fn repl_print(eval_res: Result<RispExp, RispErr>) {
    match eval_res {
        Ok(res) => println!(";;=> {}", res),
        Err(e) => match e {
            RispErr::Reason(msg) => println!(";;[ERROR]=> {}", msg),
        },
    }
}
