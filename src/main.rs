use risp::env::default_env;
use risp::parser::read;
use risp::repl::repl_eval;
use risp::repl::repl_print;
use rustyline::Editor;

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
