use risp::env::default_env;
use risp::parser::read;
use risp::repl::repl_eval;
use risp::repl::repl_print;
use rustyline::DefaultEditor;

fn main() {
    let mut rl = DefaultEditor::new().unwrap();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history")
    }
    let env = &mut default_env();

    loop {
        if let Ok(line) = rl.readline("risp> ") {
            let _ = rl.add_history_entry(line.as_str());
            repl_print(repl_eval(read(line), env));
        } else {
            break;
        }
    }
    rl.save_history("history.txt").unwrap();
}
