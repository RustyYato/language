use dec::base::error::CaptureInput;

fn main() {
    let file = std::env::args_os().nth(1).expect("Provide a file name");
    let file = std::fs::read_to_string(file).expect("Could not open file");
    let file = file.as_str();

    let tokens = compiler::tokenizer::tokenize::<CaptureInput<_>>(file);
    // println!("{:#?}", tokens);
    let (_, tokens) = tokens.unwrap();
    let (parse, errors) = compiler::parser::parse(&tokens);
    println!("{:?}", parse.syntax());
}
