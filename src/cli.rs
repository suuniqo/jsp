use clap::Parser;


#[derive(Parser)]
#[command(
    version,
    about = "Modular and efficient language processor for MyJS",
    long_about = None
)]
pub struct Cli {
    /// Source file to compile
    #[arg(value_name = "SOURCE", required = true)]
    pub source: String,

    /// Dump lexer trace (stdout by default)
    #[arg(long, short = 'l', value_name = "FILE")]
    pub lexer_trace: Option<Option<String>>,

    /// Dump symbol table trace (stdout by default)
    #[arg(long, short = 's', value_name = "FILE")]
    pub symtb_trace: Option<Option<String>>,

    /// Dump parse trace (stdout by default)
    #[arg(long, short = 'p', value_name = "FILE")]
    pub parse_trace: Option<Option<String>>,
}

impl Cli {
    pub fn parse_args() -> Self {
        Cli::parse()
    }
}
