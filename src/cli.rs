use clap::Parser;
use clap::builder::styling::{Ansi256Color, Styles, Style};

use crate::style::Color;


fn get_style() -> Styles {
    Styles::styled()
        .header(Style::default())
        .usage(Style::default())
        .placeholder(Style::default().bold())
        .literal(Ansi256Color(Color::White.code()).on_default().bold())
        .error(Ansi256Color(Color::Red.code()).on_default().bold())
        .valid(Ansi256Color(Color::White.code()).on_default().bold())
        .invalid(Ansi256Color(Color::Red.code()).on_default().bold())
}

#[derive(Parser)]
#[command(
    version,
    about = "Modular and efficient language processor for MyJS",
    long_about = None,
    styles=get_style(),
)]
pub struct Config {
    /// Source files to process
    #[arg(value_name = "SOURCES", required = true)]
    pub sources: Vec<String>,

    /// Dump lexer trace (stdout by default)
    #[arg(long, short = 'l', value_name = "FILE")]
    pub lexer_trace: Option<Option<String>>,

    /// Dump symbol table trace (stdout by default)
    #[arg(long, short = 's', value_name = "FILE")]
    pub symtb_trace: Option<Option<String>>,

    /// Dump parse trace (stdout by default)
    #[arg(long, short = 'p', value_name = "FILE")]
    pub parse_trace: Option<Option<String>>,

    /// Silence any errors or warnings
    #[arg(long, short = 'q')]
    pub quiet: bool,
}

impl Config {
    pub fn build() -> Self {
        Config::parse()
    }
}
