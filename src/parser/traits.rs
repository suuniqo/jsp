pub trait Parser {
    fn parse(&mut self) -> Option<Vec<usize>>;
}
