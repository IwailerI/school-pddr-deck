use std::{fmt::Debug, fs, path::PathBuf, str::FromStr};

use clap::Parser;

use serde::{ser::SerializeTuple, Serialize, Serializer};

#[derive(Parser)]
#[command(name = "QardCompiler")]
#[command(version = "1.0")]
#[command(
    about = "Qard compiler.",
    long_about = "Quard compiler. (Pronounced Q-ard) Question and Answer caRDs"
)]
struct Cli {
    /// Path to the source file
    #[arg(short, long)]
    input: PathBuf,

    /// Path to the output file. Defaults to "./a.json"
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Path to the resource directory. All images will be saved there. Defaults to "./resources/"
    #[arg(short, long)]
    resource_dir: Option<PathBuf>,

    /// Whether output json file will be minified or not.
    #[arg(short, long, default_value_t = true)]
    compact: bool,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Card {
    question: String,
    answer: String,
}

#[derive(Serialize)]
pub struct RichText(Vec<RichTextComponent>);

pub enum RichTextComponent {
    Text(String),

    /// Contains hash of rendered latex.
    Latex(u64),
}

pub struct RichCard {
    question: RichText,
    answer: RichText,
}

fn main() -> Result<(), String> {
    let cli = Cli::parse();

    let input = cli.input;
    if !input.exists() {
        return Err("Input file does not exist!".to_owned());
    }

    let output = match cli.output {
        Some(v) => v,
        None => PathBuf::from_str("./a.json").unwrap(),
    };

    let resource_dir = match cli.resource_dir {
        Some(v) => v,
        None => PathBuf::from_str("./resources/").unwrap(),
    };

    let source = fs::read_to_string(input).map_err(|_| "Unable to open input file.")?;

    let cards = parse_qard::parse(&source).map_err(|e| format!("{e:?}"))?;

    let rich_cards =
        latex::enrich_text(cards, &resource_dir, true).map_err(|e| format!("{e:?}"))?;

    compiler::compile(rich_cards, &output, !cli.compact).map_err(|e| format!("{e:?}"))?;

    println!("All done!");

    Ok(())
}

impl Serialize for RichTextComponent {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut tuple = serializer.serialize_tuple(2)?;
        match self {
            Self::Text(text) => {
                tuple.serialize_element("text")?;
                tuple.serialize_element(text)?;
            }
            Self::Latex(hash) => {
                tuple.serialize_element("latex")?;
                tuple.serialize_element(&latex::hash_to_hex(*hash))?;
            }
        }
        tuple.end()
    }
}

impl Serialize for RichCard {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut tuple = serializer.serialize_tuple(2)?;
        tuple.serialize_element(&self.question)?;
        tuple.serialize_element(&self.answer)?;
        tuple.end()
    }
}

mod compiler {
    use std::{fs, io, path::Path};

    use crate::RichCard;

    #[derive(Debug)]
    pub enum Error {
        Io(io::Error),
        Serde(serde_json::Error),
    }

    impl From<io::Error> for Error {
        fn from(value: io::Error) -> Self {
            Self::Io(value)
        }
    }

    impl From<serde_json::Error> for Error {
        fn from(value: serde_json::Error) -> Self {
            Self::Serde(value)
        }
    }

    pub fn compile<I>(data: I, output: &Path, pretty: bool) -> Result<(), Error>
    where
        I: IntoIterator<Item = RichCard>,
    {
        let file_out = fs::File::create(output)?;
        let data = data.into_iter().collect::<Vec<_>>();

        if pretty {
            serde_json::to_writer_pretty(file_out, &data)?;
        } else {
            serde_json::to_writer(file_out, &data)?;
        }

        Ok(())
    }
}

mod parse_qard {
    use std::fmt::Debug;

    use super::Card;

    #[derive(Clone, Copy, PartialEq, Eq, Debug)]
    enum Delim {
        Question,
        Answer,
    }

    #[derive(Clone, Copy, PartialEq, Eq)]
    pub enum ParseError {
        SyntaxError(usize),
    }

    impl Debug for ParseError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::SyntaxError(line) => write!(f, "Syntax error near line {line}"),
            }
        }
    }

    /// Parses data into cards.
    /// Parsing rules:
    /// - Each card consists of question and answer
    /// - Question should begin with Q: (or Q#:)
    /// - Answer should begin with A: (or A#:)
    /// - Number of # in Q#: or A#: signifies a rank. (Q: and A: are rank 0)
    /// - Each question of rank n should be followed by answer of rank n.
    /// - Blank lines are ignored.
    /// - There can be either one Q or one A or one Q and one A on one line.
    /// - Anything before Q: on a line is ignored.
    /// - If line doesn't contain a Q, everything before A: is ignored.
    /// - Anything before first Q: of the file is ignored.
    /// - Any line inside Q or A of rank n starting with > followed by n # is ignored.
    /// - Line breaks between lines are preserved, but otherwise lines are trimmed.
    pub fn parse(data: &str) -> Result<Vec<Card>, ParseError> {
        use Delim as D;
        use QardLineResult as R;

        let mut res = vec![];
        let mut delim = None;
        let mut question: Option<(String, usize)> = None;
        let mut answer: Option<(String, usize)> = None;

        let mut flush = |line_number: usize,
                         question: Option<(String, usize)>,
                         answer: Option<(String, usize)>|
         -> Result<(), ParseError> {
            match (question, answer) {
                (Some(q), Some(a)) if q.1 == a.1 => res.push(Card {
                    question: q.0.trim().to_owned(),
                    answer: a.0.trim().to_owned(),
                }),
                (None, None) => (),
                _ => return Err(ParseError::SyntaxError(line_number)),
            };
            Ok(())
        };

        let mut last = 0;
        for (i, line) in data.lines().enumerate() {
            last = i;
            match parse_qard_line(line, delim) {
                R::Question(rank, text) => {
                    flush(i, question, answer)?;
                    question = Some((text.trim().to_owned(), rank));
                    answer = None;
                    delim = Some((D::Question, rank));
                }
                R::QuestionAndAnswer(rank, q_text, a_text) => {
                    flush(i, question, answer)?;
                    question = Some((q_text.trim().to_owned(), rank));
                    answer = Some((a_text.trim().to_owned(), rank));
                    delim = Some((D::Answer, rank))
                }
                R::Answer(rank, text) => {
                    if answer.is_some() {
                        return Err(ParseError::SyntaxError(i));
                    }
                    answer = Some((text.trim().to_owned(), rank));
                    delim = Some((D::Answer, rank))
                }
                R::Text(text) => match (&mut question, &mut answer) {
                    (Some(q), None) => {
                        q.0 += "\n";
                        q.0 += text.trim();
                    }
                    (Some(_), Some(a)) => {
                        a.0 += "\n";
                        a.0 += text.trim();
                    }
                    _ => return Err(ParseError::SyntaxError(i)),
                },
                R::Empty => (), // noop
            }
        }

        flush(last, question, answer)?;

        Ok(res)
    }

    #[derive(Clone, Copy, PartialEq, Eq, Debug)]
    enum QardLineResult<'a> {
        Empty,
        Text(&'a str),
        Question(usize, &'a str),
        Answer(usize, &'a str),
        QuestionAndAnswer(usize, &'a str, &'a str),
    }

    /// Parses one qard line.
    fn parse_qard_line(line: &str, delim: Option<(Delim, usize)>) -> QardLineResult<'_> {
        use Delim as D;
        use QardLineResult as R;

        match delim {
            Some((delim, rank)) => {
                // Check for comment
                if is_comment(line, rank) {
                    return R::Empty;
                }

                if delim == D::Question {
                    // Check for line with corresponding answer
                    match trim_until_delim(line, Some(rank)) {
                        Some((substr, D::Answer, _)) => return R::Answer(rank, substr),
                        _ => R::Text(line),
                    }
                } else {
                    // Check for line with next question
                    match trim_until_delim(line, None) {
                        Some((substr_q, D::Question, rank)) => {
                            // Check for Q and A on one line
                            let r = trim_until_delim(substr_q, Some(rank));
                            match r {
                                Some((substr_a, D::Answer, rank2)) if rank2 == rank => {
                                    R::QuestionAndAnswer(
                                        rank,
                                        strip_up_to(substr_q.strip_suffix(substr_a).unwrap(), 'A'),
                                        substr_a,
                                    )
                                }
                                _ => R::Question(rank, substr_q),
                            }
                        }
                        _ => R::Text(line),
                    }
                }
            }
            None => {
                if is_comment(line, 0) {
                    return R::Empty;
                }

                match trim_until_delim(line, None) {
                    Some((substr_q, D::Question, rank)) => {
                        match trim_until_delim(substr_q, Some(rank)) {
                            Some((substr_a, D::Answer, rank2)) if rank2 == rank => {
                                R::QuestionAndAnswer(
                                    rank,
                                    strip_up_to(substr_q.strip_suffix(substr_a).unwrap(), 'A'),
                                    substr_a,
                                )
                            }
                            _ => R::Question(rank, substr_q),
                        }
                    }
                    _ => R::Empty,
                }
            }
        }
    }

    /// Strips all character from the end of string up to and including c.
    /// If c is not found, returns ""
    fn strip_up_to(s: &str, c: char) -> &str {
        let mut found = false;
        s.trim_end_matches(|x| {
            if x == c {
                found = true;
                true
            } else {
                !found
            }
        })
    }

    /// Trims data, stripping everything up to and including first delimiter (Q: or A:)
    /// Returns trimmed string, type of delimiter and order of delimiter.
    /// If rank is None, all ranks of delimiters are respected.
    /// If rank is Some, only delimiters of given rank are respected.
    fn trim_until_delim(data: &str, wanted_rank: Option<usize>) -> Option<(&str, Delim, usize)> {
        let mut rank = 0_usize;
        let mut delim_type = None;
        for (i, c) in data.char_indices() {
            match delim_type {
                None => {
                    delim_type = Some(match c {
                        'Q' => Delim::Question,
                        'A' => Delim::Answer,
                        _ => continue,
                    })
                }
                Some(delim) => match c {
                    '#' => rank += 1,
                    ':' => match wanted_rank {
                        None => return Some((&data[i + c.len_utf8()..], delim, rank)),
                        Some(wanted_rank) => {
                            if rank == wanted_rank {
                                return Some((&data[i + c.len_utf8()..], delim, rank));
                            }
                            delim_type = None;
                            rank = 0;
                        }
                    },
                    _ => return None,
                },
            }
        }

        None
    }

    /// Check if given line is a comment.
    fn is_comment(line: &str, rank: usize) -> bool {
        line.trim_start()
            .starts_with(&(">".to_owned() + &"#".repeat(rank)))
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_trim_until_delim() {
            assert_eq!(
                trim_until_delim("dsodjfksjd Q: abacaba", None),
                Some((" abacaba", Delim::Question, 0)),
            );

            assert_eq!(
                trim_until_delim("A: abacaba", None),
                Some((" abacaba", Delim::Answer, 0)),
            );

            assert_eq!(
                trim_until_delim("dsodjfksjd Q##: abacaba", None),
                Some((" abacaba", Delim::Question, 2)),
            );

            assert_eq!(trim_until_delim("dsodjfksjd Q#2#: abacaba", None), None);

            assert_eq!(trim_until_delim(";fdjgjkl3243 r'gfl kxfgljksk", None), None);

            assert_eq!(
                trim_until_delim("dsodjfksjd Q###: abacaba", Some(3)),
                Some((" abacaba", Delim::Question, 3)),
            );

            assert_eq!(
                trim_until_delim("A###: abacaba", Some(3)),
                Some((" abacaba", Delim::Answer, 3)),
            );

            assert_eq!(trim_until_delim("dsodjfksjd Q##: abacaba", Some(0)), None,);

            assert_eq!(trim_until_delim("dsodjfksjd Q#2#: abacaQ:a", Some(1)), None);

            assert_eq!(
                trim_until_delim(";fdjgjkl3Q#:24A###:3 r'gfl kxfgljksk", Some(0)),
                None
            );
        }

        #[test]
        fn test_parse_qard_line() {
            use Delim as D;
            use QardLineResult as R;

            assert_eq!(parse_qard_line("Aba ca ba Q Q : DQ#%$%:", None), R::Empty);
            assert_eq!(
                parse_qard_line("Aba ca ba Q Q : DQ#%$%:", Some((D::Question, 3))),
                R::Text("Aba ca ba Q Q : DQ#%$%:")
            );
            assert_eq!(
                parse_qard_line("Aba ca ba Q Q : DQ#%$%:", Some((D::Answer, 1))),
                R::Text("Aba ca ba Q Q : DQ#%$%:")
            );
            assert_eq!(parse_qard_line("Aba ca ba Q Q : DQ#%$%:", None), R::Empty);

            assert_eq!(
                parse_qard_line("Q: abcdef", None),
                R::Question(0, " abcdef")
            );
            assert_eq!(
                parse_qard_line("asd asdQ: abcdef", None),
                R::Question(0, " abcdef")
            );
            assert_eq!(
                parse_qard_line(" sdfgdfs dfgQ: abcdef", None),
                R::Question(0, " abcdef")
            );
            assert_eq!(
                parse_qard_line("1.2. Q: abcdef Q:", None),
                R::Question(0, " abcdef Q:")
            );
            assert_eq!(
                parse_qard_line("Q##: abcdef A:", None),
                R::Question(2, " abcdef A:")
            );
            assert_eq!(
                parse_qard_line("Q: abcdef", Some((D::Answer, 4))),
                R::Question(0, " abcdef")
            );
            assert_eq!(
                parse_qard_line("Q: abcdef Q:", Some((D::Answer, 4))),
                R::Question(0, " abcdef Q:")
            );
            assert_eq!(
                parse_qard_line("Q##: abcdef A:", Some((D::Answer, 4))),
                R::Question(2, " abcdef A:")
            );

            assert_eq!(
                parse_qard_line("A: abcdef", Some((D::Question, 0))),
                R::Answer(0, " abcdef")
            );
            assert_eq!(
                parse_qard_line("A: abcdef Q:", Some((D::Question, 4))),
                R::Text("A: abcdef Q:")
            );
            assert_eq!(
                parse_qard_line("A##: abcdef A:", Some((D::Question, 4))),
                R::Text("A##: abcdef A:")
            );
            assert_eq!(
                parse_qard_line("A##: abcdef A:", Some((D::Question, 2))),
                R::Answer(2, " abcdef A:")
            );

            assert_eq!(
                parse_qard_line("Q: abc? A: abc.", Some((D::Answer, 0))),
                R::QuestionAndAnswer(0, " abc? ", " abc.")
            );
            assert_eq!(
                parse_qard_line("Q: abc? A: abc.", None),
                R::QuestionAndAnswer(0, " abc? ", " abc.")
            );
            assert_eq!(
                parse_qard_line("Q#: abA:c? A#: aQ:bc.", Some((D::Answer, 1))),
                R::QuestionAndAnswer(1, " abA:c? ", " aQ:bc.")
            );
        }

        #[test]
        fn test_parse() {
            const DATA: &str = r"
        Q: Переведите $36\frac{km}{h}$ в $\frac{m}{s}$. A: $10 \frac{m}{s}$ (: 3.6)
        Q: Переведем $2 ha$ в $m^2$ A: $20000m^2$
        Q: Переведите из $200 cm^3$ в $ml$ A: $200ml$
        Q: Переведите $75kPa$ в $Pa$ A: $75000Pa$ 

        Q: Переведите 1. $98\micro m$ в $m$ 
        > This is a comment
        > This is a comment
        A: $0.000098m$ 

        > This is a comment
        Q: Переведите 1Q##:. $98\micro m$ в $m$ 
        A: $0.000098m$A##:: 
        > This is a comment

        > This is a comment
        Q###: Переведите 1Q Q: $98\micro m$ в $m$ 
        >### This is a comment
        > This is not a comment!
        A###: $0.000098m$A::: 
        > This is a comment

        Q: Переведите 1Q Q: $98\micro m$ в $m$ 
        > This is a comment
        A: $0.000098m$A::: 
        ";
            assert_eq!(
                parse(DATA),
                Ok(vec![
                    Card {
                        question: "Переведите $36\\frac{km}{h}$ в $\\frac{m}{s}$.".into(),
                        answer: "$10 \\frac{m}{s}$ (: 3.6)".into(),
                    },
                    Card {
                        question: "Переведем $2 ha$ в $m^2$".into(),
                        answer: "$20000m^2$".into(),
                    },
                    Card {
                        question: "Переведите из $200 cm^3$ в $ml$".into(),
                        answer: "$200ml$".into(),
                    },
                    Card {
                        question: "Переведите $75kPa$ в $Pa$".into(),
                        answer: "$75000Pa$".into(),
                    },
                    Card {
                        question: "Переведите 1. $98\\micro m$ в $m$".into(),
                        answer: "$0.000098m$".into(),
                    },
                    Card {
                        question: "Переведите 1Q##:. $98\\micro m$ в $m$".into(),
                        answer: "$0.000098m$A##::".into(),
                    },
                    Card {
                        question: "Переведите 1Q Q: $98\\micro m$ в $m$\n> This is not a comment!"
                            .into(),
                        answer: "$0.000098m$A:::\n> This is a comment".into(),
                    },
                    Card {
                        question: "Переведите 1Q Q: $98\\micro m$ в $m$".into(),
                        answer: "$0.000098m$A:::".into(),
                    },
                ],)
            )
        }

        #[test]
        fn test_invalid_parse() {
            const DATA: &str = r"
            Q: DSKJF";

            assert_eq!(parse(DATA), Err(ParseError::SyntaxError(1)))
        }
    }
}

mod latex {
    use indicatif::ProgressBar;
    use regex::Regex;
    use std::{
        collections::HashSet,
        ffi::OsStr,
        fs::{self},
        io, iter,
        path::{Path, PathBuf},
        process::Command,
        str::FromStr,
        sync::mpsc::{channel, Receiver, Sender},
        thread::{self},
    };

    use crate::{Card, RichCard, RichText};

    const BLOCK_MATH_PATTERN: &str = r"\$\$.+\$\$";
    const INLINE_MATH_PATTERN: &str = r"\$(?:[^$\\]|\\.)+\$";

    #[derive(Debug)]
    pub enum Error {
        IO(io::Error),
        // LatexRenderFailed(String),
    }

    impl From<io::Error> for Error {
        fn from(value: io::Error) -> Self {
            Self::IO(value)
        }
    }

    /// Searches for all latex expressions, renders them (if needed) and transforms text into
    /// rich text/
    pub fn enrich_text<I>(
        cards: I,
        resources: &Path,
        show_progress: bool,
    ) -> Result<Vec<RichCard>, Error>
    where
        I: IntoIterator<Item = Card>,
    {
        let cards: Vec<_> = cards.into_iter().collect();

        // get all needed expressions
        let latex_expressions = extract_latex(cards.iter().flat_map(|c| {
            iter::once(c.question.to_owned()).chain(iter::once(c.answer.to_owned()))
        }));

        // get already existing expressions
        let mut already_rendered: HashSet<u64> = HashSet::new();
        fs::create_dir_all(resources)?;
        for entry in fs::read_dir(resources)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_file() && path.extension() == Some(OsStr::new("png")) {
                let path = match path.file_stem() {
                    Some(v) => v,
                    None => continue,
                };
                let path = match path.to_str() {
                    Some(v) => v,
                    None => continue,
                };
                match hex_to_hash(path) {
                    Some(v) => already_rendered.insert(v),
                    None => continue,
                };
            }
        }

        // render latex
        render_latex(
            latex_expressions
                .iter()
                .filter(|s| !already_rendered.contains(&hash_string(s)))
                .collect::<Vec<_>>()
                .into_iter(),
            resources,
            show_progress,
        )?;

        let regex_block = Regex::new(BLOCK_MATH_PATTERN).unwrap();
        let regex_inline = Regex::new(INLINE_MATH_PATTERN).unwrap();

        {
            use crate::RichTextComponent as RTC;
            Ok(cards
                .into_iter()
                .map(|card| {
                    let mut rc = RichCard {
                        question: RichText(vec![]),
                        answer: RichText(vec![]),
                    };

                    let convert = |source: &str, target: &mut RichText| {
                        let mut v: Vec<_> = regex_block
                            .find_iter(source)
                            .chain(regex_inline.find_iter(source))
                            .collect();
                        v.sort_unstable_by(|l, r| l.range().start.cmp(&r.range().start));
                        let mut i = 0;
                        for m in v.into_iter() {
                            let bounds = m.range();
                            if bounds.start > i {
                                target
                                    .0
                                    .push(RTC::Text(source[i..bounds.start].to_string()))
                            }
                            target.0.push(RTC::Latex(hash_string(m.as_str())));
                            i = bounds.end;
                        }
                        if i < source.len() {
                            target.0.push(RTC::Text(source[i..].to_string()));
                        }
                    };

                    convert(&card.question, &mut rc.question);
                    convert(&card.answer, &mut rc.answer);

                    rc
                })
                .collect())
        }
    }

    /// Searches for all latex expressions and returns them.
    pub fn extract_latex<I, S>(strings: I) -> HashSet<String>
    where
        S: Into<String>,
        I: IntoIterator<Item = S> + Clone,
    {
        let regex_block = Regex::new(BLOCK_MATH_PATTERN).unwrap();
        let regex_inline = Regex::new(INLINE_MATH_PATTERN).unwrap();

        strings
            .clone()
            .into_iter()
            .map(|s| {
                regex_block
                    .find_iter(&s.into())
                    .map(|m| m.as_str().to_owned())
                    .collect::<Vec<_>>()
                    .into_iter()
            })
            .chain(strings.into_iter().map(|s| {
                regex_inline
                    .find_iter(&s.into())
                    .map(|m| m.as_str().to_owned())
                    .collect::<Vec<_>>()
                    .into_iter()
            }))
            .flatten()
            .collect()
    }

    /// Renders each latex equation from data to a .png file.
    pub fn render_latex<I, II, S>(
        data: I,
        resources: &Path,
        show_progress: bool,
    ) -> Result<(), Error>
    where
        S: Into<String>,
        I: IntoIterator<Item = S, IntoIter = II>,
        II: ExactSizeIterator<Item = S>,
    {
        const THREAD_COUNT: usize = 32;

        let display = |total: usize, chan: Receiver<bool>| {
            // draw bar
            let bar = ProgressBar::new(total as u64);

            while chan.recv().is_ok() {
                bar.inc(1);
            }
        };

        let work = |data: Vec<String>,
                    resources: PathBuf,
                    chan: Sender<bool>|
         -> Result<Vec<String>, Error> {
            let mut bad = vec![];
            for expression in data {
                let mut path = resources
                    .join(PathBuf::from_str(&hash_to_hex(hash_string(&expression))).unwrap());
                path.set_extension("png");
                let output = Command::new("klatexformula")
                    .args(["-l", &expression])
                    .args(["-o", path.to_str().unwrap()])
                    .args(["-f", "#ffffff"])
                    .args(["-X", "250"])
                    .args(["-m", "..."])
                    .args(["-p", r"\usepackage{amsmath}\usepackage{amssymb}\usepackage{amsfonts}\usepackage{gensymb}"])
                    .output()?;

                if !output.status.success() {
                    bad.push(expression);
                }

                chan.send(true).unwrap();
            }

            Ok(bad)
        };

        let mut split_data: Vec<Vec<String>> = vec![];
        split_data.resize_with(THREAD_COUNT, std::vec::Vec::new);
        let mut total = 0;
        for (i, e) in data
            .into_iter()
            .map(|x| <S as Into<String>>::into(x))
            .enumerate()
        {
            total += 1;
            split_data[i % THREAD_COUNT].push(e);
        }

        let (sender, receiver) = channel();
        let mut join_handles = vec![];
        for _i in 0..THREAD_COUNT {
            let d = split_data.pop().unwrap();
            let r = resources.to_owned();
            let rx = sender.clone();
            join_handles.push(thread::spawn(move || work(d, r, rx)))
        }

        if show_progress {
            thread::spawn(move || display(total, receiver));
        }

        for handle in join_handles {
            handle.join().unwrap().unwrap();
        }

        Ok(())
    }

    pub fn hash_string(s: &str) -> u64 {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        use std::hash::Hasher;

        hasher.write(s.as_bytes());

        hasher.finish()
    }

    pub fn hash_to_hex(hash: u64) -> String {
        format!("{hash:016x}")
    }

    pub fn hex_to_hash(s: &str) -> Option<u64> {
        u64::from_str_radix(s, 16).ok()
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn aaa() {
            for s in &["a", "Hello world!", "sdfjsd;lf", "Текст на русском"] {
                let hash = hash_string(s);
                println!("{hash:016x} - {s}");
            }
        }
    }
}
