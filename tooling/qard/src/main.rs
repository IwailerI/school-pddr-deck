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

    /// Force rerender
    #[arg(short, long, default_value_t = false)]
    force_rerender: bool,

    /// Verbose debug output
    #[arg(short, long, default_value_t = false)]
    verbose: bool,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Card {
    question: String,
    answer: String,
}

#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct RichText(Vec<RichTextComponent>);

#[derive(Debug, PartialEq, Eq)]
pub enum RichTextComponent {
    Text(String),

    /// Contains hash of rendered latex.
    Latex(u64),
}

#[derive(Debug, PartialEq, Eq)]
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

    let cards = parse_qard::parse(&source, cli.verbose).map_err(|e| format!("{e:?}"))?;

    let rich_cards = latex::enrich_text(cards, &resource_dir, true, cli.force_rerender, cli.verbose)
        .map_err(|e| format!("{e:?}"))?;

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
    pub fn parse(data: &str, verbose: bool) -> Result<Vec<Card>, ParseError> {
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
                        Some((substr, D::Answer, _)) => R::Answer(rank, substr),
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
                    });
                    rank = 0;
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
                    _ => delim_type = None,
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
        A: $$0.000098m$$ 

        > This is a comment
        12. Q: Переведите 1Q##:. $98\micro m$ в $m$ 
        A: $0.000098m$A##:: 
        > This is a comment

        > This is a comment
        54645. Q###: Переведите 1Q Q: $98\micro m$ в $m$ 
        >### This is a comment
        > This is not a comment!
        A###: $0.000098m$A::: 
        > This is a comment

        Q: Переведите 1Q Q: $98\micro m$ в $m$ 
        > This is a comment
        A: $0.000098m$A::: 
        ";
            assert_eq!(
                parse(DATA, false),
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
                        answer: "$$0.000098m$$".into(),
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

            assert_eq!(parse(DATA, false), Err(ParseError::SyntaxError(1)))
        }

        #[test]
        fn test_realistic_parse() {
            const DATA: &str = r"
            211. Q: $\triangle ABC$ с углами $\alpha, \beta, \gamma$ и сторонами $a, b, c$. $\gamma = 90\degree$. $\sin \alpha =?$ A: $\sin \alpha=\frac{a}{c}$
            212. Q: $\triangle ABC$ с углами $\alpha, \beta, \gamma$ и сторонами $a, b, c$. $\gamma = 90\degree$. $\sin \beta =?$ A: $\sin \beta=\frac{b}{c}$
            213. Q: $\triangle ABC$ с углами $\alpha, \beta, \gamma$ и сторонами $a, b, c$. $\gamma = 90\degree$. $\tan \alpha =?$ A: $\tan \alpha=\frac{a}{b}$
            214. Q: $\triangle ABC$ с углами $\alpha, \beta, \gamma$ и сторонами $a, b, c$. $\gamma = 90\degree$. $\tan \beta =?$ A: $\tan \beta=\frac{b}{a}$
            215. Q: $\triangle ABC$ с углами $\alpha, \beta, \gamma$ и сторонами $a, b, c$. $\gamma = 90\degree$. $\cos \alpha =?$ A: $\cos \alpha=\frac{b}{c}$
            216. Q: $\triangle ABC$ с углами $\alpha, \beta, \gamma$ и сторонами $a, b, c$. $\gamma = 90\degree$. $\cos \beta =?$ A: $\cos \beta=\frac{a}{c}$
            217. Q: $\triangle ABC$ с углами $\alpha, \beta, \gamma$ и сторонами $a, b, c$. $\gamma = 90\degree$. $\text{ctg} \alpha =?$ A: $\text{ctg} \alpha=\frac{b}{a}$
            218. Q: $\triangle ABC$ с углами $\alpha, \beta, \gamma$ и сторонами $a, b, c$. $\gamma = 90\degree$. $\text{ctg} \beta =?$ A: $\text{ctg} \beta=\frac{a}{b}$
            219. Q: Является ли данный график выражения $y=D_{20}x + D_{12}$ функцией или зависимостью? A: Функция.";

            fn c<S1, S2>(q: S1, a: S2) -> Card
            where
                S1: Into<String>,
                S2: Into<String>,
            {
                Card {
                    answer: a.into(),
                    question: q.into(),
                }
            }

            assert_eq!(
                parse(DATA, false),
                Ok(vec![
                    c(
                        r"$\triangle ABC$ с углами $\alpha, \beta, \gamma$ и сторонами $a, b, c$. $\gamma = 90\degree$. $\sin \alpha =?$",
                        r"$\sin \alpha=\frac{a}{c}$"
                    ),
                    c(
                        r"$\triangle ABC$ с углами $\alpha, \beta, \gamma$ и сторонами $a, b, c$. $\gamma = 90\degree$. $\sin \beta =?$",
                        r"$\sin \beta=\frac{b}{c}$"
                    ),
                    c(
                        r"$\triangle ABC$ с углами $\alpha, \beta, \gamma$ и сторонами $a, b, c$. $\gamma = 90\degree$. $\tan \alpha =?$",
                        r"$\tan \alpha=\frac{a}{b}$"
                    ),
                    c(
                        r"$\triangle ABC$ с углами $\alpha, \beta, \gamma$ и сторонами $a, b, c$. $\gamma = 90\degree$. $\tan \beta =?$",
                        r"$\tan \beta=\frac{b}{a}$"
                    ),
                    c(
                        r"$\triangle ABC$ с углами $\alpha, \beta, \gamma$ и сторонами $a, b, c$. $\gamma = 90\degree$. $\cos \alpha =?$",
                        r"$\cos \alpha=\frac{b}{c}$"
                    ),
                    c(
                        r"$\triangle ABC$ с углами $\alpha, \beta, \gamma$ и сторонами $a, b, c$. $\gamma = 90\degree$. $\cos \beta =?$",
                        r"$\cos \beta=\frac{a}{c}$"
                    ),
                    c(
                        r"$\triangle ABC$ с углами $\alpha, \beta, \gamma$ и сторонами $a, b, c$. $\gamma = 90\degree$. $\text{ctg} \alpha =?$",
                        r"$\text{ctg} \alpha=\frac{b}{a}$"
                    ),
                    c(
                        r"$\triangle ABC$ с углами $\alpha, \beta, \gamma$ и сторонами $a, b, c$. $\gamma = 90\degree$. $\text{ctg} \beta =?$",
                        r"$\text{ctg} \beta=\frac{a}{b}$"
                    ),
                    c(
                        r"Является ли данный график выражения $y=D_{20}x + D_{12}$ функцией или зависимостью?",
                        r"Функция."
                    )
                ])
            )
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

    const BLOCK_MATH_PATTERN: &str = r"\$\$.+?\$\$";
    const INLINE_MATH_PATTERN: &str = r"(?:[^$]|^)(\$(?:[^$\\]|\\.)+\$)";

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
    /// rich text
    pub fn enrich_text<I>(
        cards: I,
        resources: &Path,
        show_progress: bool,
        force_rerender: bool,
        verbose: bool,
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

        if !force_rerender {
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

        Ok(convert_to_rich(cards))
    }

    fn convert_to_rich(cards: Vec<Card>) -> Vec<RichCard> {
        use crate::RichTextComponent as RTC;

        let regex_block = Regex::new(BLOCK_MATH_PATTERN).unwrap();
        let regex_inline = Regex::new(INLINE_MATH_PATTERN).unwrap();

        cards
            .into_iter()
            .map(|card| {
                let mut rc = RichCard {
                    question: RichText(vec![]),
                    answer: RichText(vec![]),
                };

                let convert = |source: &str, target: &mut RichText| {
                    let mut v: Vec<_> = regex_block
                        .find_iter(source)
                        .map(|x| (x.range(), x.as_str().to_owned()))
                        .chain(regex_inline.captures_iter(source).map(|x| {
                            let c = x.get(1).unwrap();
                            (c.range(), c.as_str().to_owned())
                        }))
                        .collect();
                    v.sort_unstable_by(|l, r| l.0.start.cmp(&r.0.start));
                    let mut i = 0;
                    for (range, text) in v.into_iter() {
                        let bounds = range;
                        if bounds.start > i {
                            target
                                .0
                                .push(RTC::Text(source[i..bounds.start].to_string()))
                        }
                        target
                            .0
                            .push(RTC::Latex(hash_string(text.trim_matches(|c| c != '$'))));
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
            .collect()
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
                    .captures_iter(&s.into())
                    .map(|m| m.extract::<1>().1[0].to_string())
                    .collect::<Vec<String>>()
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

        let mut bad = vec![];
        for handle in join_handles {
            let t = handle.join().unwrap().unwrap();
            bad.extend(t.into_iter());
        }

        if !bad.is_empty() {
            println!("Failed to compile following {} expressions:", bad.len());
            for expr in bad {
                println!("({}) {expr}", hash_to_hex(hash_string(&expr)));
            }
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
        fn test_extract_latex() {
            assert_eq!(
                extract_latex([
                    r"Назовите хотя бы 2 способа разложения на множители.",
                    r"Вынесение общего множителя за скобки, группировка, формула сокращённого умножения, разложения квадратным трёхчленом.",
                    r"Разложите на множители: $75\alpha+50\beta-25\gamma$",
                    r"$25\left(3\alpha+2\beta-\gamma\right)$",
                    r"Разложите на множители: $a(2+b) - 4(2+b)$",
                    r"$(a-4)(2+b)$",
                    r"Разложите на множители: $6x+7y+42+xy$",
                    r"$(6x+xy) + (42+7y) = x(6 + y) + 7(6+y) = (x + 7)(y + 6)$",
                    r"Разложите на множители: $(x+2)^2 - y^2$",
                    r"$$\left(x-y+2\right)\left(x+y+2\right)$$",
                    r"Сумма углов $D_{6}$-угольника?",
                    r"$180\degree \cdot \left(D_{6}-2\right)$",
                    r"Сумма углов = $540 \degree$. Сколько углов у этого многоугольников?",
                    r"$5$",
                ]),
                HashSet::from([
                    r"$75\alpha+50\beta-25\gamma$".to_owned(),
                    r"$25\left(3\alpha+2\beta-\gamma\right)$".to_owned(),
                    r"$(a-4)(2+b)$".to_owned(),
                    r"$a(2+b) - 4(2+b)$".to_owned(),
                    r"$6x+7y+42+xy$".to_owned(),
                    r"$(6x+xy) + (42+7y) = x(6 + y) + 7(6+y) = (x + 7)(y + 6)$".to_owned(),
                    r"$(x+2)^2 - y^2$".to_owned(),
                    r"$$\left(x-y+2\right)\left(x+y+2\right)$$".to_owned(),
                    r"$D_{6}$".to_owned(),
                    r"$180\degree \cdot \left(D_{6}-2\right)$".to_owned(),
                    r"$540 \degree$".to_owned(),
                    r"$5$".to_owned(),
                ]),
            );
        }

        #[test]
        fn test_another_bug() {
            const DATA: &'static str = r"
            211. Q: $\triangle ABC$ с углами $\alpha, \beta, \gamma$ и сторонами $a, b, c$. $\gamma = 90\degree$. $\sin \alpha =?$ A: $\sin \alpha=\frac{a}{c}$
            212. Q: $\triangle ABC$ с углами $\alpha, \beta, \gamma$ и сторонами $a, b, c$. $\gamma = 90\degree$. $\sin \beta =?$ A: $\sin \beta=\frac{b}{c}$
            213. Q: $\triangle ABC$ с углами $\alpha, \beta, \gamma$ и сторонами $a, b, c$. $\gamma = 90\degree$. $\tan \alpha =?$ A: $\tan \alpha=\frac{a}{b}$
            214. Q: $\triangle ABC$ с углами $\alpha, \beta, \gamma$ и сторонами $a, b, c$. $\gamma = 90\degree$. $\tan \beta =?$ A: $\tan \beta=\frac{b}{a}$
            215. Q: $\triangle ABC$ с углами $\alpha, \beta, \gamma$ и сторонами $a, b, c$. $\gamma = 90\degree$. $\cos \alpha =?$ A: $\cos \alpha=\frac{b}{c}$
            216. Q: $\triangle ABC$ с углами $\alpha, \beta, \gamma$ и сторонами $a, b, c$. $\gamma = 90\degree$. $\cos \beta =?$ A: $\cos \beta=\frac{a}{c}$
            217. Q: $\triangle ABC$ с углами $\alpha, \beta, \gamma$ и сторонами $a, b, c$. $\gamma = 90\degree$. $\text{ctg} \alpha =?$ A: $\text{ctg} \alpha=\frac{b}{a}$
            218. Q: $\triangle ABC$ с углами $\alpha, \beta, \gamma$ и сторонами $a, b, c$. $\gamma = 90\degree$. $\text{ctg} \beta =?$ A: $\text{ctg} \beta=\frac{a}{b}$
            219. Q: Является ли данный график выражения $y=D_{20}x + D_{12}$ функцией или зависимостью? A: Функция.";

            assert_eq!(
                extract_latex(DATA.lines()),
                HashSet::from([
                    r"$\triangle ABC$".to_owned(),
                    r"$\alpha, \beta, \gamma$".to_owned(),
                    r"$\alpha, \beta, \gamma$".to_owned(),
                    r"$a, b, c$".to_owned(),
                    r"$\gamma = 90\degree$".to_owned(),
                    r"$\sin \alpha =?$".to_owned(),
                    r"$\sin \beta =?$".to_owned(),
                    r"$\tan \alpha =?$".to_owned(),
                    r"$\tan \beta =?$".to_owned(),
                    r"$\cos \alpha =?$".to_owned(),
                    r"$\cos \beta =?$".to_owned(),
                    r"$\text{ctg} \alpha =?$".to_owned(),
                    r"$\text{ctg} \beta =?$".to_owned(),
                    r"$\sin \alpha=\frac{a}{c}$".to_owned(),
                    r"$\sin \beta=\frac{b}{c}$".to_owned(),
                    r"$\tan \alpha=\frac{a}{b}$".to_owned(),
                    r"$\tan \beta=\frac{b}{a}$".to_owned(),
                    r"$\cos \alpha=\frac{b}{c}$".to_owned(),
                    r"$\cos \beta=\frac{a}{c}$".to_owned(),
                    r"$\text{ctg} \alpha=\frac{b}{a}$".to_owned(),
                    r"$\text{ctg} \beta=\frac{a}{b}$".to_owned(),
                    r"$y=D_{20}x + D_{12}$".to_owned(),
                ])
            );
        }

        #[test]
        fn test_convert_to_rich() {
            fn c<S1, S2>(q: S1, a: S2) -> Card
            where
                S1: Into<String>,
                S2: Into<String>,
            {
                Card {
                    answer: a.into(),
                    question: q.into(),
                }
            }

            let data = vec![
                c(
                    r"$\triangle ABC$ с углами $\alpha, \beta, \gamma$ и сторонами $a, b, c$. $\gamma = 90\degree$. $\sin \alpha =?$",
                    r"$\sin \alpha=\frac{a}{c}$",
                ),
                c(r"How do you do $fellow$ kids?", r"$\sin \beta=\frac{b}{c}$"),
                c(r"jkdhslfslkjdfhshj", r"$$as$$  $s$ $sda$ asd"),
            ];

            use crate::RichTextComponent as RTC;

            assert_eq!(
                convert_to_rich(data),
                vec![
                    RichCard {
                        question: RichText(vec![
                            RTC::Latex(12401561065036509712),
                            RTC::Text(" с углами ".into()),
                            RTC::Latex(15995697945151550248),
                            RTC::Text(" и сторонами ".into()),
                            RTC::Latex(5206233681529036138),
                            RTC::Text(". ".into()),
                            RTC::Latex(16131095731852739553),
                            RTC::Text(". ".into()),
                            RTC::Latex(16130633799840544398),
                        ]),
                        answer: RichText(vec![RTC::Latex(6420836051647171949)]),
                    },
                    RichCard {
                        question: RichText(vec![
                            RTC::Text("How do you do ".into()),
                            RTC::Latex(1831719063848115660),
                            RTC::Text(" kids?".into()),
                        ]),
                        answer: RichText(vec![RTC::Latex(12486462396849526576),]),
                    },
                    RichCard {
                        question: RichText(vec![RTC::Text("jkdhslfslkjdfhshj".into()),]),
                        answer: RichText(vec![
                            RTC::Latex(13673530227706961097),
                            RTC::Text("  ".into()),
                            RTC::Latex(9841187739635155080),
                            RTC::Text(" ".into()),
                            RTC::Latex(16591311224064121595),
                            RTC::Text(" asd".into()),
                        ]),
                    },
                ]
            );
        }
    }
}
