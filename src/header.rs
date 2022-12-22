use crate::*;
use nom::{branch::alt, bytes::complete::*, character::complete::*, sequence::tuple, Parser};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Header {
    pub version: (u64, u64),
    pub title: String,
    pub format: Format,
}

pub fn header(input: &str) -> Result<Header> {
    tuple((head_line, multispace1, title, line_ending, format))
        .map(|(version, _, title, _, format)| Header {
            version,
            title: title.to_string(),
            format,
        })
        .parse(input)
}

/// File header `# vtk DataFile Version x.x`
pub fn head_line(input: &str) -> Result<(u64, u64)> {
    let (input, _) = tuple((
        char('#'),
        space0,
        tag("vtk"),
        space1,
        tag("DataFile"),
        space1,
        tag("Version"),
        space1,
    ))
    .parse(input)?;

    tuple((uint, char('.'), uint))
        .map(|(major, _dot, minor)| (major, minor))
        .parse(input)
}

/// Second line of the file
pub fn title(input: &str) -> Result<&str> {
    let (input, _) = space0(input)?;
    take_until("\n").parse(input)
}

/// `ASCII` or `BINARY`
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Format {
    ASCII,
    BINARY,
}

/// Third line of the file, `ASCII` or `BINARY`
pub fn format(input: &str) -> Result<Format> {
    let (input, _) = space0(input)?;
    alt((
        tag("ASCII").map(|_| Format::ASCII),
        tag("BINARY").map(|_| Format::BINARY),
    ))
    .parse(input)
}

#[cfg(test)]
mod test {
    use super::{Format, Header};
    use nom::Finish;

    #[test]
    fn head_line() {
        assert_eq!(
            super::head_line("# vtk DataFile Version 2.0")
                .finish()
                .unwrap(),
            ("", (2, 0))
        );
    }

    #[test]
    fn header() {
        let (residual, out) = super::header(
            r#"
            # vtk DataFile Version 2.0
            Cube example
            ASCII
            "#
            .trim(),
        )
        .finish()
        .unwrap();

        assert_eq!(residual, "");
        assert_eq!(
            out,
            Header {
                version: (2, 0),
                title: "Cube example".to_string(),
                format: Format::ASCII,
            }
        );
    }
}
