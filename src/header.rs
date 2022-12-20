use crate::*;
use nom::{branch::alt, bytes::complete::*, character::complete::*, sequence::tuple, Parser};

pub fn header(input: &str) -> Result<(u64, u64)> {
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

pub fn title(input: &str) -> Result<&str> {
    let (input, _) = space0(input)?;
    take_until("\n").parse(input)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Format {
    ASCII,
    BINARY,
}

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
    use nom::Finish;

    #[test]
    fn header() {
        assert_eq!(
            super::header("# vtk DataFile Version 2.0")
                .finish()
                .unwrap(),
            ("", (2, 0))
        );
    }
}
