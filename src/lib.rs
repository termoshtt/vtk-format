//! Parser of [VTK legacy format](https://vtk.org/wp-content/uploads/2015/04/file-formats.pdf)

pub mod dataset;
pub mod header;
pub mod primitive;

use header::*;
use primitive::*;

use nom::{character::complete::*, error::VerboseError, sequence::tuple, Parser};

pub type Result<'input, T> = nom::IResult<&'input str, T, VerboseError<&'input str>>;

pub fn line_end(input: &str) -> Result<()> {
    tuple((space0, line_ending)).map(|_| ()).parse(input)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Vtk {
    pub version: (u64, u64),
    pub title: String,
    pub format: Format,
}

pub fn vtk(input: &str) -> Result<Vtk> {
    tuple((header, line_end, title, line_end, format))
        .map(|(version, _, title, _, format)| Vtk {
            version,
            title: title.to_string(),
            format,
        })
        .parse(input)
}

#[cfg(test)]
mod test {
    use super::{Format, Vtk};
    use nom::Finish;

    #[test]
    fn vtk() {
        let (residual, out) = super::vtk(
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
            Vtk {
                version: (2, 0),
                title: "Cube example".to_string(),
                format: Format::ASCII,
            }
        );
    }
}
