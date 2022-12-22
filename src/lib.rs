//! Parser of [VTK legacy format](https://vtk.org/wp-content/uploads/2015/04/file-formats.pdf)

pub mod attribute;
pub mod dataset;
pub mod header;
pub mod primitive;

use header::*;
use primitive::*;

use nom::error::VerboseError;

pub type Result<'input, T> = nom::IResult<&'input str, T, VerboseError<&'input str>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Vtk {
    pub header: Header,
}

pub fn vtk(input: &str) -> Result<Vtk> {
    let (input, header) = header(input)?;
    Ok((input, Vtk { header }))
}
