//! VTK legacy format parser as an example for nom parser combinator
//!
//! VTK legacy format is defined in <https://vtk.org/wp-content/uploads/2015/04/file-formats.pdf>
//!

use nom::{
    branch::alt,
    bytes::complete::*,
    character::complete::*,
    combinator::opt,
    error::{VerboseError, VerboseErrorKind},
    multi::separated_list0,
    number::complete::*,
    sequence::tuple,
    Parser,
};
use num_traits::{Signed, Unsigned};
use std::str::FromStr;

pub type Result<'input, T> = nom::IResult<&'input str, T, VerboseError<&'input str>>;

fn failure<'input>(input: &'input str, msg: &'static str) -> nom::Err<VerboseError<&'input str>> {
    nom::Err::Failure(VerboseError {
        errors: vec![(input, VerboseErrorKind::Context(msg))],
    })
}

pub fn uint<I: Unsigned + FromStr>(input: &str) -> Result<I> {
    let (residual, dig) = digit1(input)?;
    let num: I = dig
        .parse()
        .map_err(|_| failure(input, "unsigned integer"))?;
    Ok((residual, num))
}

pub fn int<I: Signed + FromStr>(input: &str) -> Result<I> {
    let (input, sign) = opt(char('-')).parse(input)?;
    let (residual, dig) = digit1(input)?;
    let num: I = if sign.is_some() {
        format!("-{}", dig).parse()
    } else {
        dig.parse()
    }
    .map_err(|_| failure(input, "signed integer"))?;
    Ok((residual, num))
}

pub fn line_end(input: &str) -> Result<()> {
    tuple((space0, line_ending)).map(|_| ()).parse(input)
}

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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub enum DataType {
    Bit,
    UnsignedChar,
    Char,
    UnsignedShort,
    Short,
    UnsignedInt,
    Int,
    UnsignedLong,
    Long,
    Float,
    Double,
}

pub fn data_type(input: &str) -> Result<DataType> {
    alt((
        tag("bit").map(|_| DataType::Bit),
        tag("unsigned_char").map(|_| DataType::UnsignedChar),
        tag("char").map(|_| DataType::Bit),
        tag("unsigned_short").map(|_| DataType::UnsignedShort),
        tag("short").map(|_| DataType::Short),
        tag("unsigned_int").map(|_| DataType::UnsignedInt),
        tag("int").map(|_| DataType::Int),
        tag("unsigned_long").map(|_| DataType::UnsignedLong),
        tag("long").map(|_| DataType::Long),
        tag("float").map(|_| DataType::Float),
        tag("double").map(|_| DataType::Double),
    ))
    .parse(input)
}

pub trait Data: Sized {
    fn parse(input: &str) -> Result<Self>;
    fn parse3(input: &str) -> Result<[Self; 3]> {
        tuple((
            space0,
            Self::parse,
            space1,
            Self::parse,
            space1,
            Self::parse,
        ))
        .map(|(_, x, _, y, _, z)| [x, y, z])
        .parse(input)
    }
    fn parse_vec(input: &str) -> Result<Vec<Self>> {
        separated_list0(space1, Self::parse).parse(input)
    }
}

macro_rules! impl_data {
    ($data:ty, $f:ident) => {
        impl Data for $data {
            fn parse(input: &str) -> Result<Self> {
                $f(input)
            }
        }
    };
}

impl_data!(u8, uint);
impl_data!(u16, uint);
impl_data!(u32, uint);
impl_data!(u64, uint);
impl_data!(i8, int);
impl_data!(i16, int);
impl_data!(i32, int);
impl_data!(i64, int);
impl_data!(f32, float);
impl_data!(f64, double);

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Data3 {
    Bit(Vec<[bool; 3]>),
    UnsignedChar(Vec<[u8; 3]>),
    Char(Vec<[i8; 3]>),
    UnsignedShort(Vec<[u16; 3]>),
    Short(Vec<[i16; 3]>),
    UnsignedInt(Vec<[u32; 3]>),
    Int(Vec<[i32; 3]>),
    UnsignedLong(Vec<[u64; 3]>),
    Long(Vec<[i64; 3]>),
    Float(Vec<[f32; 3]>),
    Double(Vec<[f64; 3]>),
}

pub fn data3(data_type: DataType, input: &str) -> Result<Data3> {
    fn inner<D: Data>(input: &str) -> Result<Vec<[D; 3]>> {
        separated_list0(line_end, D::parse3).parse(input)
    }

    match data_type {
        DataType::Bit => unimplemented!(),
        DataType::Char => inner.map(Data3::Char).parse(input),
        DataType::UnsignedChar => inner.map(Data3::UnsignedChar).parse(input),
        DataType::Short => inner.map(Data3::Short).parse(input),
        DataType::UnsignedShort => inner.map(Data3::UnsignedShort).parse(input),
        DataType::Int => inner.map(Data3::Int).parse(input),
        DataType::UnsignedInt => inner.map(Data3::UnsignedInt).parse(input),
        DataType::Long => inner.map(Data3::Long).parse(input),
        DataType::UnsignedLong => inner.map(Data3::UnsignedLong).parse(input),
        DataType::Float => inner.map(Data3::Float).parse(input),
        DataType::Double => inner.map(Data3::Double).parse(input),
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum DataVec {
    Bit(Vec<Vec<bool>>),
    UnsignedChar(Vec<Vec<u8>>),
    Char(Vec<Vec<i8>>),
    UnsignedShort(Vec<Vec<u16>>),
    Short(Vec<Vec<i16>>),
    UnsignedInt(Vec<Vec<u32>>),
    Int(Vec<Vec<i32>>),
    UnsignedLong(Vec<Vec<u64>>),
    Long(Vec<Vec<i64>>),
    Float(Vec<Vec<f32>>),
    Double(Vec<Vec<f64>>),
}

pub fn data_vec(data_type: DataType, input: &str) -> Result<DataVec> {
    fn inner<D: Data>(input: &str) -> Result<Vec<Vec<D>>> {
        separated_list0(line_end, D::parse_vec).parse(input)
    }

    match data_type {
        DataType::Bit => unimplemented!(),
        DataType::Char => inner.map(DataVec::Char).parse(input),
        DataType::UnsignedChar => inner.map(DataVec::UnsignedChar).parse(input),
        DataType::Short => inner.map(DataVec::Short).parse(input),
        DataType::UnsignedShort => inner.map(DataVec::UnsignedShort).parse(input),
        DataType::Int => inner.map(DataVec::Int).parse(input),
        DataType::UnsignedInt => inner.map(DataVec::UnsignedInt).parse(input),
        DataType::Long => inner.map(DataVec::Long).parse(input),
        DataType::UnsignedLong => inner.map(DataVec::UnsignedLong).parse(input),
        DataType::Float => inner.map(DataVec::Float).parse(input),
        DataType::Double => inner.map(DataVec::Double).parse(input),
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Dimension {
    nx: u64,
    ny: u64,
    nz: u64,
}

pub fn dimension(input: &str) -> Result<Dimension> {
    tuple((space0, tag("DIMENSIONS"), space1, u64::parse3))
        .map(|(_, _tag, _, [nx, ny, nz])| Dimension { nx, ny, nz })
        .parse(input)
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct StructuredPoints {
    dimension: Dimension,
    origin: [f64; 3],
    spacing: [f64; 3],
}

pub fn structured_points(input: &str) -> Result<StructuredPoints> {
    // DATASET STRUCTURED_POINTS
    let (input, _tag) =
        tuple((tag("DATASET"), space1, tag("STRUCTURED_POINTS"), line_end)).parse(input)?;

    // DIMENSIONS nx ny nz
    let (input, (dimension, _)) = tuple((dimension, line_end)).parse(input)?;

    // ORIGIN x y z
    let (input, (_, _, origin, _)) =
        tuple((tag("ORIGIN"), space1, f64::parse3, line_end)).parse(input)?;

    // SPACING sx sy sz
    let (input, (_, _, spacing, _)) =
        tuple((tag("SPACING"), space1, f64::parse3, line_end)).parse(input)?;

    Ok((
        input,
        StructuredPoints {
            dimension,
            origin,
            spacing,
        },
    ))
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct StructuredGrid {
    dimension: Dimension,
    points: Data3,
}

pub fn structured_grid(input: &str) -> Result<StructuredGrid> {
    // DATASET STRUCTURED_GRID
    let (input, _) =
        tuple((tag("DATASET"), space1, tag("STRUCTURED_GRID"), line_end)).parse(input)?;

    // DIMENSIONS nx ny nz
    let (input, (_, dimension, _)) = tuple((space0, dimension, line_end)).parse(input)?;

    // POINTS n dataType
    let (input, (_, _, _, _n, _, data_type, _)) = tuple((
        space0,
        tag("POINTS"),
        space1,
        uint::<u64>,
        space1,
        data_type,
        line_end,
    ))
    .parse(input)?;

    // p0x p0y p0z
    // p1x p1y p1z
    // ...
    let (input, points) = data3(data_type, input)?;

    Ok((input, StructuredGrid { dimension, points }))
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Coordinate {
    Bit(Vec<bool>),
    UnsignedChar(Vec<u8>),
    Char(Vec<i8>),
    UnsignedShort(Vec<u16>),
    Short(Vec<i16>),
    UnsignedInt(Vec<u32>),
    Int(Vec<i32>),
    UnsignedLong(Vec<u64>),
    Long(Vec<i64>),
    Float(Vec<f32>),
    Double(Vec<f64>),
}

pub fn coordinate(data_type: DataType, input: &str) -> Result<Coordinate> {
    let (input, _) = space0(input)?;
    match data_type {
        DataType::Bit => unimplemented!(),
        DataType::Char => i8::parse_vec.map(Coordinate::Char).parse(input),
        DataType::UnsignedChar => u8::parse_vec.map(Coordinate::UnsignedChar).parse(input),
        DataType::Short => i16::parse_vec.map(Coordinate::Short).parse(input),
        DataType::UnsignedShort => u16::parse_vec.map(Coordinate::UnsignedShort).parse(input),
        DataType::Int => i32::parse_vec.map(Coordinate::Int).parse(input),
        DataType::UnsignedInt => u32::parse_vec.map(Coordinate::UnsignedInt).parse(input),
        DataType::Long => i64::parse_vec.map(Coordinate::Long).parse(input),
        DataType::UnsignedLong => u64::parse_vec.map(Coordinate::UnsignedLong).parse(input),
        DataType::Float => f32::parse_vec.map(Coordinate::Float).parse(input),
        DataType::Double => f64::parse_vec.map(Coordinate::Double).parse(input),
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct RectlinearGrid {
    dimension: Dimension,
    x_coodinates: Coordinate,
    y_coodinates: Coordinate,
    z_coodinates: Coordinate,
}

pub fn rectlinear_grid(input: &str) -> Result<RectlinearGrid> {
    // DATASET RECTILINEAR_GRID
    let (input, _) = tuple((
        space0,
        tag("DATASET"),
        space1,
        tag("RECTILINEAR_GRID"),
        line_end,
    ))
    .parse(input)?;
    // DIMENSIONS nx ny nz
    let (input, (_, dimension, _)) = tuple((space0, dimension, line_end)).parse(input)?;
    // X_COORDINATES nx dataType
    let (input, (_, _, _, _nx, _, ty, _)) = tuple((
        space0,
        tag("X_COORDINATES"),
        space1,
        uint::<u64>,
        space1,
        data_type,
        line_end,
    ))
    .parse(input)?;
    // x0 x1 ...
    let (input, x_coodinates) = coordinate(ty, input)?;
    let (input, _) = line_end(input)?;
    // Y_COORDINATES ny dataType
    let (input, (_, _, _, _ny, _, ty, _)) = tuple((
        space0,
        tag("Y_COORDINATES"),
        space1,
        uint::<u64>,
        space1,
        data_type,
        line_end,
    ))
    .parse(input)?;
    // y0 y1 ...
    let (input, y_coodinates) = coordinate(ty, input)?;
    let (input, _) = line_end(input)?;
    // Z_COORDINATES nz dataType
    let (input, (_, _, _, _nz, _, ty, _)) = tuple((
        space0,
        tag("Z_COORDINATES"),
        space1,
        uint::<u64>,
        space1,
        data_type,
        line_end,
    ))
    .parse(input)?;
    // z0 z1 ...
    let (input, z_coodinates) = coordinate(ty, input)?;

    Ok((
        input,
        RectlinearGrid {
            dimension,
            x_coodinates,
            y_coodinates,
            z_coodinates,
        },
    ))
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
    use super::{
        Coordinate, Data3, DataType, Dimension, Format, RectlinearGrid, StructuredGrid, Vtk,
    };
    use nom::Finish;

    #[test]
    fn uint() {
        assert_eq!(super::uint::<u32>("1234").finish().unwrap(), ("", 1234));
        assert!(super::uint::<u32>("abcd").finish().is_err());
    }

    #[test]
    fn int() {
        assert_eq!(super::int::<i32>("1234").finish().unwrap(), ("", 1234));
        assert_eq!(super::int::<i32>("-1234").finish().unwrap(), ("", -1234));
    }

    #[test]
    fn header() {
        assert_eq!(
            super::header("# vtk DataFile Version 2.0")
                .finish()
                .unwrap(),
            ("", (2, 0))
        );
    }

    #[test]
    fn data3() {
        let (residual, out) = super::data3(
            DataType::Float,
            r#"
            0.0 1.0 2.0
            3.0 4.0 5.0
            "#
            .trim(),
        )
        .finish()
        .unwrap();
        assert_eq!(residual, "");
        assert_eq!(out, Data3::Float(vec![[0.0, 1.0, 2.0], [3.0, 4.0, 5.0],]));

        let (residual, out) = super::data3(
            DataType::UnsignedInt,
            r#"
            0 1 2
            3 4 5
            "#
            .trim(),
        )
        .finish()
        .unwrap();
        assert_eq!(residual, "");
        assert_eq!(out, Data3::UnsignedInt(vec![[0, 1, 2], [3, 4, 5],]));
    }

    #[test]
    fn structured_grid() {
        let (residual, out) = super::structured_grid(
            r#"
            DATASET STRUCTURED_GRID
            DIMENSIONS 2 2 2
            POINTS 8 unsigned_int
            0 0 0
            1 0 0
            0 1 0
            1 1 0
            0 0 1
            1 0 1
            0 1 1
            1 1 1
            "#
            .trim(),
        )
        .finish()
        .unwrap();
        assert_eq!(residual, "");
        assert_eq!(
            out,
            StructuredGrid {
                dimension: Dimension {
                    nx: 2,
                    ny: 2,
                    nz: 2
                },
                points: Data3::UnsignedInt(vec![
                    [0, 0, 0],
                    [1, 0, 0],
                    [0, 1, 0],
                    [1, 1, 0],
                    [0, 0, 1],
                    [1, 0, 1],
                    [0, 1, 1],
                    [1, 1, 1],
                ])
            }
        );
    }

    #[test]
    fn rectlinear_grid() {
        let (residual, out) = super::rectlinear_grid(
            r#"
            DATASET RECTILINEAR_GRID
            DIMENSIONS 5 5 5
            X_COORDINATES 5 float
            0.0 1.0 2.0 3.0 4.0
            Y_COORDINATES 5 float
            0.0 1.0 2.0 3.0 4.0
            Z_COORDINATES 5 float
            0.0 1.0 2.0 3.0 4.0
            "#
            .trim(),
        )
        .finish()
        .unwrap();
        assert_eq!(residual, "");
        assert_eq!(
            out,
            RectlinearGrid {
                dimension: Dimension {
                    nx: 5,
                    ny: 5,
                    nz: 5
                },
                x_coodinates: Coordinate::Float(vec![0.0, 1.0, 2.0, 3.0, 4.0]),
                y_coodinates: Coordinate::Float(vec![0.0, 1.0, 2.0, 3.0, 4.0]),
                z_coodinates: Coordinate::Float(vec![0.0, 1.0, 2.0, 3.0, 4.0]),
            }
        );
    }

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
