//! Parser of [VTK legacy format](https://vtk.org/wp-content/uploads/2015/04/file-formats.pdf)

mod data;
mod header;

pub use data::*;
pub use header::*;

use nom::{
    bytes::complete::*, character::complete::*, error::VerboseError, sequence::tuple, Parser,
};

pub type Result<'input, T> = nom::IResult<&'input str, T, VerboseError<&'input str>>;

pub fn line_end(input: &str) -> Result<()> {
    tuple((space0, line_ending)).map(|_| ()).parse(input)
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
    use super::{Coordinate, Data3, Dimension, Format, RectlinearGrid, StructuredGrid, Vtk};
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
