use crate::*;
use nom::{bytes::complete::*, character::complete::*, sequence::tuple, Parser};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Dimension {
    pub nx: u64,
    pub ny: u64,
    pub nz: u64,
}

pub fn dimension(input: &str) -> Result<Dimension> {
    tuple((space0, tag("DIMENSIONS"), space1, u64::parse3))
        .map(|(_, _tag, _, [nx, ny, nz])| Dimension { nx, ny, nz })
        .parse(input)
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct StructuredPoints {
    pub dimension: Dimension,
    pub origin: [f64; 3],
    pub spacing: [f64; 3],
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
    let (input, (_, _, _, n, _, data_type, _)) = tuple((
        space0,
        tag("POINTS"),
        space1,
        uint::<usize>,
        space1,
        data_type,
        line_end,
    ))
    .parse(input)?;

    // p0x p0y p0z
    // p1x p1y p1z
    // ...
    let (input, (_, points)) = tuple((multispace0, data3(data_type, n))).parse(input)?;

    Ok((input, StructuredGrid { dimension, points }))
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct RectlinearGrid {
    dimension: Dimension,
    x_coodinates: Data1D,
    y_coodinates: Data1D,
    z_coodinates: Data1D,
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
    let (input, (_, _, _, nx, _, ty, _)) = tuple((
        space0,
        tag("X_COORDINATES"),
        space1,
        uint::<usize>,
        space1,
        data_type,
        line_end,
    ))
    .parse(input)?;
    // x0 x1 ...
    let (input, (_, x_coodinates)) = tuple((multispace0, data1d(ty, nx))).parse(input)?;
    let (input, _) = line_end(input)?;
    // Y_COORDINATES ny dataType
    let (input, (_, _, _, ny, _, ty, _)) = tuple((
        space0,
        tag("Y_COORDINATES"),
        space1,
        uint::<usize>,
        space1,
        data_type,
        line_end,
    ))
    .parse(input)?;
    // y0 y1 ...
    let (input, (_, y_coodinates)) = tuple((multispace0, data1d(ty, ny))).parse(input)?;
    let (input, _) = line_end(input)?;
    // Z_COORDINATES nz dataType
    let (input, (_, _, _, nz, _, ty, _)) = tuple((
        space0,
        tag("Z_COORDINATES"),
        space1,
        uint::<usize>,
        space1,
        data_type,
        line_end,
    ))
    .parse(input)?;
    // z0 z1 ...
    let (input, (_, z_coodinates)) = tuple((multispace0, data1d(ty, nz))).parse(input)?;

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

#[cfg(test)]
mod test {
    use super::{Data1D, Data3, Dimension, RectlinearGrid, StructuredGrid};
    use nom::Finish;

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
                x_coodinates: Data1D::Float(vec![0.0, 1.0, 2.0, 3.0, 4.0]),
                y_coodinates: Data1D::Float(vec![0.0, 1.0, 2.0, 3.0, 4.0]),
                z_coodinates: Data1D::Float(vec![0.0, 1.0, 2.0, 3.0, 4.0]),
            }
        );
    }
}
