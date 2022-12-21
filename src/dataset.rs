use crate::*;
use nom::{bytes::complete::*, character::complete::*, sequence::tuple, Parser};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Dimension {
    pub nx: u64,
    pub ny: u64,
    pub nz: u64,
}

pub fn dimension(input: &str) -> Result<Dimension> {
    tuple((tag("DIMENSIONS"), multispace1, u64::parse3))
        .map(|(_, _, [nx, ny, nz])| Dimension { nx, ny, nz })
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
    let (input, _tag) = tuple((
        tag("DATASET"),
        multispace1,
        tag("STRUCTURED_POINTS"),
        multispace1,
    ))
    .parse(input)?;

    // DIMENSIONS nx ny nz
    let (input, (dimension, _)) = tuple((dimension, multispace1)).parse(input)?;

    // ORIGIN x y z
    let (input, (_, _, origin, _)) =
        tuple((tag("ORIGIN"), multispace1, f64::parse3, multispace1)).parse(input)?;

    // SPACING sx sy sz
    let (input, (_, _, spacing)) =
        tuple((tag("SPACING"), multispace1, f64::parse3)).parse(input)?;

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
    let (input, _) = tuple((
        tag("DATASET"),
        multispace1,
        tag("STRUCTURED_GRID"),
        multispace1,
    ))
    .parse(input)?;

    // DIMENSIONS nx ny nz
    let (input, (dimension, _)) = tuple((dimension, multispace1)).parse(input)?;

    // POINTS n dataType
    let (input, (_, _, n, _, data_type, _)) = tuple((
        tag("POINTS"),
        multispace1,
        uint::<usize>,
        multispace1,
        data_type,
        multispace1,
    ))
    .parse(input)?;

    // p0x p0y p0z
    // p1x p1y p1z
    // ...
    let (input, points) = data3(data_type, n).parse(input)?;

    Ok((input, StructuredGrid { dimension, points }))
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct RectlinearGrid {
    pub dimension: Dimension,
    pub x_coodinates: Data1D,
    pub y_coodinates: Data1D,
    pub z_coodinates: Data1D,
}

pub fn rectlinear_grid(input: &str) -> Result<RectlinearGrid> {
    // DATASET RECTILINEAR_GRID
    let (input, _) = tuple((
        tag("DATASET"),
        multispace1,
        tag("RECTILINEAR_GRID"),
        multispace1,
    ))
    .parse(input)?;
    // DIMENSIONS nx ny nz
    let (input, (dimension, _)) = tuple((dimension, multispace1)).parse(input)?;
    // X_COORDINATES nx dataType
    let (input, (_, _, nx, _, ty, _)) = tuple((
        tag("X_COORDINATES"),
        multispace1,
        uint::<usize>,
        multispace1,
        data_type,
        multispace1,
    ))
    .parse(input)?;
    // x0 x1 ...
    let (input, (x_coodinates, _)) = tuple((data1d(ty, nx), multispace1)).parse(input)?;
    // Y_COORDINATES ny dataType
    let (input, (_, _, ny, _, ty, _)) = tuple((
        tag("Y_COORDINATES"),
        multispace1,
        uint::<usize>,
        multispace1,
        data_type,
        multispace1,
    ))
    .parse(input)?;
    // y0 y1 ...
    let (input, (y_coodinates, _)) = tuple((data1d(ty, ny), multispace1)).parse(input)?;
    // Z_COORDINATES nz dataType
    let (input, (_, _, nz, _, ty, _)) = tuple((
        tag("Z_COORDINATES"),
        multispace1,
        uint::<usize>,
        multispace1,
        data_type,
        multispace1,
    ))
    .parse(input)?;
    // z0 z1 ...
    let (input, z_coodinates) = data1d(ty, nz).parse(input)?;

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

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Polydata {
    pub points: Data3,
    pub vertices: Vec<Vec<u64>>,
    pub lines: Vec<Vec<u64>>,
    pub polygons: Vec<Vec<u64>>,
    pub triangle_strips: Vec<Vec<u64>>,
}

pub fn polydata(input: &str) -> Result<Polydata> {
    // DATASET POLYDATA
    let (input, _) =
        tuple((tag("DATASET"), multispace1, tag("POLYDATA"), multispace1)).parse(input)?;

    // POINTS n dataType
    let (input, (_, _, n, _, data_type, _)) = tuple((
        tag("POINTS"),
        multispace1,
        uint::<usize>,
        multispace1,
        data_type,
        multispace1,
    ))
    .parse(input)?;
    // p0x p0y p0z
    // p1x p1y p1z
    // ...
    let (input, (points, _)) = tuple((data3(data_type, n), multispace1)).parse(input)?;

    // VERTICES n size
    let (input, (_, _, n, _, size, _)) = tuple((
        tag("VERTICES"),
        multispace1,
        uint::<usize>,
        multispace1,
        uint::<usize>,
        multispace1,
    ))
    .parse(input)?;
    // numPoints0 i0 j0 k0 ...
    // numPoints1 i1 j1 k1 ...
    // ...
    let (input, vertices) = take_n_m(n, size / n).parse(input)?;

    // LINES n size
    let (input, (_, _, n, _, size, _)) = tuple((
        tag("LINES"),
        multispace1,
        uint::<usize>,
        multispace1,
        uint::<usize>,
        multispace1,
    ))
    .parse(input)?;
    // numPoints0 i0 j0 k0 ...
    // numPoints1 i1 j1 k1 ...
    // ...
    let (input, lines) = take_n_m(n, size / n).parse(input)?;

    // POLYGONS n size
    let (input, (_, _, n, _, size, _)) = tuple((
        tag("POLYGONS"),
        multispace1,
        uint::<usize>,
        multispace1,
        uint::<usize>,
        multispace1,
    ))
    .parse(input)?;
    // numPoints0 i0 j0 k0 ...
    // numPoints1 i1 j1 k1 ...
    // ...
    let (input, polygons) = take_n_m(n, size / n).parse(input)?;

    // TRIANGLE_STRIPS n size
    let (input, (_, _, n, _, size, _)) = tuple((
        tag("TRIANGLE_STRIPS"),
        multispace1,
        uint::<usize>,
        multispace1,
        uint::<usize>,
        multispace1,
    ))
    .parse(input)?;
    // numPoints0 i0 j0 k0 ...
    // numPoints1 i1 j1 k1 ...
    // ...
    let (input, triangle_strips) = take_n_m(n, size / n).parse(input)?;

    Ok((
        input,
        Polydata {
            points,
            vertices,
            lines,
            polygons,
            triangle_strips,
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
