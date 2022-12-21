use crate::*;
use nom::{bytes::complete::*, character::complete::*, combinator::opt, sequence::tuple, Parser};

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

// POINTS n dataType
// p0x p0y p0z
// p1x p1y p1z
// ...
pub fn points(input: &str) -> Result<Data3> {
    let (input, (_, _, n, _, data_type, _)) = tuple((
        tag("POINTS"),
        multispace1,
        uint::<usize>,
        multispace1,
        data_type,
        multispace1,
    ))
    .parse(input)?;
    data3(data_type, n).parse(input)
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
    let (input, points) = points(input)?;
    Ok((input, StructuredGrid { dimension, points }))
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct RectlinearGrid {
    pub dimension: Dimension,
    pub x_coodinates: Data1D,
    pub y_coodinates: Data1D,
    pub z_coodinates: Data1D,
}

fn coordinate(tag_: &'static str) -> impl FnMut(&str) -> Result<Data1D> {
    move |input| {
        let (input, (_, _, n, _, ty, _)) = tuple((
            tag(tag_),
            multispace1,
            uint::<usize>,
            multispace1,
            data_type,
            multispace1,
        ))
        .parse(input)?;
        data1d(ty, n).parse(input)
    }
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
    let (input, (dimension, _)) = tuple((dimension, multispace1)).parse(input)?;
    let (input, (x_coodinates, _)) =
        tuple((coordinate("X_COORDINATES"), multispace1)).parse(input)?;
    let (input, (y_coodinates, _)) =
        tuple((coordinate("Y_COORDINATES"), multispace1)).parse(input)?;
    let (input, z_coodinates) = coordinate("Z_COORDINATES").parse(input)?;
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
    pub vertices: Option<Vec<Vec<u64>>>,
    pub lines: Option<Vec<Vec<u64>>>,
    pub polygons: Option<Vec<Vec<u64>>>,
    pub triangle_strips: Option<Vec<Vec<u64>>>,
}

fn indices2d(tag_: &'static str) -> impl FnMut(&str) -> Result<Vec<Vec<u64>>> {
    move |input| {
        let (input, (_, _, n, _, size, _)) = tuple((
            tag(tag_),
            multispace1,
            uint::<usize>,
            multispace1,
            uint::<usize>,
            multispace1,
        ))
        .parse(input)?;
        take_n_m(n, size / n).parse(input)
    }
}

// DATASET POLYDATA
pub fn polydata(input: &str) -> Result<Polydata> {
    let (input, _) =
        tuple((tag("DATASET"), multispace1, tag("POLYDATA"), multispace1)).parse(input)?;
    let (input, points) = points(input)?;
    let (input, vertices) =
        opt(tuple((multispace1, indices2d("VERTICES"))).map(|x| x.1)).parse(input)?;
    let (input, lines) = opt(tuple((multispace1, indices2d("LINES"))).map(|x| x.1)).parse(input)?;
    let (input, polygons) =
        opt(tuple((multispace1, indices2d("POLYGONS"))).map(|x| x.1)).parse(input)?;
    let (input, triangle_strips) =
        opt(tuple((multispace1, indices2d("TRIANGLE_STRIPS"))).map(|x| x.1)).parse(input)?;
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
