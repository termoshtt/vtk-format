use crate::*;
use nom::{bytes::complete::*, character::complete::*, combinator::opt, sequence::tuple, Parser};
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::FromPrimitive;

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
pub fn points(input: &str) -> Result<Data3N> {
    let (input, (_, _, n, _, data_type, _)) = tuple((
        tag("POINTS"),
        multispace1,
        uint::<usize>,
        multispace1,
        data_type,
        multispace1,
    ))
    .parse(input)?;
    data3n(data_type, n).parse(input)
}

/// Structured Points
///
/// The file format supports 1D, 2D, and 3D structured point datasets.
/// The dimensions `nx`, `ny`, `nz` must be greater than or equal to 1.
/// The data spacing `sx`, `sy`, `sz` must be greater than 0.
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

/// Structured Grid
///
/// The file format supports 1D, 2D, and 3D structured grid datasets.
/// The dimensions `nx`, `ny`, `nz` must be greater than or equal to `1`.
/// The point coordinates are defined by the data in the POINTS section.
/// This consists of x-y-z data values for each point.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct StructuredGrid {
    pub dimension: Dimension,
    pub points: Data3N,
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

/// Rectilinear Grid
///
/// A rectilinear grid defines a dataset with regular topology,
/// and semi-regular geometry aligned along the x-y-z coordinate axes.
/// The geometry is defined by three lists of monotonically increasing coordinate values,
/// one list for each of the x-y-z coordinate axes.
/// The topology is defined by specifying the grid dimensions,
/// which must be greater than or equal to `1`.
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

/// Polygonal Data
///
/// The polygonal dataset consists of arbitrary combinations of surface graphics
/// primitives vertices (and polyvertices), lines (and polylines),
/// polygons (of various types), and triangle strips.
/// Polygonal data is defined by the `POINTS`, `VERTICES`, `LINES`, `POLYGONS`,
/// or `TRIANGLE_STRIPS` sections.
/// The `POINTS` definition is the same as we saw for structured grid datasets.
/// The `VERTICES`, `LINES`, `POLYGONS`, or `TRIANGLE_STRIPS` keywords define the polygonal dataset topology.
/// Each of these keywords requires two parameters:
/// the number of cells n and the size of the cell list size.
/// The cell list size is the total number of integer values required to represent the list
/// (i.e., sum of `numPoints` and connectivity indices over each cell).
/// None of the keywords `VERTICES`, `LINES`, `POLYGONS`, or `TRIANGLE_STRIPS` is required
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Polydata {
    pub points: Data3N,
    pub vertices: Option<Vec<Vec<u64>>>,
    pub lines: Option<Vec<Vec<u64>>>,
    pub polygons: Option<Vec<Vec<u64>>>,
    pub triangle_strips: Option<Vec<Vec<u64>>>,
}

fn indices2d(tag_: &'static str) -> impl FnMut(&str) -> Result<Vec<Vec<u64>>> {
    move |input| {
        let (mut input, (_, _, n, _, mut size)) = tuple((
            tag(tag_),
            multispace1,
            uint::<usize>,
            multispace1,
            uint::<usize>,
        ))
        .parse(input)?;
        let mut out = Vec::with_capacity(n);
        for _ in 0..n {
            let (sub, (_, num_points, _)) =
                tuple((multispace1, uint::<usize>, multispace1))(input)?;
            let (sub, current) = take_n(num_points)(sub)?;
            out.push(current);
            size -= num_points + 1;
            input = sub;
        }
        assert_eq!(size, 0);
        Ok((input, out))
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

/// Unstructured Grid
///
/// The unstructured grid dataset consists of arbitrary combinations of any possible cell type.
/// Unstructured grids are defined by points, cells, and cell types.
/// The `CELLS` keyword requires two parameters: the number of cells n and the size of the cell list size.
/// The cell list size is the total number of integer values required to represent the list
/// (i.e., sum of numPoints and connectivity indices over each cell).
/// The `CELL_TYPES` keyword requires a single parameter: the number of cells n.
/// This value should match the value specified by the `CELLS` keyword.
/// The cell types data is a single integer value per cell that specified cell type (see vtkCell.h or Figure 2).
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct UnstructuredGrid {
    pub points: Data3N,
    pub cells: Vec<Vec<u64>>,
    pub cell_types: Vec<CellType>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, FromPrimitive, ToPrimitive)]
pub enum CellType {
    // Linear cell types
    Vertex = 1,
    PolyVertex = 2,
    Line = 3,
    PolyLine = 4,
    Triangle = 5,
    TriangleStrip = 6,
    Polygon = 7,
    Pixel = 8,
    Quad = 9,
    Tetra = 10,
    Voxel = 11,
    Hexahedron = 12,
    Wedge = 13,
    Pyramid = 14,

    // Non-linear cell types
    QuadraticEdge = 21,
    QuadraticTriangle = 22,
    QuadraticQuad = 23,
    QuadraticTetra = 24,
    QuadraticHexahedron = 25,
}

pub fn unstructured_grid(input: &str) -> Result<UnstructuredGrid> {
    // DATASET UNSTRUCTURED_GRID
    let (input, _) = tuple((
        tag("DATASET"),
        multispace1,
        tag("UNSTRUCTURED_GRID"),
        multispace1,
    ))
    .parse(input)?;
    let (input, points) = points(input)?;
    let (input, _) = multispace1(input)?;
    let (input, cells) = indices2d("CELLS").parse(input)?;
    let (input, _) = multispace1(input)?;

    let (input, (_, _, n, _)) =
        tuple((tag("CELL_TYPES"), multispace1, uint::<usize>, multispace1)).parse(input)?;
    let (input, cell_types) = take_n::<u8>(n).parse(input)?;
    Ok((
        input,
        UnstructuredGrid {
            points,
            cells,
            cell_types: cell_types
                .into_iter()
                .map(|i| FromPrimitive::from_u8(i).unwrap())
                .collect(),
        },
    ))
}

#[cfg(test)]
mod test {
    use super::{
        CellType, Data1D, Data3N, Dimension, Polydata, RectlinearGrid, StructuredGrid,
        UnstructuredGrid,
    };
    use nom::Finish;
    use num_traits::FromPrimitive;

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
                points: Data3N::UnsignedInt(vec![
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

    #[test]
    fn polydata() {
        let (residual, out) = super::polydata(
            r#"
            DATASET POLYDATA
            POINTS 8 float
            0.0 0.0 0.0
            1.0 0.0 0.0
            1.0 1.0 0.0
            0.0 1.0 0.0
            0.0 0.0 1.0
            1.0 0.0 1.0
            1.0 1.0 1.0
            0.0 1.0 1.0
            POLYGONS 6 30
            4 0 1 2 3
            4 4 5 6 7
            4 0 1 5 4
            4 2 3 7 6
            4 0 4 7 3
            4 1 2 6 5
            "#
            .trim(),
        )
        .finish()
        .unwrap();
        assert_eq!(residual, "");
        assert_eq!(
            out,
            Polydata {
                points: Data3N::Float(vec![
                    [0.0, 0.0, 0.0],
                    [1.0, 0.0, 0.0],
                    [1.0, 1.0, 0.0],
                    [0.0, 1.0, 0.0],
                    [0.0, 0.0, 1.0],
                    [1.0, 0.0, 1.0],
                    [1.0, 1.0, 1.0],
                    [0.0, 1.0, 1.0],
                ]),
                polygons: Some(vec![
                    vec![0, 1, 2, 3],
                    vec![4, 5, 6, 7],
                    vec![0, 1, 5, 4],
                    vec![2, 3, 7, 6],
                    vec![0, 4, 7, 3],
                    vec![1, 2, 6, 5],
                ]),
                vertices: None,
                lines: None,
                triangle_strips: None,
            }
        );
    }

    #[test]
    fn unstructured_grid() {
        let (residual, out) = super::unstructured_grid(
            r#"
            DATASET UNSTRUCTURED_GRID
            POINTS 27 float
            0 0 0  1 0 0  2 0 0  0 1 0  1 1 0  2 1 0
            0 0 1  1 0 1  2 0 1  0 1 1  1 1 1  2 1 1
            0 1 2  1 1 2  2 1 2  0 1 3  1 1 3  2 1 3
            0 1 4  1 1 4  2 1 4  0 1 5  1 1 5  2 1 5
            0 1 6  1 1 6  2 1 6

            CELLS 11 60
            8 0 1 4 3 6 7 10 9
            8 1 2 5 4 7 8 11 10
            4 6 10 9 12
            4 5 11 10 14
            6 15 16 17 14 13 12
            6 18 15 19 16 20 17
            4 22 23 20 19
            3 21 22 18
            3 22 19 18
            2 26 25
            1 24

            CELL_TYPES 11
            12
            12
            10
            10
            7
            6
            9
            5
            5
            3
            1
            "#
            .trim(),
        )
        .finish()
        .unwrap();
        assert_eq!(residual, "");
        assert_eq!(
            out,
            UnstructuredGrid {
                points: Data3N::Float(vec![
                    [0.0, 0.0, 0.0],
                    [1.0, 0.0, 0.0],
                    [2.0, 0.0, 0.0],
                    [0.0, 1.0, 0.0],
                    [1.0, 1.0, 0.0],
                    [2.0, 1.0, 0.0],
                    [0.0, 0.0, 1.0],
                    [1.0, 0.0, 1.0],
                    [2.0, 0.0, 1.0],
                    [0.0, 1.0, 1.0],
                    [1.0, 1.0, 1.0],
                    [2.0, 1.0, 1.0],
                    [0.0, 1.0, 2.0],
                    [1.0, 1.0, 2.0],
                    [2.0, 1.0, 2.0],
                    [0.0, 1.0, 3.0],
                    [1.0, 1.0, 3.0],
                    [2.0, 1.0, 3.0],
                    [0.0, 1.0, 4.0],
                    [1.0, 1.0, 4.0],
                    [2.0, 1.0, 4.0],
                    [0.0, 1.0, 5.0],
                    [1.0, 1.0, 5.0],
                    [2.0, 1.0, 5.0],
                    [0.0, 1.0, 6.0],
                    [1.0, 1.0, 6.0],
                    [2.0, 1.0, 6.0],
                ]),
                cells: vec![
                    vec![0, 1, 4, 3, 6, 7, 10, 9],
                    vec![1, 2, 5, 4, 7, 8, 11, 10],
                    vec![6, 10, 9, 12],
                    vec![5, 11, 10, 14],
                    vec![15, 16, 17, 14, 13, 12],
                    vec![18, 15, 19, 16, 20, 17],
                    vec![22, 23, 20, 19],
                    vec![21, 22, 18],
                    vec![22, 19, 18],
                    vec![26, 25],
                    vec![24],
                ],
                cell_types: vec![
                    CellType::from_u8(12).unwrap(),
                    CellType::from_u8(12).unwrap(),
                    CellType::from_u8(10).unwrap(),
                    CellType::from_u8(10).unwrap(),
                    CellType::from_u8(7).unwrap(),
                    CellType::from_u8(6).unwrap(),
                    CellType::from_u8(9).unwrap(),
                    CellType::from_u8(5).unwrap(),
                    CellType::from_u8(5).unwrap(),
                    CellType::from_u8(3).unwrap(),
                    CellType::from_u8(1).unwrap(),
                ]
            }
        );
    }
}
