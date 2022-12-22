use crate::*;
use nom::{bytes::complete::*, character::complete::*, combinator::opt, sequence::tuple, Parser};

fn name(input: &str) -> Result<&str> {
    take_till(|c: char| c.is_whitespace()).parse(input)
}

/// Scalars
///
/// Scalar definition includes specification of a lookup table.
/// The definition of a lookup table is optional.
/// If not specified, the default VTK table will be used (and tableName should be "default").
/// Also note that the numComp variable is optional by default the number of components is equal to one.
/// (The parameter numComp must range between (1,4) inclusive;
/// in versions of VTK prior to vtk2.3 this parameter was not supported.)
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Scalars {
    name: String,
    table_name: Option<String>,
    num_comp: u8,
    scalars: Data1D,
}

pub fn scalars(n: usize) -> impl FnMut(&str) -> Result<Scalars> {
    move |input| {
        let (input, (_tag, _, data_name, _, data_type, num_comp)) = tuple((
            tag("SCALARS"),
            multispace1,
            name,
            multispace1,
            data_type,
            opt(tuple((multispace1, uint::<u8>)).map(|x| x.1)),
        ))
        .parse(input)?;
        let (input, table_name) = opt(tuple((tag("LOOKUP_TABLE"), multispace1, name))
            .map(|(_, _, table_name)| table_name.to_string()))
        .parse(input)?;
        let (input, scalars) = data1d(data_type, n).parse(input)?;
        Ok((
            input,
            Scalars {
                name: data_name.to_string(),
                table_name,
                num_comp: num_comp.unwrap_or(1),
                scalars,
            },
        ))
    }
}
