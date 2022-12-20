use crate::{line_end, Result};
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
        separated_list0(multispace1, D::parse3).parse(input)
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

#[cfg(test)]
mod test {
    use super::{Data3, DataType};
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
}
