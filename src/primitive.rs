use crate::Result;
use nom::{
    branch::alt,
    bytes::complete::*,
    character::complete::*,
    combinator::opt,
    error::{VerboseError, VerboseErrorKind},
    multi::*,
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

pub fn take_n<D: Data>(n: usize) -> impl FnMut(&str) -> Result<Vec<D>> {
    move |input| {
        let mut out = Vec::with_capacity(n);
        let (mut input, first) = D::parse(input)?;
        out.push(first);
        for _ in 0..(n - 1) {
            let (sub_input, (_, nth)) = tuple((multispace1, D::parse)).parse(input)?;
            out.push(nth);
            input = sub_input;
        }
        Ok((input, out))
    }
}

pub fn take_3n<D: Data>(n: usize) -> impl FnMut(&str) -> Result<Vec<[D; 3]>> {
    move |input| {
        let data3 = |s| {
            tuple((D::parse, multispace1, D::parse, multispace1, D::parse))
                .map(|(d1, _, d2, _, d3)| [d1, d2, d3])
                .parse(s)
        };

        let mut out = Vec::with_capacity(n);
        let (mut input, first) = data3(input)?;
        out.push(first);
        for _ in 0..(n - 1) {
            let (sub_input, (_, nth)) = tuple((multispace1, data3)).parse(input)?;
            out.push(nth);
            input = sub_input;
        }
        Ok((input, out))
    }
}

pub fn take_n_m<D: Data>(
    size_outer: usize,
    size_inner: usize,
) -> impl FnMut(&str) -> Result<Vec<Vec<D>>> {
    move |input| {
        let mut outer = Vec::with_capacity(size_outer);
        let (mut input, first) = take_n(size_inner).parse(input)?;
        outer.push(first);
        for _ in 0..(size_outer - 1) {
            let (sub, (_, inner)) = tuple((multispace1, take_n(size_inner))).parse(input)?;
            outer.push(inner);
            input = sub;
        }
        Ok((input, outer))
    }
}

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

pub fn data3(data_type: DataType, n: usize) -> impl FnMut(&str) -> Result<Data3> {
    move |input| match data_type {
        DataType::Bit => unimplemented!(),
        DataType::Char => take_3n(n).map(Data3::Char).parse(input),
        DataType::UnsignedChar => take_3n(n).map(Data3::UnsignedChar).parse(input),
        DataType::Short => take_3n(n).map(Data3::Short).parse(input),
        DataType::UnsignedShort => take_3n(n).map(Data3::UnsignedShort).parse(input),
        DataType::Int => take_3n(n).map(Data3::Int).parse(input),
        DataType::UnsignedInt => take_3n(n).map(Data3::UnsignedInt).parse(input),
        DataType::Long => take_3n(n).map(Data3::Long).parse(input),
        DataType::UnsignedLong => take_3n(n).map(Data3::UnsignedLong).parse(input),
        DataType::Float => take_3n(n).map(Data3::Float).parse(input),
        DataType::Double => take_3n(n).map(Data3::Double).parse(input),
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Data1D {
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

pub fn data1d(data_type: DataType, n: usize) -> impl FnMut(&str) -> Result<Data1D> {
    move |input| match data_type {
        DataType::Bit => unimplemented!(),
        DataType::Char => take_n(n).map(Data1D::Char).parse(input),
        DataType::UnsignedChar => take_n(n).map(Data1D::UnsignedChar).parse(input),
        DataType::Short => take_n(n).map(Data1D::Short).parse(input),
        DataType::UnsignedShort => take_n(n).map(Data1D::UnsignedShort).parse(input),
        DataType::Int => take_n(n).map(Data1D::Int).parse(input),
        DataType::UnsignedInt => take_n(n).map(Data1D::UnsignedInt).parse(input),
        DataType::Long => take_n(n).map(Data1D::Long).parse(input),
        DataType::UnsignedLong => take_n(n).map(Data1D::UnsignedLong).parse(input),
        DataType::Float => take_n(n).map(Data1D::Float).parse(input),
        DataType::Double => take_n(n).map(Data1D::Double).parse(input),
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Data2D {
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

pub fn data2d(
    data_type: DataType,
    outer: usize,
    inner: usize,
) -> impl FnMut(&str) -> Result<Data2D> {
    move |input| match data_type {
        DataType::Bit => unimplemented!(),
        DataType::Char => take_n_m(outer, inner).map(Data2D::Char).parse(input),
        DataType::UnsignedChar => take_n_m(outer, inner)
            .map(Data2D::UnsignedChar)
            .parse(input),
        DataType::Short => take_n_m(outer, inner).map(Data2D::Short).parse(input),
        DataType::UnsignedShort => take_n_m(outer, inner)
            .map(Data2D::UnsignedShort)
            .parse(input),
        DataType::Int => take_n_m(outer, inner).map(Data2D::Int).parse(input),
        DataType::UnsignedInt => take_n_m(outer, inner).map(Data2D::UnsignedInt).parse(input),
        DataType::Long => take_n_m(outer, inner).map(Data2D::Long).parse(input),
        DataType::UnsignedLong => take_n_m(outer, inner)
            .map(Data2D::UnsignedLong)
            .parse(input),
        DataType::Float => take_n_m(outer, inner).map(Data2D::Float).parse(input),
        DataType::Double => take_n_m(outer, inner).map(Data2D::Double).parse(input),
    }
}

#[cfg(test)]
mod test {
    use super::{Data3, DataType};
    use nom::{Finish, Parser};

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
    fn take_n() {
        let (residual, taken) = super::take_n::<f32>(4)
            .parse(r#"1.0 2.0 3.0 4.0"#)
            .finish()
            .unwrap();
        assert_eq!(residual, "");
        assert_eq!(taken, vec![1.0, 2.0, 3.0, 4.0]);

        let (residual, taken) = super::take_n::<f32>(2)
            .parse(r#"1.0 2.0 3.0 4.0"#)
            .finish()
            .unwrap();
        assert_eq!(residual, " 3.0 4.0");
        assert_eq!(taken, vec![1.0, 2.0]);
    }

    #[test]
    fn take_3n() {
        let (residual, taken) = super::take_3n::<f32>(2)
            .parse(
                r#"
                1.0 2.0 3.0
                4.0 5.0 6.0
                "#
                .trim(),
            )
            .finish()
            .unwrap();
        assert_eq!(residual, "");
        assert_eq!(taken, vec![[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]);

        let (residual, taken) = super::take_3n::<f32>(2)
            .parse(
                r#"
                1.0 2.0 3.0 4.0 5.0 6.0
                "#
                .trim(),
            )
            .finish()
            .unwrap();
        assert_eq!(residual, "");
        assert_eq!(taken, vec![[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]);

        assert!(super::take_3n::<f32>(2)
            .parse(
                r#"
                1.0 2.0 3.0 4.0 5.0
                "#
                .trim(),
            )
            .finish()
            .is_err());
    }

    #[test]
    fn take_n_m() {
        let (residual, taken) = super::take_n_m::<f32>(2, 3)
            .parse(
                r#"
                1.0 2.0 3.0 4.0 5.0 6.0
                "#
                .trim(),
            )
            .finish()
            .unwrap();
        assert_eq!(residual, "");
        assert_eq!(taken, vec![[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]);
    }

    #[test]
    fn data3() {
        let (residual, out) = super::data3(DataType::Float, 2)
            .parse(
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

        let (residual, out) = super::data3(DataType::UnsignedInt, 2)
            .parse(
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
