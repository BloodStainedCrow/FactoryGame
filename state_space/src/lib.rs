use std::ops::{Range, RangeInclusive};

trait StateSpace {
    type Value: Clone;

    fn num_dimensions(&self) -> usize;

    fn min(&self, dimension: usize) -> Self::Value;
    fn max(&self, dimension: usize) -> Self::Value;

    fn average(&self, lower: &Self::Value, higher: &Self::Value) -> Self::Value;
}

impl<T: Clone + Avg> StateSpace for RangeInclusive<T> {
    type Value = T;
    fn num_dimensions(&self) -> usize {
        1
    }

    fn min(&self, _dimension: usize) -> Self::Value {
        self.start().clone()
    }

    fn max(&self, _dimension: usize) -> Self::Value {
        self.end().clone()
    }

    fn average(&self, lower: &Self::Value, higher: &Self::Value) -> Self::Value {
        T::average(lower, higher)
    }
}

impl<A: StateSpace, B: StateSpace> StateSpace for (A, B) {
    type Value = (A::Value, B::Value);

    fn num_dimensions(&self) -> usize {
        self.0.num_dimensions() + self.1.num_dimensions()
    }

    fn min(&self, dimension: usize) -> Self::Value {
        // (self.0.min(), self.1.min())
        todo!()
    }

    fn max(&self, dimension: usize) -> Self::Value {
        // (self.0.max(), self.1.max())
        todo!()
    }

    fn average(&self, lower: &Self::Value, higher: &Self::Value) -> Self::Value {
        (
            self.0.average(&lower.0, &higher.0),
            self.1.average(&lower.1, &higher.1),
        )
    }
}

trait Avg {
    fn average(lower: &Self, higher: &Self) -> Self;
}

macro_rules! avg {
    ($ty: ty) => {
        impl Avg for $ty {
            fn average(lower: &Self, higher: &Self) -> Self {
                *lower + (*higher - *lower) / 2
            }
        }
    };
}
macro_rules! avgf {
    ($ty: ty) => {
        impl Avg for $ty {
            fn average(lower: &Self, higher: &Self) -> Self {
                *lower + (*higher - *lower) / 2.0
            }
        }
    };
}

avg!(u8);
avg!(u16);
avg!(u32);
avg!(u64);
avg!(u128);
avg!(i8);
avg!(i16);
avg!(i32);
avg!(i64);
avg!(i128);
avgf!(f32);
avgf!(f64);
