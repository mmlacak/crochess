// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use std::ops as so;
use std::fmt;


#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct RelStep(pub i32, pub i32);


impl so::Add for RelStep {
    type Output = RelStep;

    fn add(self, other: RelStep) -> RelStep {
        return RelStep( self.0 + other.0, self.1 + other.1 );
    }
}

impl so::AddAssign for RelStep {
    fn add_assign(&mut self, other: RelStep) {
        self.0 += other.0;
        self.1 += other.1;
    }
}

impl so::Sub for RelStep {
    type Output = RelStep;

    fn sub(self, other: RelStep) -> RelStep {
        return RelStep( self.0 - other.0, self.1 - other.1 );
    }
}

impl so::SubAssign for RelStep {
    fn sub_assign(&mut self, other: RelStep) {
        self.0 -= other.0;
        self.1 -= other.1;
    }
}

impl so::Mul<i32> for RelStep {
    type Output = RelStep;

    fn mul(self, factor: i32) -> RelStep {
        return RelStep( self.0 * factor, self.1 * factor );
    }
}

impl so::MulAssign<i32> for RelStep {
    fn mul_assign(&mut self, factor: i32) {
        self.0 *= factor;
        self.1 *= factor;
    }
}


// TODO :: Div, Rem ... (?)


impl fmt::Display for RelStep {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "<{}, {}>", self.0, self.1);
    }
}
