// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use std::ops as so;
use std::fmt;


#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Step(pub i32, pub i32);


impl so::Add for Step {
    type Output = Step;

    fn add(self, other: Step) -> Step {
        return Step( self.0 + other.0, self.1 + other.1 );
    }
}

impl so::AddAssign for Step {
    fn add_assign(&mut self, other: Step) {
        self.0 += other.0;
        self.1 += other.1;
    }
}

impl so::Sub for Step {
    type Output = Step;

    fn sub(self, other: Step) -> Step {
        return Step( self.0 - other.0, self.1 - other.1 );
    }
}

impl so::SubAssign for Step {
    fn sub_assign(&mut self, other: Step) {
        self.0 -= other.0;
        self.1 -= other.1;
    }
}

impl so::Mul<i32> for Step {
    type Output = Step;

    fn mul(self, factor: i32) -> Step {
        return Step( self.0 * factor, self.1 * factor );
    }
}

impl so::MulAssign<i32> for Step {
    fn mul_assign(&mut self, factor: i32) {
        self.0 *= factor;
        self.1 *= factor;
    }
}


// TODO :: Div, Rem ... (?)


impl fmt::Display for Step {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "<{}, {}>", self.0, self.1);
    }
}
