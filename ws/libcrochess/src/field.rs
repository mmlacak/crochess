// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.

use std::ops as so;
use std::fmt;

use crate::step as s;


#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Field(pub i32, pub i32);


impl so::Add<s::Step> for Field {
    type Output = Field;

    fn add(self, step: s::Step) -> Field {
        return Field( self.0 + step.0, self.1 + step.1 );
    }
}

impl so::AddAssign<s::Step> for Field {
    fn add_assign(&mut self, step: s::Step) {
        self.0 += step.0;
        self.1 += step.1;
    }
}

impl so::Sub<s::Step> for Field {
    type Output = Field;

    fn sub(self, step: s::Step) -> Field {
        return Field( self.0 - step.0, self.1 - step.1 );
    }
}

impl so::SubAssign<s::Step> for Field {
    fn sub_assign(&mut self, step: s::Step) {
        self.0 -= step.0;
        self.1 -= step.1;
    }
}


impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "({}, {})", self.0, self.1);
    }
}
