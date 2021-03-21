// Copyright (c) 2021 Mario Mlačak, mmlacak@gmail.com
// Licensed under 3-clause (modified) BSD license. See LICENSE for details.


pub trait AsciiPrintable {
    fn is_ascii_printable(self) -> bool;
}


impl AsciiPrintable for char {
    fn is_ascii_printable(self) -> bool {
        if !( self.is_ascii_alphanumeric() ||
              self.is_ascii_punctuation() ||
              self.is_ascii_graphic() ) {
                  return false;
        }
        return true;
    }
}

impl AsciiPrintable for u8 {
    fn is_ascii_printable(self) -> bool {
        if !( self.is_ascii_alphanumeric() ||
              self.is_ascii_punctuation() ||
              self.is_ascii_graphic() ) {
                  return false;
        }
        return true;
    }
}

impl AsciiPrintable for &str {
    fn is_ascii_printable(self) -> bool {
        for c in self.chars() {
            if !c.is_ascii_printable() {
                return false;
            }
        }
        return true;
    }
}
