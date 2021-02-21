
pub mod piece_type;

// fn dbg(x: &impl std::fmt::Debug) {
//     println!("{:?}", &x);
// }

// fn dbgv(x: &impl std::fmt::Debug) {
//     println!("{:#?}", x);
// }

pub fn dbg<T: std::fmt::Debug>(x: &T) {
    println!("{:?}", &x);
}

pub fn dbgv<T: std::fmt::Debug>(x: &T) {
    println!("{:#?}", x);
}

pub fn dbg_<T: std::fmt::Debug>(x: &T) {
    print!("{:?}", &x);
}



#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
