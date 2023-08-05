#[derive(Debug)]
pub struct Error {
    // Not dead, used in the debug impl.
    #[allow(dead_code)]
    message: &'static str,
}

pub type Result<T> = std::result::Result<T, Error>;

impl Error {
    // TODO: Add a better error type which records source span info etc.
    pub fn new(message: &'static str) -> Error {
        Error { message }
    }
}

impl From<&'static str> for Error {
    fn from(message: &'static str) -> Error {
        Error { message }
    }
}
